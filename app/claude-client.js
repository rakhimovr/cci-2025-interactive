// =============================================================================
// CCI Explorer - Claude API Client
// Handles communication with the Anthropic API (client-side, browser-direct)
// Uses compact data format to minimize token usage
// =============================================================================

var CCIClient = (function() {
  var API_URL = 'https://api.anthropic.com/v1/messages';
  var MODEL = 'claude-haiku-4-5-20251001';
  var MAX_TOKENS = 1500;
  var MAX_RETRIES = 2;
  var RETRY_DELAY_MS = 5000;

  // Compact data serializer: converts nested JSON to a dense text table
  // This cuts token usage by ~80% vs raw JSON
  function compactData(cciData) {
    var lines = [];

    // Helper: serialize one behavior's indicators as a compact line
    function behaviorLine(beh, data) {
      if (!data) return '';
      var parts = [];
      Object.keys(data).forEach(function(ind) {
        var d = data[ind];
        if (d && d.mean !== undefined) {
          parts.push(ind + '=' + d.mean + '(' + d.ci_lower + '-' + d.ci_upper + ')');
        }
      });
      return beh + ': ' + parts.join(', ');
    }

    // Overall
    lines.push('=== OVERALL (n=' + cciData.metadata.sample_size + ') ===');
    Object.keys(cciData.overall).forEach(function(beh) {
      lines.push(behaviorLine(beh, cciData.overall[beh]));
    });

    // Demographic breakdowns
    var breakdowns = [
      ['by_age', 'AGE'], ['by_gender', 'GENDER'], ['by_race', 'RACE'],
      ['by_party', 'PARTY'], ['by_segment', 'SASSY SEGMENT'],
      ['by_education', 'EDUCATION'], ['by_income', 'INCOME']
    ];

    breakdowns.forEach(function(bd) {
      var key = bd[0], label = bd[1];
      var section = cciData[key];
      if (!section || Object.keys(section).length === 0) return;
      lines.push('');
      lines.push('=== BY ' + label + ' ===');
      Object.keys(section).forEach(function(group) {
        lines.push('--- ' + group + ' ---');
        Object.keys(section[group]).forEach(function(beh) {
          lines.push(behaviorLine(beh, section[group][beh]));
        });
      });
    });

    // Trends
    if (cciData.trends && Object.keys(cciData.trends).length > 0) {
      lines.push('');
      lines.push('=== TRENDS 2024→2025 ===');
      Object.keys(cciData.trends).forEach(function(beh) {
        var parts = [];
        Object.keys(cciData.trends[beh]).forEach(function(ind) {
          var t = cciData.trends[beh][ind];
          parts.push(ind + ': ' + t.mean_2024 + '→' + t.mean_2025 +
            ' (diff=' + t.difference + ', sig=' + (t.significant ? 'YES' : 'no') + ')');
        });
        lines.push(beh + ': ' + parts.join('; '));
      });
    }

    return lines.join('\n');
  }

  function buildSystemPrompt(cciData) {
    var meta = cciData.metadata;
    var compactStr = compactData(cciData);

    return 'You are the Climate Culture Index (CCI) Explorer by Rare. You answer questions about US public attitudes toward climate-mitigating behaviors.\n\n' +
      'Survey: ' + meta.survey_date + ', n=' + meta.sample_size + ' US adults, weighted via ' + meta.weighting_method + '.\n\n' +
      'Behaviors: ' + Object.entries(meta.behaviors).map(function(e) { return e[1] + ' (' + e[0] + ')'; }).join(', ') + '\n\n' +
      'Indicators (14 per behavior): ' + Object.entries(meta.indicators).map(function(e) { return e[1] + ' [' + e[0] + ', ' + meta.indicator_scales[e[0]] + ']'; }).join('; ') + '\n\n' +
      'Policies: ' + Object.entries(meta.policy_descriptions).map(function(e) { return meta.behaviors[e[0]] + ': ' + e[1]; }).join('; ') + '\n\n' +
      'Demographics available: ' + Object.entries(meta.demographic_dimensions).map(function(e) { return e[0] + ' (' + e[1].join(', ') + ')'; }).join('; ') + '\n\n' +
      '## DATA\nFormat: indicator=mean(ci_lower-ci_upper). All values survey-weighted.\n\n' +
      compactStr + '\n\n' +
      '## RESPONSE RULES\n' +
      '1. Cite specific numbers. Use format: X.X% (95% CI: X.X%-X.X%).\n' +
      '2. When comparing 3+ values, include a chart block:\n' +
      '```chart\n{"type":"bar","title":"Title","labels":["A","B"],"datasets":[{"label":"Name","data":[1,2],"colors":["#hex1","#hex2"]}]}\n```\n' +
      'Types: bar, horizontalBar, line. Behavior colors: ' + JSON.stringify(meta.behavior_colors) + '\n' +
      '3. Stay in scope. Only CCI data.\n' +
      '4. For trends, note if change is statistically significant (sig=YES means non-overlapping CIs).\n' +
      '5. Use markdown: **bold** key numbers, bullet lists for comparisons.';
  }

  async function sendMessage(apiKey, cciData, conversationHistory) {
    var systemPrompt = buildSystemPrompt(cciData);
    var attempt = 0;

    while (attempt <= MAX_RETRIES) {
      var response = await fetch(API_URL, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'x-api-key': apiKey,
          'anthropic-version': '2023-06-01',
          'anthropic-dangerous-direct-browser-access': 'true'
        },
        body: JSON.stringify({
          model: MODEL,
          max_tokens: MAX_TOKENS,
          temperature: 0,
          system: systemPrompt,
          messages: conversationHistory
        })
      });

      if (response.ok) {
        var data = await response.json();
        return data.content[0].text;
      }

      if (response.status === 429 && attempt < MAX_RETRIES) {
        // Rate limited — wait and retry
        attempt++;
        var waitMs = RETRY_DELAY_MS * attempt; // 5s, 10s
        console.log('Rate limited. Retrying in ' + (waitMs / 1000) + 's (attempt ' + attempt + '/' + MAX_RETRIES + ')...');
        await new Promise(function(resolve) { setTimeout(resolve, waitMs); });
        continue;
      }

      // Non-retryable error or retries exhausted
      var errorBody = await response.text();
      if (response.status === 401) {
        throw new Error('Invalid API key. Please check your Anthropic API key in settings.');
      }
      if (response.status === 429) {
        throw new Error('Rate limit exceeded. Your API key may have a low usage tier. Please wait 30-60 seconds between questions, or upgrade your Anthropic plan at console.anthropic.com.');
      }
      throw new Error('API error (' + response.status + '): ' + errorBody);
    }
  }

  return { sendMessage: sendMessage };
})();
