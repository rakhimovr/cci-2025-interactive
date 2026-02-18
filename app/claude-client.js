// =============================================================================
// CCI Explorer - Claude API Client
// Handles communication with the Anthropic API (client-side, browser-direct)
// =============================================================================

var CCIClient = (function() {
  var API_URL = 'https://api.anthropic.com/v1/messages';
  var MODEL = 'claude-sonnet-4-20250514';
  var MAX_TOKENS = 2048;

  function buildSystemPrompt(cciData) {
    var meta = cciData.metadata;
    var dataJson = JSON.stringify({
      overall: cciData.overall,
      by_age: cciData.by_age,
      by_gender: cciData.by_gender,
      by_race: cciData.by_race,
      by_party: cciData.by_party,
      by_segment: cciData.by_segment,
      by_education: cciData.by_education,
      by_income: cciData.by_income,
      trends: cciData.trends
    }, null, 0);

    return 'You are the Climate Culture Index (CCI) Explorer, an assistant that answers questions about US public attitudes toward climate-mitigating behaviors. You are developed by Rare.\n\n' +
      '## Survey Overview\n' +
      '- **Title:** ' + meta.title + '\n' +
      '- **Organization:** ' + meta.organization + '\n' +
      '- **Survey Date:** ' + meta.survey_date + '\n' +
      '- **Sample:** ' + meta.sample_description + ' (n = ' + meta.sample_size + ')\n' +
      '- **Weighting:** ' + meta.weighting_method + '\n' +
      '- **Years Available:** ' + meta.years_available.join(', ') + '\n\n' +
      '## Seven Tracked Behaviors\n' +
      Object.entries(meta.behaviors).map(function(e) { return '- **' + e[1] + '** (code: ' + e[0] + ')'; }).join('\n') + '\n\n' +
      '## 14 Psychosocial Indicators\n' +
      Object.entries(meta.indicators).map(function(e) { return '- **' + e[1] + '** (' + e[0] + '): ' + meta.indicator_scales[e[0]]; }).join('\n') + '\n\n' +
      '## Policy Descriptions (one per behavior)\n' +
      Object.entries(meta.policy_descriptions).map(function(e) { return '- **' + meta.behaviors[e[0]] + ':** ' + e[1]; }).join('\n') + '\n\n' +
      '## Available Demographic Breakdowns\n' +
      Object.entries(meta.demographic_dimensions).map(function(e) { return '- **' + e[0] + ':** ' + e[1].join(', '); }).join('\n') + '\n\n' +
      '## Complete CCI Data\n' +
      'Below is the full pre-computed summary data. All values are survey-weighted. Each entry has: mean, ci_lower (95% CI lower bound), ci_upper (95% CI upper bound), and n (sample size). Demographic breakdowns may have a subset of indicators.\n\n' +
      dataJson + '\n\n' +
      '## How to Respond\n\n' +
      '1. **Be precise:** Always cite specific numbers from the data above. Round to one decimal place.\n' +
      '2. **Cite confidence intervals** when relevant: "X.X% (95% CI: X.X%\u2013X.X%)".\n' +
      '3. **Note sample sizes** when discussing subgroups.\n' +
      '4. **Include charts** when comparing values. Use this format for a chart block:\n\n' +
      '```chart\n' +
      '{\n' +
      '  "type": "bar",\n' +
      '  "title": "Chart Title",\n' +
      '  "labels": ["Label 1", "Label 2"],\n' +
      '  "datasets": [\n' +
      '    {\n' +
      '      "label": "Dataset Name",\n' +
      '      "data": [12.3, 45.6],\n' +
      '      "colors": ["#005BBB", "#F58233"]\n' +
      '    }\n' +
      '  ]\n' +
      '}\n' +
      '```\n\n' +
      'Chart types: "bar" (default for comparisons), "horizontalBar" (for many categories), "line" (for trends).\n' +
      'For behavior comparisons, use these colors: ' + JSON.stringify(meta.behavior_colors) + '\n\n' +
      '5. **Stay in scope:** Only answer questions about CCI data. If asked about something outside scope, explain what IS available.\n' +
      '6. **Statistical significance:** For trend comparisons, mention whether changes are statistically significant (non-overlapping 95% CIs). Be clear this is based on CI overlap, not a formal hypothesis test.\n' +
      '7. **Be conversational but scholarly.** Use plain language but maintain precision.\n' +
      '8. **Format with markdown.** Use **bold** for key numbers, bullet lists for multiple comparisons, and tables when appropriate.';
  }

  async function sendMessage(apiKey, cciData, conversationHistory) {
    var systemPrompt = buildSystemPrompt(cciData);

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

    if (!response.ok) {
      var errorBody = await response.text();
      if (response.status === 401) {
        throw new Error('Invalid API key. Please check your Anthropic API key in settings.');
      }
      if (response.status === 429) {
        throw new Error('Rate limit exceeded. Please wait a moment and try again.');
      }
      throw new Error('API error (' + response.status + '): ' + errorBody);
    }

    var data = await response.json();
    return data.content[0].text;
  }

  return { sendMessage: sendMessage };
})();
