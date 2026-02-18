// =============================================================================
// CCI Explorer - Chart Renderer
// Parses ```chart blocks from Claude responses and renders Chart.js charts
// =============================================================================

var CCICharts = (function() {
  var DEFAULT_COLORS = [
    "#005BBB", "#F58233", "#008542", "#5E6A71",
    "#7f5d90", "#AA1948", "#884600", "#2196F3",
    "#FF5722", "#4CAF50", "#9C27B0", "#FF9800"
  ];

  function parseResponse(responseText) {
    var segments = [];
    var chartRegex = /```chart\s*\n([\s\S]*?)```/g;
    var lastIndex = 0;
    var match;

    while ((match = chartRegex.exec(responseText)) !== null) {
      if (match.index > lastIndex) {
        var textBefore = responseText.slice(lastIndex, match.index).trim();
        if (textBefore) segments.push({ type: 'text', content: textBefore });
      }
      try {
        var chartSpec = JSON.parse(match[1]);
        segments.push({ type: 'chart', content: chartSpec });
      } catch (e) {
        segments.push({ type: 'text', content: match[0] });
      }
      lastIndex = match.index + match[0].length;
    }

    if (lastIndex < responseText.length) {
      var remaining = responseText.slice(lastIndex).trim();
      if (remaining) segments.push({ type: 'text', content: remaining });
    }

    if (segments.length === 0) {
      segments.push({ type: 'text', content: responseText });
    }

    return segments;
  }

  function renderChart(spec) {
    var container = document.createElement('div');
    container.className = 'chart-container';

    var canvas = document.createElement('canvas');
    container.appendChild(canvas);

    var chartType = spec.type === 'horizontalBar' ? 'bar' : (spec.type || 'bar');
    var isHorizontal = spec.type === 'horizontalBar';

    var datasets = (spec.datasets || []).map(function(ds, i) {
      var colors = ds.colors || ds.data.map(function(_, j) { return DEFAULT_COLORS[j % DEFAULT_COLORS.length]; });
      var bgColors = Array.isArray(colors) ? colors : [colors];

      return {
        label: ds.label || ('Dataset ' + (i + 1)),
        data: ds.data,
        backgroundColor: bgColors.length === 1 ? bgColors[0] : bgColors,
        borderColor: bgColors.length === 1 ? bgColors[0] : bgColors,
        borderWidth: 1,
        borderRadius: 4,
        maxBarThickness: 60
      };
    });

    var config = {
      type: chartType,
      data: {
        labels: spec.labels || [],
        datasets: datasets
      },
      options: {
        responsive: true,
        maintainAspectRatio: true,
        indexAxis: isHorizontal ? 'y' : 'x',
        plugins: {
          title: {
            display: !!spec.title,
            text: spec.title || '',
            font: { size: 14, weight: '600' },
            color: '#333',
            padding: { bottom: 16 }
          },
          legend: {
            display: datasets.length > 1,
            position: 'top',
            labels: {
              font: { size: 12 },
              padding: 12,
              usePointStyle: true,
              pointStyle: 'rectRounded'
            }
          },
          tooltip: {
            backgroundColor: 'rgba(0,0,0,0.8)',
            titleFont: { size: 13 },
            bodyFont: { size: 12 },
            padding: 10,
            cornerRadius: 6,
            callbacks: {
              label: function(ctx) {
                var val = ctx.parsed.y !== undefined ?
                  (isHorizontal ? ctx.parsed.x : ctx.parsed.y) : ctx.parsed;
                return ctx.dataset.label + ': ' + (typeof val === 'number' ? val.toFixed(1) : val);
              }
            }
          }
        },
        scales: {
          x: {
            grid: { display: !isHorizontal, color: '#f0f0f0' },
            ticks: { font: { size: 11 }, maxRotation: 45, autoSkip: false }
          },
          y: {
            grid: { display: isHorizontal ? false : true, color: '#f0f0f0' },
            beginAtZero: true,
            ticks: { font: { size: 11 } }
          }
        },
        animation: { duration: 600, easing: 'easeOutQuart' }
      }
    };

    if (chartType === 'line') {
      datasets.forEach(function(ds) {
        ds.fill = false;
        ds.tension = 0.3;
        ds.pointRadius = 5;
        ds.pointHoverRadius = 7;
        ds.borderWidth = 2;
      });
    }

    new Chart(canvas, config);
    return container;
  }

  function renderMarkdown(text) {
    if (typeof marked !== 'undefined' && marked.parse) {
      return marked.parse(text);
    }
    return text
      .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
      .replace(/\*(.*?)\*/g, '<em>$1</em>')
      .replace(/`(.*?)`/g, '<code>$1</code>')
      .replace(/\n\n/g, '</p><p>')
      .replace(/^/, '<p>')
      .replace(/$/, '</p>');
  }

  return {
    parseResponse: parseResponse,
    renderChart: renderChart,
    renderMarkdown: renderMarkdown
  };
})();
