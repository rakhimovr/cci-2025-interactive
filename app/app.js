// =============================================================================
// CCI 2025 Explorer - Main Application
// Orchestrates the chat interface, Claude API, and chart rendering
// =============================================================================

(function() {
  // ---- State ----
  var cciData = null;
  var conversationHistory = [];
  var isLoading = false;

  // ---- DOM Elements ----
  var chatMessages = document.getElementById('chat-messages');
  var userInput = document.getElementById('user-input');
  var sendBtn = document.getElementById('send-btn');
  var apiModal = document.getElementById('api-modal');
  var apiKeyInput = document.getElementById('api-key-input');
  var saveKeyBtn = document.getElementById('save-key-btn');
  var settingsBtn = document.getElementById('settings-btn');
  var suggestions = document.getElementById('suggestions');

  // ---- Initialize ----
  function init() {
    // Load CCI data via XMLHttpRequest (works with file:// protocol)
    var xhr = new XMLHttpRequest();
    xhr.open('GET', 'data/cci-data.json', true);
    xhr.onreadystatechange = function() {
      if (xhr.readyState === 4) {
        if (xhr.status === 200 || xhr.status === 0) { // status 0 for file://
          try {
            cciData = JSON.parse(xhr.responseText);
            console.log('CCI data loaded successfully:', Object.keys(cciData));
          } catch (e) {
            showError('Failed to parse CCI data: ' + e.message);
            return;
          }
        } else {
          showError('Failed to load CCI data (HTTP ' + xhr.status + '). Please ensure data/cci-data.json exists.');
          return;
        }

        // Check for API key
        var apiKey = localStorage.getItem('cci-api-key');
        if (!apiKey) {
          apiModal.classList.add('active');
        }

        // Set up event listeners
        setupEventListeners();
        console.log('CCI Explorer initialized.');
      }
    };
    xhr.send();
  }

  function setupEventListeners() {
    // Send message
    sendBtn.addEventListener('click', handleSend);
    userInput.addEventListener('keydown', function(e) {
      if (e.key === 'Enter' && !e.shiftKey) {
        e.preventDefault();
        handleSend();
      }
    });

    // Auto-resize textarea
    userInput.addEventListener('input', function() {
      userInput.style.height = 'auto';
      userInput.style.height = Math.min(userInput.scrollHeight, 120) + 'px';
      sendBtn.disabled = !userInput.value.trim();
    });

    // API key modal
    saveKeyBtn.addEventListener('click', saveApiKey);
    apiKeyInput.addEventListener('keydown', function(e) {
      if (e.key === 'Enter') saveApiKey();
    });

    // Settings
    settingsBtn.addEventListener('click', function() {
      apiKeyInput.value = localStorage.getItem('cci-api-key') || '';
      apiModal.classList.add('active');
    });

    // Suggestion chips
    var chips = document.querySelectorAll('.suggestion-chip');
    for (var i = 0; i < chips.length; i++) {
      chips[i].addEventListener('click', function() {
        var question = this.getAttribute('data-question');
        userInput.value = question;
        sendBtn.disabled = false;
        handleSend();
      });
    }
  }

  function saveApiKey() {
    var key = apiKeyInput.value.trim();
    if (!key) return;
    localStorage.setItem('cci-api-key', key);
    apiModal.classList.remove('active');
    apiKeyInput.value = '';
  }

  // ---- Message Handling ----
  function handleSend() {
    var text = userInput.value.trim();
    if (!text || isLoading) return;

    var apiKey = localStorage.getItem('cci-api-key');
    if (!apiKey) {
      apiModal.classList.add('active');
      return;
    }

    if (!cciData) {
      showError('CCI data not loaded yet. Please refresh the page.');
      return;
    }

    // Hide suggestions after first message
    if (suggestions) {
      suggestions.style.display = 'none';
    }

    // Add user message to UI
    addMessage(text, 'user');
    userInput.value = '';
    userInput.style.height = 'auto';
    sendBtn.disabled = true;

    // Add to conversation history
    conversationHistory.push({ role: 'user', content: text });

    // Show typing indicator
    var typingEl = showTyping();
    isLoading = true;

    CCIClient.sendMessage(apiKey, cciData, conversationHistory)
      .then(function(response) {
        // Add to conversation history
        conversationHistory.push({ role: 'assistant', content: response });

        // Remove typing indicator and show response
        typingEl.remove();
        addAssistantMessage(response);
        isLoading = false;
      })
      .catch(function(error) {
        typingEl.remove();
        showError(error.message);

        // Remove the failed user message from history
        conversationHistory.pop();
        isLoading = false;
      });
  }

  function addMessage(text, role) {
    var messageDiv = document.createElement('div');
    messageDiv.className = 'message ' + role + '-message';

    var avatar = document.createElement('div');
    avatar.className = 'message-avatar';
    avatar.textContent = role === 'user' ? 'You' : 'CCI';

    var body = document.createElement('div');
    body.className = 'message-body';

    var textDiv = document.createElement('div');
    textDiv.className = 'message-text';
    textDiv.textContent = text;

    body.appendChild(textDiv);
    messageDiv.appendChild(avatar);
    messageDiv.appendChild(body);
    chatMessages.appendChild(messageDiv);
    scrollToBottom();
  }

  function addAssistantMessage(responseText) {
    var segments = CCICharts.parseResponse(responseText);

    var messageDiv = document.createElement('div');
    messageDiv.className = 'message assistant-message';

    var avatar = document.createElement('div');
    avatar.className = 'message-avatar';
    avatar.textContent = 'CCI';

    var body = document.createElement('div');
    body.className = 'message-body';

    segments.forEach(function(segment) {
      if (segment.type === 'text') {
        var textDiv = document.createElement('div');
        textDiv.className = 'message-text';
        textDiv.innerHTML = CCICharts.renderMarkdown(segment.content);
        body.appendChild(textDiv);
      } else if (segment.type === 'chart') {
        var chartEl = CCICharts.renderChart(segment.content);
        body.appendChild(chartEl);
      }
    });

    messageDiv.appendChild(avatar);
    messageDiv.appendChild(body);
    chatMessages.appendChild(messageDiv);
    scrollToBottom();
  }

  function showTyping() {
    var messageDiv = document.createElement('div');
    messageDiv.className = 'message assistant-message';

    var avatar = document.createElement('div');
    avatar.className = 'message-avatar';
    avatar.textContent = 'CCI';

    var body = document.createElement('div');
    body.className = 'message-body';

    var typing = document.createElement('div');
    typing.className = 'typing-indicator';
    typing.innerHTML = '<div class="typing-dot"></div><div class="typing-dot"></div><div class="typing-dot"></div>';

    body.appendChild(typing);
    messageDiv.appendChild(avatar);
    messageDiv.appendChild(body);
    chatMessages.appendChild(messageDiv);
    scrollToBottom();

    return messageDiv;
  }

  function showError(message) {
    var messageDiv = document.createElement('div');
    messageDiv.className = 'message assistant-message error-message';

    var avatar = document.createElement('div');
    avatar.className = 'message-avatar';
    avatar.textContent = 'CCI';

    var body = document.createElement('div');
    body.className = 'message-body';

    var textDiv = document.createElement('div');
    textDiv.className = 'message-text';
    textDiv.textContent = message;

    body.appendChild(textDiv);
    messageDiv.appendChild(avatar);
    messageDiv.appendChild(body);
    chatMessages.appendChild(messageDiv);
    scrollToBottom();
  }

  function scrollToBottom() {
    requestAnimationFrame(function() {
      chatMessages.scrollTop = chatMessages.scrollHeight;
    });
  }

  // ---- Boot ----
  init();
})();
