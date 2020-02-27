// ==UserScript==
// @name        Syntax Highlighting for textboard.org
// @namespace   http://textboard.org/
// @description Syntax Highlighting of code blocks with highlight.js
// @version     1
// @match       *://textboard.org/*
// @resource    css https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css
// @require     https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js
// @require     https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/haskell.min.js
// @require     https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/lisp.min.js
// @require     https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/scheme.min.js
// @require     https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/javascript.min.js
// @require     https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/lua.min.js
// @require     https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/languages/go.min.js
// @grant       GM_addStyle
// @grant       GM_getResourceText
// ==/UserScript==
GM_addStyle(GM_getResourceText('css'));
(function () {
  'use strict';
  hljs.initHighlighting();
})();
