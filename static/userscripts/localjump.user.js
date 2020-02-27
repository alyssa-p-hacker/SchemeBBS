// ==UserScript==
// @name        localjump
// @author      Anon
// @namespace   http://textboard.org
// @description Change single post quote links inside full thread views from separate single post views to local scroll jumps within the thread
// @version     1
// @match       *://textboard.org/*
// @grant       none
// ==/UserScript==
(function() {
  'use strict';
  Array.from (document.getElementsByTagName ("a")).filter (e => e.hasAttribute ("href")).forEach (e => {
  var h = e.getAttribute ("href")
  var s = h.replace (/^\/([^\/]+)\/(\d+)\/(\d+)$/, "/$1/$2#t$2p$3")
  if (h != s) { e.setAttribute ("href", s); }
})
})();
