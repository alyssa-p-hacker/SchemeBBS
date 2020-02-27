// ==UserScript==
// @name        Word Filter for textboard.org
// @namespace   http://textboard.org
// @description Replace words with other words
// @version     1
// @match       *://textboard.org/*
// @grant       none
// ==/UserScript==
(function() {
  'use strict';
  var replacements, regex, key, textnodes, node, s;
  replacements = {
    "nigger": "mujina",
    "faggot": "baku",
  };
  regex = {};
  for (key in replacements) {
    regex[key] = new RegExp(key, 'gi');
  }
  textnodes = document.evaluate( "//body//text()", document, null, XPathResult.UNORDERED_NODE_SNAPSHOT_TYPE, null);
  for (var i = 0; i < textnodes.snapshotLength; i++) {
    node = textnodes.snapshotItem(i);
    s = node.data;
    for (key in replacements) {
      s = s.replace(regex[key], replacements[key]);
    }
    node.data = s;
  }
})();
