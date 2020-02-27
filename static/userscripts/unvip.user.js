// ==UserScript==
// @name        UnVIP
// @author      Anon
// @namespace   http://textboard.org
// @description View the thread list in update order regardless of VIPs
// @version     1
// @match       *://textboard.org/*
// @grant       none
// ==/UserScript==
(function() {
  'use strict';
  const tbody = document.getElementsByTagName ("tbody") [0]
  const rows = Array.from (tbody.children)
  const upd = e => e.children [3].children [0].textContent
  rows.sort ((a, b) => {var u = upd (a); var v = upd (b); return u < v ? 1 : u > v ? -1 : 0; })
  tbody.innerHTML = ""
  rows.forEach (e => tbody.appendChild (e))
})();
