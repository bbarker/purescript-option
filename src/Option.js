
"use strict";

exports.pickFn = function(ks, r) {
  var copy = {};
  for(var i = 0; i < ks.length; i++) {
    if (typeof r[ks[i]] !== 'undefined') {
      copy[ks[i]] = r[ks[i]];
    }
  }
  return copy;
};
