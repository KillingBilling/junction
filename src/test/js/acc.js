'use strict';

var sum = {
  aggr: function(a, b) {return a + b;},
  init: function(v) {return 0;}
};
var prod = {
  aggr: function(a, b) {return a * b;},
  init: function(v) {return 1;}
};

exports.aggregates = {sum: sum, prod: prod};
