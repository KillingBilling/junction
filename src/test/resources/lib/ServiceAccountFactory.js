'use strict';

var Aggregate = Java.type("org.killingbilling.junction.ModuleSpec.Aggregate");
var ServiceAccount = Java.type("org.killingbilling.junction.ModuleSpec.ServiceAccount");
var ServiceAccountFactory = Java.type("org.killingbilling.junction.ModuleSpec.ServiceAccountFactory");

module.exports = new ServiceAccountFactory(function(acc) {
  return new ServiceAccount(function() {
    return {sum: new Aggregate(acc.aggregates.sum), prod: new Aggregate(acc.aggregates.prod)};
  });
});
