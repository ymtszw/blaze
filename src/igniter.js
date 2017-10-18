const AWS = require('aws-sdk')

// PAAPI Credentials must be set in Shared Credential File (~/.aws/credentials)
// for running user (both local and cloud) as 'paapi' profile.
const paapiCredentials = new AWS.SharedIniFileCredentials({profile: 'paapi'});

var IgniterElm = require('./Igniter.elm')

var app = IgniterElm.Igniter.worker({
  accessKeyId: paapiCredentials.accessKeyId,
  secretAccessKey: paapiCredentials.secretAccessKey,
  associateTag: 'blaze0c-22', // Hardcoded but decoupled from ID/Secret so no problem
})

app.ports.sendModelDump.subscribe(model => {
  console.log(model)
});

// Body of the script

app.ports.requestModelDump.send(null);
