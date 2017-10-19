const AWS = require('aws-sdk')

// PAAPI Credentials must be set in Shared Credential File (~/.aws/credentials)
// for running user (both local and cloud) as 'paapi' profile.
const paapiCredentials = new AWS.SharedIniFileCredentials({
  profile: 'paapi',
})

var Elm = require('./Igniter.elm')

var igniteWorker = Elm.Igniter.worker({
  accessKeyId: paapiCredentials.accessKeyId,
  secretAccessKey: paapiCredentials.secretAccessKey,
  associateTag: 'paradoxica019-22', // Hardcoded but decoupled from ID/Secret so no problem
})

// Ports

igniteWorker.ports.sendModelDump.subscribe(model => {
  console.log(model)
})

// Body of the script

igniteWorker.ports.ignite.send('Fire!')
