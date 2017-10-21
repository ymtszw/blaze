const fs = require('fs')
const ini = require('ini')

// Enables XMLHttpRequest (required by elm-lang/http) in node environment
global.XMLHttpRequest = require('xhr2').XMLHttpRequest

const home = process.env.HOME

if (!home) {
  throw new Error('Home directory cannot be found in $HOME env var.')
}

// PAAPI Credentials must be set in Shared Credential INI File (~/.aws/credentials)
// for running user (both local and cloud) as '[paapi]' profile.
// You should set non-standard property 'associate_tag' too.
const paapiCredentials = ini.decode(fs.readFileSync(`${home}/.aws/credentials`, 'utf-8')).paapi

var Elm = require('./Igniter.elm')

var igniteWorker = Elm.Igniter.worker({
  accessKeyId: paapiCredentials.aws_access_key_id,
  secretAccessKey: paapiCredentials.aws_secret_access_key,
  associateTag: paapiCredentials.associate_tag,
})

// Ports

igniteWorker.ports.sendModelDump.subscribe(model => {
  console.log(model)
})

// Body of the script

igniteWorker.ports.ignite.send('Fire!')
