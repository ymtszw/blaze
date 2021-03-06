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

const Elm = require('./Igniter.elm')

let knownPublishers = []

try {
  knownPublishers = fs.readFileSync('known_publishers', 'utf-8').split('\n')
} catch (err) {
  if (err.code !== 'ENOENT') {
    console.error(err)
    throw err
  }
}

const startIndexOfAdditionalArgs = 2

const worker = Elm.Igniter.worker({
  paapiCredentials: {
    accessKeyId: paapiCredentials.aws_access_key_id,
    secretAccessKey: paapiCredentials.aws_secret_access_key,
  },
  associateTag: paapiCredentials.associate_tag,
  knownPublishers,
  argv: process.argv.slice(startIndexOfAdditionalArgs),
})

// Ports

worker.ports.writeFile.subscribe(([filename, contents]) => {
  fs.writeFile(filename, contents, err => {
    if (err) {
      console.error(`Cannot write to ${filename}`)
      console.error(contents)
      throw err
    } else {
      console.log(`Written to ${filename}`)
    }
  })
})
