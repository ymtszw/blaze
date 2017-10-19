var Elm = require('./Worker.elm')

var worker = Elm.Worker.worker()

worker.ports.start.send('Hello World!')

process.on('SIGINT', () => {
  worker.ports.interrupt.send(null)
})
