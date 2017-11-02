const Elm = require('./Worker.elm')

const worker = Elm.Worker.worker()

worker.ports.start.send('Hello World!')

process.on('SIGINT', () => {
  worker.ports.interrupt.send(null)
})
