const WebSocket = require('ws')
const EventEmitter = require('events')

const wss = new WebSocket.Server({ port: 9998 })
console.log('Server listening on port 9998')

const events = new EventEmitter()

wss.on('connection', function connection (ws) {
  // Send some stuff to client
  ws.send('All currently existing islands')

  // Hook up to events
  const eventListener = function (someEvent) {
    // send something to client on event
    ws.send('Some event happened')
  }
  events.on('someEvent', eventListener)

  // remove disconnected client from events
  ws.on('close', function () {
    events.removeListener('someEvent', eventListener)
  })

  // Handle messages from client
  ws.on('message', function incoming (raw) {
    const msg = JSON.parse(raw)

    switch (msg.type) {
      case 'NewIsland':
        // do something
        break

      case 'NewPost':
        // do something
        break
    }
  })
})
