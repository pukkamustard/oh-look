const WebSocket = require('ws')
const EventEmitter = require('events')

const wss = new WebSocket.Server({ port: 9998 })
console.log('Server listening on port 9998')

const events = new EventEmitter()

// var posts = {}

wss.on('connection', function connection (ws) {
  // Send some stuff to client
  ws.send('All currently existing islands')

  // Hook up to events
  const eventListener = function (post) {
    // send something to client on event
    ws.send(JSON.stringify({type: 'NewPost', post: post}))
  }
  events.on('new-post', eventListener)

  // remove disconnected client from events
  ws.on('close', function () {
    events.removeListener('new-post', eventListener)
  })

  ws.on('error', function () {
    events.removeListener('new-post', eventListener)
  })

  // Handle messages from client
  ws.on('message', function incoming (raw) {
    const msg = JSON.parse(raw)
    console.log(msg)

    switch (msg.type) {
      case 'NewIsland':
        // do something
        break

      case 'NewPost':
        // do something
        events.emit('new-post', msg.post)
        break
    }
  })
})
