const WebSocket = require('ws')
const EventEmitter = require('events')

const wss = new WebSocket.Server({ host: '0.0.0.0', port: 9998 })
console.log('Server listening on port 9998')

const events = new EventEmitter()

var islands = []
var posts = []

wss.on('connection', function connection (ws) {
  // Send list of exising islands posts
  islands.map((island) => {
    ws.send(JSON.stringify({type: 'NewIsland', island: island}))
  })

  posts.map((post) => {
    ws.send(JSON.stringify({type: 'NewPost', post: post}))
  })

  // Hook up to events
  const onNewPost = function (post) {
    ws.send(JSON.stringify({type: 'NewPost', post: post}))
  }

  const onNewIsland = function (island) {
    ws.send(JSON.stringify({type: 'NewIsland', island: island}))
  }

  events.on('NewPost', onNewPost)
  events.on('NewIsland', onNewIsland)

  // remove disconnected client from events
  ws.on('close', function () {
    events.removeListener('NewPost', onNewPost)
    events.removeListener('NewIsland', onNewIsland)
  })

  ws.on('error', function () {
    events.removeListener('NewPost', onNewPost)
    events.removeListener('NewIsland', onNewIsland)
  })

  // Handle messages from client
  ws.on('message', function incoming (raw) {
    const msg = JSON.parse(raw)
    console.log(msg)

    switch (msg.type) {
      case 'NewIsland':
        events.emit('NewIsland', msg.island)
        islands.push(msg.island)
        break

      case 'NewPost':
        events.emit('NewPost', msg.post)
        posts.push(msg.post)
        break

      case 'Clear':
        islands = []
        posts = []
        break
    }
  })
})
