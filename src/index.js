const Elm = require('./Main.elm')

function enterFullscreen (element) {
  if (element.requestFullscreen) {
    element.requestFullscreen()
  } else if (element.mozRequestFullScreen) {
    element.mozRequestFullScreen()
  } else if (element.msRequestFullscreen) {
    element.msRequestFullscreen()
  } else if (element.webkitRequestFullscreen) {
    element.webkitRequestFullscreen()
  }
}

document.getElementById('start-button').addEventListener('click', () => {
  document.getElementById('pre-page').style.display = 'none'
  enterFullscreen(document.documentElement)

  document.getElementById('audio').play()
  Elm.Main.fullscreen()
})
