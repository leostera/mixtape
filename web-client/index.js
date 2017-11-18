// Polyfills
require('fetch')
const WebSocket = window.WebSocket

// Dependencies
const R = require('ramda')
const Type = require('./type')

const log = (...x) => {
  console.log(...x)
  return R.last(x)
}
log('')

const parseQuery = R.compose(
  R.fromPairs,
  R.map(R.split('=')),
  R.split('&'),
  R.drop(1)
)

const querify = R.compose(
  R.join('&'),
  R.map(R.join('=')),
  R.map(([k, v]) => [k, encodeURIComponent(v)]),
  R.toPairs
)

/*******************************************************************************
 *
 * Lenses
 *
 ******************************************************************************/

const $rootElementId = R.lensPath(['dom', 'root'])
const accessGrantData = R.lensPath(['user', 'authData'])
const accessToken = R.lensPath(['user', 'authData', 'access_token'])
const currentLocation = R.lensPath(['location'])
const currentOffsetMs = R.lensPath(['playback', 'offsetMs'])
const currentPath = R.lensPath(['location', 'pathname'])
const currentPlayback = R.lensPath(['playback', 'current'])
const currentPlaylist = R.lensPath(['playlist', 'current'])
const currentPlaylistId = R.lensPath(['playlist', 'uri'])
const currentPlaylistName = R.lensPath(['playlist', 'current', 'name'])
const currentPlaylistTracks = R.lensPath(['playlist', 'current', 'tracks', 'items'])
const currentPosition = R.lensPath(['playback', 'position'])
const currentUser = R.lensPath(['user', 'current'])
const currentUserFullName = R.lensPath(['user', 'current', 'display_name'])
const currentUserAvatar = R.lensPath(['user', 'current', 'images', 0, 'url'])
const currentUserId = R.lensPath(['user', 'current', 'id'])
const currentTrackName = R.lensPath(['playback', 'current', 'item', 'name'])
const currentTrackArtistName = R.lensPath(['playback', 'current', 'item', 'artists', 0, 'name'])
const currentTrackAlbumArt = R.lensPath(['playback', 'current', 'item', 'album', 'images', 0, 'url'])
const errors = R.lensPath(['errors'])
const eventValue = R.lensPath(['target', 'value'])
const locationCount = R.lensPath(['app', 'locationCount'])
const locationHash = R.lensPath(['location', 'hash'])
const loginUrl = R.lensPath(['app', 'loginUrl'])
const message = R.lensPath(['app', 'ws', 'message']) // outgoing
const messages = R.lensPath(['app', 'ws', 'sentMessages']) // outgoing
const playlistUri = R.lensPath(['playlist', 'uri'])
const receivedMessages = R.lensPath(['app', 'ws', 'receivedMessages']) // incoming
const requestAccess = R.lensPath(['user', 'requestAccess'])
const requestConnect = R.lensPath(['app', 'ws', 'requestConnect'])
const requestDisconnect = R.lensPath(['app', 'ws', 'requestDisconnect'])
const requestMessage = R.lensPath(['app', 'ws', 'requestMessage'])
const requestPause = R.lensPath(['playback', 'requestPause'])
const requestPlay = R.lensPath(['playback', 'requestPlay'])
const requestPlaylist = R.lensPath(['playlist', 'requestPlaylist'])
const requestUser = R.lensPath(['user', 'requestUser'])
const responseOffsetMs = R.lensPath(['progress_ms'])
const styleBgColor = R.lensPath(['dom', 'styles', 'colors', 'bg'])
const styleDimColor = R.lensPath(['dom', 'styles', 'colors', 'dim'])
const styleFgColor = R.lensPath(['dom', 'styles', 'colors', 'fg'])
const styleLogoColor = R.lensPath(['dom', 'styles', 'colors', 'logo'])
const styleSpotifyGreen = R.lensPath(['dom', 'styles', 'colors', 'spotifyGreen'])
const syncContext = R.lensPath(['context'])
const syncContextOffset = R.lensPath(['offset_ms'])
const syncContextPosition = R.lensPath(['position'])
const syncInfo = R.lensPath(['info'])
const syncResponseItemUri = R.lensPath(['item', 'uri'])
const trackUri = R.lensPath(['track', 'uri'])
const wsOptions = R.lensPath(['app', 'ws', 'options'])
const wsSocket = R.lensPath(['app', 'ws', 'socket'])
const wsUrl = R.lensPath(['app', 'ws', 'url'])

const currentPlaylistUris = R.compose(
  R.map(R.view(trackUri)),
  R.view(currentPlaylistTracks)
)

/*******************************************************************************
 *
 * Types
 *
 ******************************************************************************/

const EffectPayload = Type({
  typeName: 'EffectPayload',
  constructors: [
    { name: 'Request', arity: 1 },
    { name: 'Response', arity: 1 }
  ]
})

const Result = Type({
  typeName: 'Result',
  constructors: [
    { name: 'Ok', arity: 1 },
    { name: 'Err', arity: 1 }
  ]
})

const Actions = Type({
  typeName: 'Actions',
  constructors: [
    { name: 'Bootstrap', arity: 0 },
    { name: 'Location', arity: 1 },
    { name: 'Playlist', arity: 1 },
    { name: 'Playback', arity: 1 },
    { name: 'Session', arity: 1 },
    { name: 'Unknown', arity: 0 },
    { name: 'User', arity: 1 },
    { name: 'WS', arity: 1 }
  ]
})

const Session = Type({
  typeName: 'Session',
  constructors: [
    { name: 'GrantAccess', arity: 1 },
    { name: 'RequestAccess', arity: 0 }
  ]
})

const UserAction = Type({
  typeName: 'UserAction',
  constructors: [
    { name: 'FetchProfile', arity: 1 }
  ]
})

const PlaylistAction = Type({
  typeName: 'PlaylistAction',
  constructors: [
    { name: 'Fetch', arity: 1 }
  ]
})

const PlaybackAction = Type({
  typeName: 'PlaybackAction',
  constructors: [
    { name: 'Sync', arity: 1 },
    { name: 'Bootstrap', arity: 1 }
  ]
})

const WebSocketAction = Type({
  typeName: 'WebSocketAction',
  constructors: [
    { name: 'Connect', arity: 1 },
    { name: 'Disconnect', arity: 1 },
    { name: 'Receive', arity: 1 },
    { name: 'Send', arity: 1 }
  ]
})

/*******************************************************************************
 *
 * Reducers
 *
 ******************************************************************************/

const host = window.location.hostname

const config = {
  client_id: `a84bae1d337b4862bd5203bc507005f6`,
  redirect_uri: `http://${host}:8000/auth/spotify/success`,
  scope: [
    'streaming',
    'user-modify-playback-state',
    'user-read-email',
    'user-read-playback-state',
    'user-read-private'
  ].join(' ')
}

const initialState = {
  dom: {
    root: 'root',
    styles: {
      colors: {
        bg: '#000',
        dim: '#616467',
        fg: '#f8f8f8',
        logo: '#f037a5',
        spotifyGreen: '#1db954'
      },
      padding: '4rem'
    }
  },
  playback: {
    requestPause: false,
    requestPlay: false
  },
  playlist: false,
  user: false,
  location: false,
  app: {
    ws: {
      url: `ws://${host}:2112`,
      opts: []
    },
    volume: 0.7,
    config,
    locationCount: 0,
    loginUrl: `https://accounts.spotify.com/authorize?${querify(config)}&response_type=token`
  }
}

const registerMessage = s => ({
  action: 'register',
  playlist_id: R.view(currentPlaylistId, s),
  user_id: R.view(currentUserId, s)
})

const shouldRegisterInSyncService = state => {
  const hasUser = !R.isNil(R.view(currentUser, state))
  const hasPlaylist = !R.isNil(R.view(currentPlaylist, state))
  return hasUser && hasPlaylist
}

const registerInSyncService = state => shouldRegisterInSyncService(state)
  ? R.compose(
    R.set(requestMessage, true),
    R.set(message, registerMessage(state))
  )(state)
  : state

const findPositionInPlaylist = (status, state) => {
  const itemUri = R.view(syncResponseItemUri, status)
  const index = R.findIndex(R.equals(itemUri), currentPlaylistUris(state)) + 1
  return (index === 0) ? 1 : index
}

const buildUpdateMessage = state => ({
  action: 'playback_update',
  offset_ms: R.view(currentOffsetMs, state),
  position: R.view(currentPosition, state),
  user_id: R.view(currentUserId, state),
  playlist_id: R.view(playlistUri, state)
})

const userReducer = UserAction.match({
  FetchProfile: EffectPayload.match({
    Request: () => R.set(requestUser, true),
    Response: Result.match({
      Ok: user => R.compose(
        registerInSyncService,
        R.set(requestUser, false),
        R.set(currentUser, user)
      ),
      Err: error => R.compose(
        R.set(requestUser, false),
        R.set(errors, error)
      )
    })
  })
})

const playlistReducer = PlaylistAction.match({
  Fetch: EffectPayload.match({
    Request: uri => state => {
      if (validPlaylistUri(uri)) {
        return R.compose(
          R.set(requestPlaylist, true),
          R.set(playlistUri, uri)
        )(state)
      }
      return state
    },
    Response: Result.match({
      Ok: p => R.compose(
        registerInSyncService,
        R.set(requestPlaylist, false),
        R.set(currentPlaylist, p)
      ),
      Err: error => R.compose(
        R.set(requestPlaylist, false),
        R.set(errors, error)
      )
    })
  })
})

const playbackReducer = PlaybackAction.match({
  Sync: status => R.compose(
    R.set(requestMessage, true),
    R.set(currentPlayback, status),
    R.set(currentOffsetMs, R.view(responseOffsetMs, status) || 0),
    s => R.set(message, buildUpdateMessage(s), s),
    s => R.set(currentPosition, findPositionInPlaylist(status, s), s)
  ),
  Bootstrap: EffectPayload.match({
    Request: ctx => R.compose(
      R.set(requestPlay, true),
      R.set(currentOffsetMs, R.view(syncContextOffset, ctx)),
      R.set(currentPosition, R.view(syncContextPosition, ctx))
    ),
    Response: Result.match({
      Ok: () => R.compose(
        R.set(requestPlay, false),
        R.set(requestPause, false)
      ),
      Err: error => R.compose(
        R.set(requestPlay, false),
        R.set(requestPause, false),
        R.set(errors, error)
      )
    })
  })
})

const wsReducer = WebSocketAction.match({
  Connect: EffectPayload.match({
    Request: () => R.set(requestConnect, true),
    Response: Result.match({
      Ok: socket => R.compose(
        R.set(requestConnect, false),
        R.set(wsSocket, socket)
      ),
      Err: error => R.compose(
        R.set(requestConnect, false),
        R.set(errors, error)
      )
    })
  }),
  Disconnect: EffectPayload.match({
    Request: () => R.set(requestDisconnect, true),
    Response: Result.match({
      Ok: () => R.compose(
        R.set(requestDisconnect, false),
        R.set(wsSocket, false)
      ),
      Err: error => R.compose(
        R.set(requestDisconnect, false),
        R.set(errors, error)
      )
    })
  }),
  Send: EffectPayload.match({
    Request: msg => R.compose(
      R.set(requestMessage, true),
      R.set(message, msg)
    ),
    Response: Result.match({
      Ok: msg => R.compose(
        R.set(requestMessage, false),
        R.over(messages, x => R.isNil(x) ? [msg] : [msg, ...x])
      ),
      Err: error => R.compose(
        R.set(requestMessage, false),
        R.set(errors, error)
      )
    })
  }),
  Receive: data => R.compose(
    R.over(receivedMessages, x => R.isNil(x) ? [data] : [data, ...x])
  )
})

const reducer = Actions.match({
  Unknown: () => state => state,
  Bootstrap: () => () => initialState,
  Location: l => R.compose(
    R.over(locationCount, x => x + 1),
    R.set(currentLocation, l)
  ),
  Playlist: playlistReducer,
  User: userReducer,
  Session: Session.match({
    RequestAccess: () => R.set(requestAccess, true),
    GrantAccess: data => R.compose(
      R.set(requestConnect, true),
      R.set(requestUser, true),
      R.set(requestAccess, false),
      R.set(accessGrantData, data)
    )
  }),
  Playback: playbackReducer,
  WS: wsReducer
})

/*******************************************************************************
 *
 * Effects
 *
 ******************************************************************************/

const grantAccess = R.compose(
  Actions.Session,
  Session.GrantAccess,
  R.view(locationHash)
)

const routingTable = [
  { path: '/', f: () => Actions.Unknown() },
  { path: '/auth/spotify/success', f: grantAccess }
]

const router = routingTable => next => {
  let lastLocationCount = 0
  return state => {
    const currentLocationCount = R.view(locationCount, state)
    if (lastLocationCount === currentLocationCount) return

    else lastLocationCount = currentLocationCount
    const path = R.view(currentPath, state)
    const match = R.find(route => route.path === path, routingTable)
    if (match) {
      log(`Router matched`, path)
      next(match.f(state))
    }
  }
}

const _fetch = ({body, method, shouldRequest, endpoint, handle}) => next => {
  let _run = false
  return state => {
    if (!_run && shouldRequest(state)) {
      _run = true
      const token = R.view(accessToken, state)

      const requestOpts = {
        method,
        headers: {
          'Authorization': `Bearer ${token}`
        },
        credentials: 'same-origin'
      }

      if ((method === 'POST' || method === 'PUT') && body) {
        requestOpts.body = JSON.stringify(body(state))
      }

      const handleResponse = response => R.compose(
        next,
        handle,
        data => {
          const httpError = response.status >= 400
          const applicationError = data.error ? (data.error.status >= 400) : false
          if (httpError || applicationError) return Result.Err(data)
          return Result.Ok(data)
        }
      )

      window.fetch(`https://api.spotify.com/${endpoint(state)}`, requestOpts)
        .then(response => {
          if (response.status === 204) next(handle(Result.Ok({})))
          else {
            response
              .json()
              .then(handleResponse(response), handleResponse(response))
          }
        })
        .then(() => { _run = false })
        .catch(() => { _run = false })
    }
  }
}

const fetchProfile = _fetch({
  method: 'GET',
  shouldRequest: R.view(requestUser),
  endpoint: () => 'v1/me',
  handle: Result.match({
    Ok: R.compose(
      Actions.User,
      UserAction.FetchProfile,
      EffectPayload.Response,
      Result.Ok
    ),
    Err: R.compose(
      Actions.User,
      UserAction.FetchProfile,
      EffectPayload.Response,
      Result.Err
    )
  })
})

const matchAlphanumerical = R.match(/^[0-9a-zA-Z]$/g)

// validBase62 -> 5WZyjENJIepWYuz1Vnfuma -> true
const validBase62 = R.compose(
  R.all(R.equals(true)),
  R.map(R.flip(R.gt)(0)),
  R.map(R.length),
  R.map(matchAlphanumerical),
  R.split('')
)

// Valid Playlist Uri -> spotify:user:leostera:playlist:5WZyjENJIepWYuz1Vnfuma
const validPlaylistUri = R.compose(
  R.all(R.equals(true)),
  R.over(R.lensIndex(4), validBase62),
  R.over(R.lensIndex(3), R.equals('playlist')),
  R.over(R.lensIndex(2), x => R.length(x) > 0),
  R.over(R.lensIndex(1), R.equals('user')),
  R.over(R.lensIndex(0), R.equals('spotify')),
  R.split(':')
)

// URI: spotify:user:leostera:playlist:5WZyjENJIepWYuz1Vnfuma
// URL: v1/users/{user_id}/playlists/{playlist_id}
const _playlistUriToUrl = R.compose(
  R.join('/'),
  R.over(R.lensIndex(3), x => `${x}s`),
  R.over(R.lensIndex(1), x => `${x}s`),
  R.over(R.lensIndex(0), x => `v1`),
  R.split(':')
)

const fetchPlaylist = _fetch({
  method: 'GET',
  shouldRequest: R.view(requestPlaylist),
  endpoint: R.compose(
    _playlistUriToUrl,
    R.view(playlistUri)
  ),
  handle: Result.match({
    Ok: R.compose(
      Actions.Playlist,
      PlaylistAction.Fetch,
      EffectPayload.Response,
      Result.Ok
    ),
    Err: R.compose(
      Actions.Playlist,
      PlaylistAction.Fetch,
      EffectPayload.Response,
      Result.Err
    )
  })
})

const playbackCurrentStatus = next => {
  let _state = false

  const fetcher = _fetch({
    method: 'GET',
    shouldRequest: shouldRegisterInSyncService,
    endpoint: () => 'v1/me/player/currently-playing',
    handle: Result.match({
      Ok: R.compose(
        Actions.Playback,
        PlaybackAction.Sync
      ),
      Err: () => Actions.Unknown()
    })
  })(next)

  /* eslint-disable */
  const fetchInterval = setInterval(() => {
    if (_state) fetcher(_state)
  }, 750)
  /* eslint-enable */

  return state => { _state = state }
}

const _playback = ({body, method, lens, name, query, actionType}) => _fetch({
  body: body || false,
  method,
  shouldRequest: R.view(lens),
  endpoint: state => `v1/me/player/${name}${query ? query(state) : ''}`,
  handle: R.compose(
    Actions.Playback,
    actionType,
    EffectPayload.Response
  )
})

const promisify = effect => state =>
  new Promise((resolve, reject) => effect(resolve)(state))

const _play = promisify(_playback({
  body: state => ({
    uris: currentPlaylistUris(state),
    offset: {
      position: R.view(currentPosition, state) - 1
    }
  }),
  method: 'PUT',
  lens: requestPlay,
  name: 'play',
  actionType: PlaybackAction.Bootstrap
}))

const _seek = promisify(_playback({
  method: 'PUT',
  lens: requestPlay,
  name: 'seek',
  query: R.compose(
    ms => `?position_ms=${ms}`,
    R.view(currentOffsetMs)
  ),
  actionType: PlaybackAction.Bootstrap
}))

const _pause = promisify(_playback({
  method: 'PUT',
  lens: requestPause,
  name: 'pause',
  actionType: PlaybackAction.Bootstrap
}))

const playbackSync = next => {
  let isRunning = false
  const stop = () => { isRunning = false }

  return state => {
    if (isRunning) return

    const shouldPlay = R.view(requestPlay, state)
    if (shouldPlay) {
      isRunning = true
      // seriously needs do-notation
      _play(state).then(
        playResult => _seek(state).then(
          seekResult => playResult))
        .then(next)
        .then(stop)
    }
    const shouldPause = R.view(requestPause, state)
    if (shouldPause) {
      isRunning = true
      _pause(state).then(next).then(stop)
    }
  }
}

const authenticate = next => state => {
  const shouldBeginFlow = R.view(requestAccess, state)
  if (shouldBeginFlow) {
    // since this will cause a redirection out and back into the application
    // we can't represent this as an action -- any state change will be lost
    // unless we persist it in local storage with another side-effect
    window.location = R.view(loginUrl, state)
  }
}

const createHistory = require('history').createBrowserHistory
const locationEffect = next => {
  const history = createHistory()
  const cleanLocation = R.evolve({ hash: parseQuery })
  const dispatchLocation = R.compose(next, Actions.Location, cleanLocation)
  history.listen((location, action) => dispatchLocation(location))
  return state => {
    if (R.equals(false, R.view(currentLocation, state))) {
      dispatchLocation(history.location)
    }
  }
}

const websocket = next => {
  let _ws = false
  let keepAlive = false

  const connect = state => next => {
    const url = R.view(wsUrl, state)
    const opts = R.view(wsOptions, state)
    _ws = new WebSocket(url, opts)

    _ws.onclose = e =>
      next(Actions.WS(WebSocketAction.Disconnect(EffectPayload.Request(true))))

    _ws.onmessage = e => {
      const event = R.evolve({ data: JSON.parse }, e)
      if (event.data === 'pong') return
      next(Actions.WS(WebSocketAction.Receive(event.data)))
    }

    _ws.onopen = e => {
      next(Actions.WS(WebSocketAction.Connect(EffectPayload.Response(Result.Ok(_ws)))))
    }

    keepAlive = setInterval(() => _ws.send(JSON.stringify('ping')), 1000)
  }

  return state => {
    if (!_ws && R.view(requestConnect, state) === true) {
      connect(state)(next)
    }

    if (_ws && R.view(requestDisconnect, state) === true) {
      _ws.close()
      _ws = false
      if (keepAlive) clearInterval(keepAlive)
    }

    const isMessageRequested = R.view(requestMessage, state) === true
    const isSocketOpen = _ws && _ws.readyState === WebSocket.OPEN
    const shouldSend = isSocketOpen && isMessageRequested
    if (shouldSend) {
      const m = R.view(message, state)
      _ws.send(JSON.stringify(m))
      next(Actions.WS(WebSocketAction.Send(EffectPayload.Response(Result.Ok(m)))))
    }
  }
}

const mapWebSocketMessageToAction = next => msg => {
  if (R.view(syncInfo, msg) === 'bootstrap') {
    return R.compose(
      next,
      Actions.Playback,
      PlaybackAction.Bootstrap,
      EffectPayload.Request,
      R.view(syncContext)
    )(msg)
  }
}

const socketReceive = pushMessage => next => {
  let lastMessageCount = 0
  return state => {
    const msgs = R.view(receivedMessages, state) || []
    const messageCount = R.length(msgs)

    if (messageCount > lastMessageCount) {
      const newMsgCount = messageCount - lastMessageCount
      lastMessageCount = messageCount

      const newMsgs = R.take(newMsgCount, msgs)
      R.forEach(pushMessage(next), newMsgs)
    }
  }
}

const DOM = require('preact')
const createElement = DOM.createElement
const logo = require('./logo')

const $ = tagOrF => props => (...children) =>
  R.length(children) === 0
    ? createElement(tagOrF, props)
    : createElement(tagOrF, props, children)

const render = next => {
  let _lastRender
  return state => {
    /***************************************************************************
     *
     * General Components
     *
     **************************************************************************/

    const wrapper = $('section')({
      style: {
        backgroundColor: R.view(styleBgColor, state),
        color: R.view(styleFgColor, state),
        fontFamily: 'Bariol',
        height: '100vh',
        left: 0,
        position: 'fixed',
        top: 0,
        width: '100vw'
      }
    })

    const container = $('Container')

    const centeredContainer = container({
      style: {
        display: 'block',
        position: 'absolute',
        textAlign: 'center',
        top: '50%',
        transform: 'translateY(-50%)',
        width: '100vw'
      }
    })

    const content = container({
      style: {
        display: 'block',
        textAlign: 'center',
        width: '100vw'
      }
    })

    const mainContent = container({
      style: {
        display: 'block',
        textAlign: 'center',
        width: '100vw',
        marginTop: '9rem'
      }
    })

    const text = $('Text')
    const button = $('button')
    const input = $('input')

    const img = $('img')

    const bold = text({
      style: {
        fontWeight: 600
      }
    })

    /***************************************************************************
     *
     * App Specific Components
     *
     **************************************************************************/

    const dot = size => $('Dot')({
      style: {
        color: R.view(styleSpotifyGreen, state),
        fontSize: size,
        lineHeight: 0,
        margin: 0,
        marginLeft: '-0.2rem',
        padding: 0
      }
    })('.')

    const bigLogo = color => logo({
      style: {
        display: 'inline-block',
        width: '10rem',
        fill: R.view(color, state)
      }
    })

    const smallLogo = container({})(
      logo({
        style: {
          display: 'inline-block',
          fill: R.view(styleFgColor, state),
          left: 0,
          margin: '1.5rem',
          position: 'absolute',
          top: 0,
          width: '3rem'
        }
      }),
      text({
        style: {
          fontSize: '1.5rem',
          left: 0,
          margin: '2.2rem 5rem',
          position: 'absolute',
          top: 0
        }
      })('mixtape', dot('4rem')))

    const brand = (...text) => $('Brand')({
      style: {
        display: 'block',
        fontFamily: 'Bariol',
        fontSize: '3rem',
        marginBottom: '1rem',
        marginTop: '1rem'
      }
    })(...[...text, dot('10rem')])

    const subtitle = $('Subtitle')({
      style: {
        display: 'block',
        fontFamily: 'Bariol',
        fontSize: '1.2rem'
      }
    })

    /***************************************************************************
     *
     * View Data Projections
     *
     **************************************************************************/

    const isLoggedIn = !R.isNil(R.view(currentUser, state))
    const isLoggingIn = R.view(requestUser, state)

    const username = R.compose(
      state => {
        const id = R.view(currentUserId, state)
        if (id) {
          const numericId = R.not(R.any(R.equals(NaN), R.map(Number, R.split('', id))))
          if (numericId) return R.head(R.split(' ', R.view(currentUserFullName, state)))
          return R.view(currentUserId, state)
        } else {
          return 'Mr. Spock'
        }
      }
    )(state)
    const artistName = R.view(currentTrackArtistName, state)
    const trackName = R.view(currentTrackName, state)
    const playlistName = R.view(currentPlaylistName, state)

    /***************************************************************************
     *
     * Elements, and Data-filled/fueled stuffs
     *
     **************************************************************************/

    const loading = brand('Loading', dot('10rem'), dot('10rem'))

    const mainTitle = brand('mixtape')
    const tagLine = subtitle('Music is better together')

    const loginButton = button({
      style: {
        backgroundColor: R.view(styleSpotifyGreen, state),
        border: 0,
        borderRadius: '16px',
        color: R.view(styleFgColor, state),
        cursor: 'pointer',
        marginTop: '3rem',
        padding: '0.5rem 1rem',
        textTransform: 'uppercase'
      },
      onClick: R.compose(
        next,
        Actions.Session,
        x => Session.RequestAccess()
      )
    })('Connect with Spotify')

    const playlistInput = input({
      style: {
        backgroundColor: R.view(styleBgColor, state),
        border: 0,
        color: R.view(styleFgColor, state),
        display: 'block',
        fontFamily: 'Bariol',
        fontSize: '2rem',
        margin: '2rem 0',
        outline: 'none',
        padding: '1rem 10vw',
        textAlign: 'center',
        width: '80vw'
      },
      placeholder: 'Paste here a Spotify Playlist URI to begin playing',
      onChange: R.compose(
        next,
        Actions.Playlist,
        PlaylistAction.Fetch,
        EffectPayload.Request,
        R.view(eventValue)
      )
    })()

    const avatar =
      img({
        src: R.view(currentUserAvatar, state),
        style: {
          borderRadius: '1rem',
          float: 'right',
          margin: '0.25rem',
          width: '2rem'
        }
      })()

    const account =
      container({
        style: {
          position: 'absolute',
          right: 0,
          top: 0,
          margin: '2rem'
        }
      })(
        avatar,
        text({
          style: {
            float: 'left',
            margin: '0.7rem'
          }
        })(`Welcome, ${username}`))

    const albumArt =
      img({
        src: R.view(currentTrackAlbumArt, state),
        style: {
          display: 'block',
          margin: '0 auto',
          padding: '2rem 0',
          width: '100%',
          maxWidth: '480px'
        }
      })()

    const hasAllData = R.all(
      R.equals(false),
      R.map(R.isNil, [
        trackName,
        artistName,
        playlistName
      ]))

    const nowPlaying =
      container({})(
        subtitle(
          bold(trackName),
          ` by `,
          bold(artistName)),
        text({})(`Playing from ${playlistName}`))

    const playback = hasAllData
      ? content(albumArt, nowPlaying)
      : loading

    /***************************************************************************
     *
     * Screens -- composed elements that make up "sections"
     *
     **************************************************************************/

    const mainScreen =
      content(
        account,
        smallLogo,
        mainContent(
          playlistInput,
          playlistName ? playback : ''))

    const loginScreen =
      centeredContainer(
        content(
          bigLogo(styleLogoColor),
          mainTitle,
          tagLine),
        content(
          loginButton))

    const loadingScreen =
      centeredContainer(
        content(
          bigLogo(styleDimColor),
          loading))

    const view = isLoggedIn
      ? mainScreen
      : (isLoggingIn
        ? loadingScreen
        : loginScreen)

    /***************************************************************************
     *
     * Render!
     *
     **************************************************************************/

    const $root = R.view($rootElementId, state)
    const $target = document.getElementById($root)
    _lastRender = DOM.render(wrapper(view), $target, _lastRender)
  }
}

/*******************************************************************************
 *
 * Glue code
 *
 ******************************************************************************/

const effects = [
  authenticate,
  fetchPlaylist,
  fetchProfile,
  locationEffect,
  playbackCurrentStatus,
  playbackSync,
  render,
  router(routingTable),
  socketReceive(mapWebSocketMessageToAction),
  websocket
]

const subscribeEffects = store => R.compose(
  R.forEach(store.subscribe),
  R.map(e => () => e(store.getState())),
  R.map(e => e(store.dispatch))
)

const redux = require('redux')
const createStore = r => a => redux.createStore((state, action) => {
  if (action && action.inspect) log(action.inspect())
  if (R.isNil(action)) return r(a)(state)
  if (action.type === '@@redux/INIT') return r(a)(state)
  if (action.type === '@@INIT') return r(a)(state)
  return r(action)(state)
},
  window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__({ maxAge: 4000 })
)

const store = createStore(reducer)(Actions.Unknown())
subscribeEffects(store)(effects)
store.dispatch(Actions.Bootstrap())

/*******************************************************************************
 *
 * Exposed crap to play around in console
 *
 ******************************************************************************/

window.next = store.dispatch

window.login = Actions.Session(Session.RequestAccess())
window.playlistUri = Actions.Playlist(PlaylistAction.Fetch(EffectPayload.Request('spotify:user:leostera:playlist:5WZyjENJIepWYuz1Vnfuma')))

window.wsOpen = Actions.WS(WebSocketAction.Connect(EffectPayload.Request(true)))
window.wsClose = Actions.WS(WebSocketAction.Disconnect(EffectPayload.Request(true)))
window.wsSend = R.compose(
  Actions.WS,
  WebSocketAction.Send,
  EffectPayload.Request
)
