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

const accessGrantData = R.lensPath(['user', 'authData'])
const accessToken = R.lensPath(['user', 'authData', 'access_token'])
const availableDevices = R.lensPath(['devices', 'available'])
const contextPosition = R.lensPath(['context', 'position'])
const currentDevice = R.lensPath(['devices', 'current'])
const currentLocation = R.lensPath(['location'])
const currentPath = R.lensPath(['location', 'pathname'])
const currentPlaylist = R.lensPath(['playlist', 'current'])
const currentPlaylistId = R.lensPath(['playlist', 'uri'])
const currentPlaylistTracks = R.lensPath(['playlist', 'current', 'tracks', 'items'])
const currentPosition = R.lensPath(['playback', 'position'])
const currentUser = R.lensPath(['user', 'current'])
const currentUserId = R.lensPath(['user', 'current', 'id'])
const errors = R.lensPath(['errors'])
const location = R.lensPath(['location'])
const locationCount = R.lensPath(['app', 'locationCount'])
const locationHash = R.lensPath(['location', 'hash'])
const loginUrl = R.lensPath(['app', 'loginUrl'])
const message = R.lensPath(['app', 'ws', 'message']) // outgoing
const messages = R.lensPath(['app', 'ws', 'sentMessages']) // outgoing
const playbackStatus = R.lensPath(['playback', 'status'])
const playlistUri = R.lensPath(['playlist', 'uri'])
const receivedMessages = R.lensPath(['app', 'ws', 'receivedMessages']) // incoming
const requestAccess = R.lensPath(['user', 'requestAccess'])
const requestConnect = R.lensPath(['app', 'ws', 'requestConnect'])
const requestDevices = R.lensPath(['devices', 'requestDevices'])
const requestDisconnect = R.lensPath(['app', 'ws', 'requestDisconnect'])
const requestMessage = R.lensPath(['app', 'ws', 'requestMessage'])
const requestNext = R.lensPath(['playback', 'requestNext'])
const currentOffsetMs = R.lensPath(['playback', 'offsetMs'])
const requestPause = R.lensPath(['playback', 'requestPause'])
const requestPlay = R.lensPath(['playback', 'requestPlay'])
const requestPlaylist = R.lensPath(['playlist', 'requestPlaylist'])
const requestPrevious = R.lensPath(['playback', 'requestPrevious'])
const requestUser = R.lensPath(['user', 'requestUser'])
const responseOffsetMs = R.lensPath(['progress_ms'])
const syncAction = R.lensPath(['action'])
const syncStatus = R.lensPath(['status'])
const syncInfo = R.lensPath(['info'])
const syncContext = R.lensPath(['context'])
const syncContextOffset = R.lensPath(['offset_ms'])
const syncContextPosition = R.lensPath(['position'])
const syncResponseItemUri = R.lensPath(['item', 'uri'])
const wsOptions = R.lensPath(['app', 'ws', 'options'])
const wsSocket = R.lensPath(['app', 'ws', 'socket'])
const trackUri = R.lensPath(['track', 'uri'])
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
    { name: 'FetchProfile', arity: 1 },
    { name: 'FetchDevices', arity: 1 }
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
    { name: 'Play', arity: 1 },
    { name: 'Pause', arity: 1 },
    { name: 'NextTrack', arity: 1 },
    { name: 'PreviousTrack', arity: 1 },
    { name: 'Sync', arity: 1 },
    { name: 'Bootstrap', arity: 1 }
  ]
})

const PlaybackStatus = Type({
  typeName: 'PlaybackStatus',
  constructors: [
    { name: 'Playing', arity: 0 },
    { name: 'Paused', arity: 0 }
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
  playback: {
    requestNext: false,
    requestPause: false,
    requestPlay: false,
    requestPrevious: false
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
  }),
  FetchDevices: EffectPayload.match({
    Request: () => R.set(requestDevices, true),
    Response: Result.match({
      Ok: devs => R.compose(
        s => (R.length(devs) === 1) ? R.set(currentDevice, devs[0], s) : s,
        R.set(requestDevices, false),
        R.set(availableDevices, devs)
      ),
      Err: error => R.compose(
        R.set(requestDevices, false),
        R.set(errors, error)
      )
    })
  })
})

const playlistReducer = PlaylistAction.match({
  Fetch: EffectPayload.match({
    Request: uri => R.compose(
      R.set(requestPlaylist, true),
      R.set(playlistUri, uri)
    ),
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
  Play: EffectPayload.match({
    Request: data => R.compose(
      R.set(currentOffsetMs, R.view(contextPosition, data)),
      R.set(currentPosition, R.view(contextPosition, data)),
      R.set(requestPlay, true)
    ),
    Response: Result.match({
      Ok: status => R.compose(
        R.set(requestPlay, false),
        R.set(playbackStatus, PlaybackStatus.Playing())
      ),
      Err: error => R.compose(
        R.set(requestPlay, false),
        R.set(errors, error)
      )
    })
  }),
  Pause: EffectPayload.match({
    Request: () => R.set(requestPause, true),
    Response: Result.match({
      Ok: status => R.compose(
        R.set(requestPause, false),
        R.set(playbackStatus, PlaybackStatus.Paused())
      ),
      Err: error => R.compose(
        R.set(requestPause, false),
        R.set(errors, error)
      )
    })
  }),
  NextTrack: EffectPayload.match({
    Request: () => R.set(requestNext, true),
    Response: Result.match({
      Ok: status => R.compose(
        R.set(requestNext, false),
        R.over(currentPosition, x => x ? x + 1 : 0)
      ),
      Err: error => R.compose(
        R.set(requestNext, false),
        R.set(errors, error)
      )
    })
  }),
  PreviousTrack: EffectPayload.match({
    Request: () => R.set(requestPrevious, true),
    Response: Result.match({
      Ok: status => R.compose(
        R.set(requestPrevious, false),
        R.over(currentPosition, x => x ? x + 1 : 0)
      ),
      Err: error => R.compose(
        R.set(requestPrevious, false),
        R.set(errors, error)
      )
    })
  }),
  Sync: status => R.compose(
    R.set(requestMessage, true),
    R.set(currentOffsetMs, R.view(responseOffsetMs, status)),
    s => R.set(message, buildUpdateMessage(s), s),
    s => R.set(currentPosition, findPositionInPlaylist(status, s), s)
  ),
  Bootstrap: ctx => R.compose(
    R.set(requestPlay, true),
    R.set(currentOffsetMs, R.view(syncContextOffset, ctx)),
    R.set(currentPosition, R.view(syncContextPosition, ctx))
  )
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
    R.set(location, l)
  ),
  Playlist: playlistReducer,
  User: userReducer,
  Session: Session.match({
    RequestAccess: () => R.set(requestAccess, true),
    GrantAccess: data => R.compose(
      R.set(requestConnect, true),
      R.set(requestDevices, true),
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
    if (match) next(match.f(state))
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

const fetchDevices = _fetch({
  method: 'GET',
  shouldRequest: R.view(requestDevices),
  endpoint: () => `v1/me/player/devices`,
  handle: Result.match({
    Ok: R.compose(
      Actions.User,
      UserAction.FetchDevices,
      EffectPayload.Response,
      Result.Ok,
      ({devices}) => devices
    ),
    Err: R.compose(
      Actions.User,
      UserAction.FetchDevices,
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
      Err: Actions.Unknown
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
  actionType: PlaybackAction.Play
}))

const _seek = promisify(_playback({
  method: 'PUT',
  lens: requestPlay,
  name: 'seek',
  query: R.compose(
    ms => `?position_ms=${ms}`,
    R.view(currentOffsetMs)
  ),
  actionType: PlaybackAction.Play
}))

const playbackPlay = next => state => {
  const shouldPlay = R.view(requestPlay, state)
  if (shouldPlay) {
    // seriously needs do-notation
    _play(state).then(
      playResult => _seek(state).then(
        seekResult => playResult))
    .then(next)
  }
}

const playbackPause = _playback({
  method: 'PUT',
  lens: requestPause,
  name: 'pause',
  actionType: PlaybackAction.Pause
})

const playbackNext = _playback({
  method: 'POST',
  lens: requestNext,
  name: 'next',
  actionType: PlaybackAction.NextTrack
})

const playbackPrevious = _playback({
  method: 'POST',
  lens: requestPrevious,
  name: 'previous',
  actionType: PlaybackAction.PreviousTrack
})

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

/* eslint-disable */
const debug = next => {
  let lastState = false
  return state => {
    log('Last State', lastState)
    log('Current State', state)
    lastState = state
  }
}
/* eslint-enable */

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

const socketReceive = next => {
  let lastMessageCount = 0

  const pickPlaybackAction = status => {
    switch (status) {
      case 'pause': return PlaybackAction.Pause
      case 'play': return PlaybackAction.Play
      case 'next': return PlaybackAction.NextTrack
      case 'previous': return PlaybackAction.PreviousTrack
    }
  }

  const pushAsAction = msg => {
    if (R.view(syncAction, msg) === 'sync') {
      const status = R.view(syncStatus, msg)
      return R.compose(
        next,
        Actions.Playback,
        pickPlaybackAction(status),
        EffectPayload.Request,
        ({data}) => data
      )(msg)
    }

    if (R.view(syncInfo, msg) === 'bootstrap') {
      return R.compose(
        next,
        Actions.Playback,
        PlaybackAction.Bootstrap,
        R.view(syncContext)
      )(msg)
    }
  }

  return state => {
    const msgs = R.view(receivedMessages, state) || []
    const messageCount = R.length(msgs)

    if (messageCount > lastMessageCount) {
      const newMsgCount = messageCount - lastMessageCount
      lastMessageCount = messageCount

      const newMsgs = R.take(newMsgCount, msgs)
      R.forEach(pushAsAction, newMsgs)
    }
  }
}

const render = next => state => {}

/*******************************************************************************
 *
 * Glue code
 *
 ******************************************************************************/

const effects = [
  authenticate,
  debug,
  fetchDevices,
  fetchPlaylist,
  fetchProfile,
  locationEffect,
  playbackCurrentStatus,
  playbackNext,
  playbackPause,
  playbackPlay,
  playbackPrevious,
  render,
  router(routingTable),
  socketReceive,
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
})

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

window.play = Actions.Playback(PlaybackAction.Play(EffectPayload.Request(false)))
window.pause = Actions.Playback(PlaybackAction.Pause(EffectPayload.Request(false)))
window.skip = Actions.Playback(PlaybackAction.NextTrack(EffectPayload.Request(false)))
window.prev = Actions.Playback(PlaybackAction.PreviousTrack(EffectPayload.Request(false)))

window.wsOpen = Actions.WS(WebSocketAction.Connect(EffectPayload.Request(true)))
window.wsClose = Actions.WS(WebSocketAction.Disconnect(EffectPayload.Request(true)))
window.wsSend = R.compose(
  Actions.WS,
  WebSocketAction.Send,
  EffectPayload.Request
)
