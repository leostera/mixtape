// Polyfills
require('fetch')

// Dependencies
const R = require('ramda')
const Type = require('./type')
const diff = require('jiff').diff

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
const currentDevice = R.lensPath(['devices', 'current'])
const currentLocation = R.lensPath(['location'])
const requestOffset = R.lensPath(['playback', 'offset'])
const currentPath = R.lensPath(['location', 'pathname'])
const currentPlayback = R.lensPath(['playback'])
const currentPlaylist = R.lensPath(['playlist', 'current'])
const currentUser = R.lensPath(['user', 'current'])
const errors = R.lensPath(['errors'])
const location = R.lensPath(['location'])
const locationCount = R.lensPath(['app', 'locationCount'])
const locationHash = R.lensPath(['location', 'hash'])
const loginUrl = R.lensPath(['app', 'loginUrl'])
const playbackStatus = R.lensPath(['playback', 'status'])
const playlistUri = R.lensPath(['playlist', 'uri'])
const requestAccess = R.lensPath(['user', 'requestAccess'])
const requestDevices = R.lensPath(['devices', 'requestDevices'])
const requestNext = R.lensPath(['playback', 'requestNext'])
const requestPause = R.lensPath(['playback', 'requestPause'])
const requestPlay = R.lensPath(['playback', 'requestPlay'])
const requestPlaylist = R.lensPath(['playlist', 'requestPlaylist'])
const requestPrevious = R.lensPath(['playback', 'requestPrevious'])
const requestSeek = R.lensPath(['playback', 'requestSeek'])
const requestUser = R.lensPath(['user', 'requestUser'])

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
    { name: 'User', arity: 1 }
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
    { name: 'Seek', arity: 1 }
  ]
})

const PlaybackStatus = Type({
  typeName: 'PlaybackStatus',
  constructors: [
    { name: 'Playing', arity: 1 },
    { name: 'Paused', arity: 1 }
  ]
})

/*
const SyncAction = Type({
  typeName: 'SyncAction',
  constructors: [
    { name: 'Connect', arity: 1 },
    { name: 'Sync', arity: 1 }
  ]
})
*/

/*******************************************************************************
 *
 * Reducers
 *
 ******************************************************************************/

const config = {
  client_id: `a84bae1d337b4862bd5203bc507005f6`,
  redirect_uri: `http://localhost:8000/auth/spotify/success`,
  scope: [
    'streaming',
    'user-modify-playback-state',
    'user-read-email',
    'user-read-playback-state',
    'user-read-private'
  ].join(' ')
}

const initialState = {
  listeningSession: {},
  playback: {
    requestNext: false,
    requestPause: false,
    requestPlay: false,
    requestPrevious: false,
    requestSeek: false
  },
  playlist: false,
  user: {},
  location: false,
  app: {
    volume: 0.7,
    config,
    locationCount: 0,
    loginUrl: `https://accounts.spotify.com/authorize?${querify(config)}&response_type=token`
  }
}

const userReducer = UserAction.match({
  FetchProfile: EffectPayload.match({
    Request: () => R.set(requestUser, true),
    Response: Result.match({
      Ok: user => R.compose(
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
    Request: () => R.set(requestPlay, true),
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
      Ok: status => R.set(requestNext, false),
      Err: error => R.compose(
        R.set(requestNext, false),
        R.set(errors, error)
      )
    })
  }),
  PreviousTrack: EffectPayload.match({
    Request: () => R.set(requestPrevious, true),
    Response: Result.match({
      Ok: status => R.set(requestPrevious, false),
      Err: error => R.compose(
        R.set(requestPrevious, false),
        R.set(errors, error)
      )
    })
  }),
  Seek: EffectPayload.match({
    Request: offset => R.compose(
      R.set(requestSeek, true),
      R.set(requestOffset, offset)
    ),
    Response: Result.match({
      Ok: status => R.compose(
        R.set(requestSeek, false),
        R.set(requestOffset, false)
      ),
      Err: error => R.compose(
        R.set(requestSeek, false),
        R.set(errors, error)
      )
    })
  })
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
      R.set(requestDevices, true),
      R.set(requestUser, true),
      R.set(requestAccess, false),
      R.set(accessGrantData, data)
    )
  }),
  Playback: playbackReducer
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
      log('Route match found! Triggering action...', match.f(state))
      next(match.f(state))
    }
  }
}

const _fetch = ({method, shouldRequest, endpoint, handle}) => next => {
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

      const handleResponse = response => R.compose(
        next,
        handle,
        data => response.status > 400 ? Result.Err(data) : Result.Ok(data)
      )

      window.fetch(`https://api.spotify.com/${endpoint(state)}`, requestOpts)
        .then(response => response
          .json()
          .then(handleResponse(response))
          .catch(handleResponse(response)))
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

const _playback = ({method, lens, name, query, actionType}) => _fetch({
  method,
  shouldRequest: R.view(lens),
  endpoint: state => `v1/me/player/${name}${query ? query(state) : ''}`,
  handle: R.compose(
    Actions.Playback,
    actionType,
    EffectPayload.Response
  )
})

const playbackPlay = _playback({
  method: 'PUT',
  lens: requestPlay,
  name: 'play',
  actionType: PlaybackAction.Play
})

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

const playbackSeek = _playback({
  method: 'PUT',
  lens: requestSeek,
  name: 'seek',
  query: R.compose(
    ms => `?position_ms=${ms}`,
    R.view(requestOffset)
  ),
  actionType: PlaybackAction.Seek
})

const syncSend = send => next => {
  let lastState = false
  return state => {
    const thisPlayback = R.view(currentPlayback, state)
    const lastPlayback = R.view(currentPlayback, lastState)
    const playbackDiff = diff(thisPlayback, lastPlayback)

    const shouldSync = lastState && R.length(playbackDiff) > 0
    if (shouldSync) {
      console.group('Syncing:')
      R.forEach(log, playbackDiff)
      console.groupEnd()
    }

    lastState = state
  }
}

const syncRecieve = on => next => {
  on('data', data => {
    // Data -> Action -> Next ()
  })

  return state => {}
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

const debug = next => {
  let lastState = {}
  return state => {
    const changes = diff(lastState, state)
    if (R.length(changes) > 0) {
      console.group('State Change:')
      R.forEach(log, changes)
      console.groupEnd()
    } else {
      console.group('No State Changes.')
      console.groupEnd()
    }
    lastState = state
    log('Current State', lastState)
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
  playbackNext,
  playbackPause,
  playbackPlay,
  playbackPrevious,
  playbackSeek,
  render,
  router(routingTable),
  syncSend(x => x),
  syncRecieve(x => x)
]

const subscribeEffects = store => R.compose(
  R.forEach(store.subscribe),
  R.map(e => () => e(store.getState())),
  R.map(e => e(store.dispatch))
)

const redux = require('redux')
const createStore = r => a => redux.createStore((state, action) => {
  log('Action: ', action)
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
window.bootstrap = Actions.Bootstrap()
window.playlistUri = Actions.Playlist(PlaylistAction.Fetch(EffectPayload.Request('spotify:user:leostera:playlist:5WZyjENJIepWYuz1Vnfuma')))
window.play = Actions.Playback(PlaybackAction.Play(EffectPayload.Request(false)))
window.pause = Actions.Playback(PlaybackAction.Pause(EffectPayload.Request(false)))
window.skip = Actions.Playback(PlaybackAction.NextTrack(EffectPayload.Request(false)))
window.prev = Actions.Playback(PlaybackAction.PreviousTrack(EffectPayload.Request(false)))
window.seek = x => Actions.Playback(PlaybackAction.Seek(EffectPayload.Request(x)))
