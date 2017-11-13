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
const currentLocation = R.lensPath(['location'])
const currentPath = R.lensPath(['location', 'pathname'])
const currentPlaylist = R.lensPath(['playlist', 'current'])
const currentUser = R.lensPath(['user', 'current'])
const currentUsername = R.lensPath(['user', 'current', 'id'])
const errors = R.lensPath(['errors'])
const location = R.lensPath(['location'])
const locationCount = R.lensPath(['app', 'locationCount'])
const locationHash = R.lensPath(['location', 'hash'])
const loginUrl = R.lensPath(['app', 'loginUrl'])
const playlistUri = R.lensPath(['playlist', 'uri'])
const requestAccess = R.lensPath(['user', 'requestAccess'])
const requestPlaylist = R.lensPath(['playlist', 'requestPlaylist'])
const requestUser = R.lensPath(['user', 'requestUser'])
const volume = R.lensPath(['app', 'volume'])

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
    { name: 'Fetch', arity: 1 }
  ]
})

const PlaylistAction = Type({
  typeName: 'PlaylistAction',
  constructors: [
    { name: 'Fetch', arity: 1 }
  ]
})

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
    'user-read-email',
    'user-read-private'
  ].join(' ')
}

const initialState = {
  listeningSession: {},
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
  Fetch: EffectPayload.match({
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
      R.set(requestUser, true),
      R.set(requestAccess, false),
      R.set(accessGrantData, data)
    )
  })
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

const _fetch = ({shouldRequest, endpoint, handle}) => next => {
  let _run = false
  return state => {
    if (!_run && shouldRequest(state)) {
      _run = true
      const token = R.view(accessToken, state)

      const requestOpts = {
        method: 'GET',
        headers: {
          'Authorization': `Bearer ${token}`
        },
        credentials: 'same-origin'
      }

      window.fetch(`https://api.spotify.com/${endpoint(state)}`, requestOpts)
        .then(response =>
          response
          .json()
          .then(R.compose(
            next,
            handle,
            data => response.status > 400 ? Result.Err(data) : Result.Ok(data)
          )))
          .then(() => { _run = false })
          .catch(() => { _run = false })
    }
  }
}

const fetchUser = _fetch({
  shouldRequest: R.view(requestUser),
  endpoint: () => 'v1/me',
  handle: Result.match({
    Ok: R.compose(
      Actions.User,
      UserAction.Fetch,
      EffectPayload.Response,
      Result.Ok
    ),
    Err: R.compose(
      Actions.User,
      UserAction.Fetch,
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

const player = next => {
  let _lastState = false
  let _player = false
  const _work = (state) => {
    /* eslint-disable */
    _player = new Spotify.Player({
      name: `mixtape:session:${R.view(currentUsername, state)}`,
      getOauthToken: cb => cb(R.view(accessToken, state)),
      volume: R.view(volume, state),
    })
    /* eslint-enable */

    _player.on('ready', x => log('Omg, data ready', x))

    _player.on('initialization_error', x => log('Omg, init error', x))
    _player.on('authentication_error', x => log('Omg, auth error', x))
    _player.on('playback_error', x => log('Omg, playback error', x))
    _player.on('account_error', x => log('Omg, account error', x))

    _player.on('player_state_changed', x => log('Player state changed', x))

    _player.connect()
    window.player = _player
  }
  window.onSpotifyWebPlaybackSDKReady = () => {
    if (_lastState && !R.isNil(R.view(currentUsername, _lastState)) && !_player) {
      return _work(_lastState)
    }
    setTimeout(window.onSpotifyWebPlaybackSDKReady, 100)
  }
  return state => { _lastState = state }
}

/*******************************************************************************
 *
 * Glue code
 *
 ******************************************************************************/

const effects = [
  authenticate,
  debug,
  locationEffect,
  fetchUser,
  fetchPlaylist,
  player,
  render,
  router(routingTable)
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

window.next = store.dispatch

window.login = Actions.Session(Session.RequestAccess())
window.bootstrap = Actions.Bootstrap()
window.playlistUri = Actions.Playlist(PlaylistAction.Fetch(EffectPayload.Request('spotify:user:leostera:playlist:5WZyjENJIepWYuz1Vnfuma')))

store.dispatch(Actions.Bootstrap())

// Player, which looks for a callback
require('./player')
