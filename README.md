# mixtape.
> A in-sync listening experience for Spotify

![](https://raw.githubusercontent.com/ostera/mixtape/master/assets/in_app.png)

## How To Run

You will need:

* make
* Erlang 20 or better with rebar3
* node and npm or yarn
* a web server that can fallback on unknown paths (beefy, apache, nginx)

With all this setup you can:

1. Start the server with `make run` from the `mixtape_service` folder
2. Build the client with `make` from the `web_client` folder
3. Profit!

Now you can open the web server, get the client, connect with Spotify, and start
listening to your favorite playlist.

### Trying the Synchronization

Bring up someone else, and have them load the client from the browser (could be
by accessing your local server, or maybe you're running this somewhere public).

Once they're connected, they just need to paste in the same playlist URI and
you're good to go.

## System Description (with pictures!)

As synchronization can be tricky, and because this is an experiment in both
synchronizing state between web players, and also a small thesis on building
web applications with typed actions and a minimum set of dependencies, I've
described some of the flows with swimlane diagrams.

#### Setup, and Authorization Flow

![](http://static.swimlanes.io/59cf3c0ac7eabbf5bd8758d1e8a67a77.png)

#### Web-client Synchronization Flow

![](http://static.swimlanes.io/f4e86175a38867ee12a99966dad9b505.png)

#### Sync-service Flow

![](http://static.swimlanes.io/c026b4300bc47963c16bf5da27a8b161.png)

## What This Thing Experiments With

1. Typed Actions and User-defined Pattern Matching
2. Effects Pattern 
3. CSS in Javascript
4. Profunctor Lenses for manipulating Data structures
