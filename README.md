# mixtape.
> A in-sync listening experience for Spotify

## How To Run

You will need:

* make
* Erlang 20 or better with rebar3
* node and npm or yarn
* a web server that can fallback on unknown paths (beefy, apache, nginx)

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
