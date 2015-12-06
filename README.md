HubRestApi
==========

Implements a REST-based plugin for the Cowboy webserver that allows json-based transactions against hub for state.

## Usage

HubRestApi is a handler for Cowboy.   An example that serves up the entire Hub at /hub/.... on port 8080.

```elixir

import Nerves.HubRestApi

dispatch = :cowboy_router.compile [{:_,[
    {"/hub/[...]", HubRestApi, %{} }
}]}]

{:ok, _pid} = :cowboy.start_http :http, 10,
  [port: 8080],
  [env: [dispatch: dispatch] ]
```

## Options

HubRestApi can also take takes a couple options in the options map, as follows:

on_wait_start :: fn/0

Called when a connection is initiated, may return a value which is passed to on_wait_stop.

on_wait_end :: fn/1

Called with the result of on_wait_start, if exists, or false, if not.  

json_provider_hook :: fn/1

Allows hooking the responses given by th json_provider and rfc7386_provider Takes a cowboy_req parameter, and returns a modified cowboy_req.

