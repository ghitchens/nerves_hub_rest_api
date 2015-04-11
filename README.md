JrtpBridge
==========

"JSON/REST Transport Protocol" Bridge

Implements a REST-based plugin for the Cowboy webserver that allows json-based transactions against hub for state.

## Usage

JrtpBridge is plugged in as a handler for Cowboy.   Here is a simple example that serves up the entire Hub at /jrtp/.... , without options/callbacks.

```elixir

dispatch = :cowboy_router.compile [{:_,[
    {"/jrtp/[...]", :jrtp_bridge, %{} }
}]}]

{:ok, _pid} = :cowboy.start_http :http, 10,
  [port: 8080],
  [env: [dispatch: dispatch] ]
```

## Options

JrtpBridge can also take takes a couple options in the options map, as follows:

on_wait_start :: fn/0

Called when a connection is initiated, may return a value which is passed to on_wait_stop.

on_wait_end :: fn/1

Called with the result of on_wait_start, if exists, or false, if not.  
