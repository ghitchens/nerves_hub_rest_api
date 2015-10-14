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

json_provider_hook :: fn/1

Allows hooking the responses given by th json_provider and rfc7386_provider Takes a cowboy_req parameter, and returns a modified cowboy_req.

webpage_title :: binary

HTTP requests of Content-Type application/html webpage title will be set to this value

firmware_acceptor :: module

When a Content-Type of application/x-firmware is provided the firmware\_acceptor/2 method of the provided module will be called passing the _req_ and _state_ will be the paramaters.


