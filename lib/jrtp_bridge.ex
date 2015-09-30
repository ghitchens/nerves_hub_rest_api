defmodule JrtpBridge do
  @moduledoc """
  JrtpBridge - JSON/REST Transport Protocol Bridge

  Supports the REST methodoy to access points on the Hub, using JSON as
  the notation of the state.

  GET /a/point       Maps to Hub.deltas
  PUT /a/point       Maps to Hub.update
  """

  alias :cowboy_req, as: CowboyReq
  require Firmware

  @doc false
  def init(_transport, _req, _state) do
    {:upgrade, :protocol, :cowboy_rest} #, req, state}
  end

  @doc false
  def rest_init(req, handler_opts) do
    {:ok, req, handler_opts}
  end

  @doc false
  def resource_exists(req, state) do
    case Hub.fetch(request_path(req)) do
      {_, :error} -> {false, req, state}
      {_, _} -> {true, req, state}
    end
  end

  @doc false
  def allowed_methods(req, state) do
    {["GET", "PUT"], req, state}
  end

  @doc false
  def content_types_provided(req, state) do
    {[{"application/merge-patch+json", :rfc7386_provider},
      {"application/json", :json_provider},
      {"text/html", :html_provider},
      {"text/plain", :text_provider}
      ], req, state}
  end

  @doc false
  def rfc7386_provider(req, state) do
    path = request_path(req)
    {vers_header, req} = CowboyReq.header("x-since-version", req)
    vreq = vheader_to_ver(vers_header)
    {long_poll, req} = CowboyReq.header("x-long-poll", req)
    {long_poll_header_value, req} = CowboyReq.header("x-long-poll-timeout", req)
    long_poll_timeout = case long_poll_header_value do
      :undefined -> 30000
      n -> String.to_integer n
    end
    {vres, tree} = case long_poll do
      :undefined -> Hub.deltas(vreq, path)
      _ ->
        start_result = case Dict.get(state, :on_wait_start) do
          nil -> nil
          wait_start_fn -> wait_start_fn.()
        end
        Hub.watch(path, [])
        r = wait_for_version_after(vreq, path, long_poll_timeout)
        Hub.unwatch(path)
        case Dict.get(state, :on_wait_end) do
          nil -> nil
          wait_end_fn -> wait_end_fn.(start_result)
        end
        r
    end

    {vlock_req, _} = vreq
    {vlock_res, _} = vres

    #{set_time, req} = CowboyReq.header("x-set-time", req)
    req = CowboyReq.set_resp_header("x-version", ver_to_vheader(vres), req)
    req = invoke_response_hook_if_present(req, state)

    req = case vlock_res do
      v when v == vlock_req -> req
      _ -> CowboyReq.set_resp_header("content-type", "application/json", req)
    end

    case {vreq, tree} do
      {{:undefined, 0}, []} -> {"", req, state}
      {_, []} ->
        {:ok, req} = CowboyReq.reply(304, [], req)
        {:halt, req, state}
      _ ->
        body = erl_to_json tree
        { body <> "\n", req, state}
    end
  end

  @doc false
  def json_provider(req, state) do
    path = request_path(req)
    {vres, tree} = Hub.deltas({:undefined, 0}, path)
    req = CowboyReq.set_resp_header("x-version", ver_to_vheader(vres), req)
    req = invoke_response_hook_if_present(req, state)
    case tree do
      [] -> {"", req, state}
      _ ->
        body = erl_to_json tree
        { body <> "\n", req, state}
    end
  end

  @doc false
  def html_provider(req, state) do
    header = "<html><head><meta charset=\"utf-8\"><title>#{Dict.get(state, :webpage_title)}</title></head><body><pre>"
    footer = "</pre></body></html>"
    {body, reply, state} = json_provider(req, state)
    {header <> body <> footer, reply, state}
  end

  @doc false
  def text_provider(req, state) do
    json_provider(req, state)
  end

  @doc false
  def content_types_accepted(req, state) do
    {[
      {{"application", "merge-patch+json", []}, :rfc7386_acceptor},
      {{"application", "json", []}, :json_acceptor},
      {{"application", "x-firmware", []}, :firmware_acceptor}
    ], req, state}
  end
  
  # REVIEW: these "extra" acceptors should be able to be passed in somehow...
  # x-firmware, x-device-lock... 
  def firmware_acceptor(req, state) do
    Firmware.upload_acceptor(req, state)
  end

  def rfc7386_acceptor(req, state) do
    json_acceptor(req, state)
  end

  def json_acceptor(req, state) do
    {:ok, request_body, req} = CowboyReq.body(req)
    proposed_changes = json_to_erl(request_body)
    case Hub.request(request_path(req), proposed_changes) do
      {:changes, vres, changes} ->
        change_json = erl_to_json(changes)
        response_body = change_json <> "\n"
        bver = ver_to_vheader(vres)
        req = CowboyReq.set_resp_header("x-version", bver, req)
        req = CowboyReq.set_resp_body(response_body, req)
        {true, req, state}
      {:nochanges, vres, _changes} ->
        bver = ver_to_vheader(vres)
        req = CowboyReq.set_resp_header("x-version", bver, req)
        {:ok, req} = CowboyReq.reply(304, [], req)
        {:halt, req, state}
      :ok ->
        {:ok, req} = CowboyReq.reply(202, [], req)
        {:halt, req, state}
      _ ->
        {:ok, req} = CowboyReq.reply(400, [], req)
        {:halt, req, state}
    end
  end

  defp vheader_to_ver(version_header_value) do
    case version_header_value do
      :undefined -> {:undefined, 0}
      s ->
        case String.split(s, ":") do
          [vlock, vs] -> {vlock, String.to_integer(vs)}
          _ -> {:undefined, 0}
        end
    end
  end

  defp ver_to_vheader({vlock, ver}) do
    bver = Integer.to_string ver
    "#{vlock}:#{bver}"
  end

  defp invoke_response_hook_if_present(req, state) do
    case Dict.get(state, :json_provider_hook) do
      nil -> req
      resp_hook_fn -> resp_hook_fn.(req)
    end
  end

  defp request_path(req) do
    {tokens, _} = CowboyReq.path_info(req)
    tokens
  end

  defp wait_for_version_after(vreq, path, long_poll_timeout) do
    case Hub.deltas(vreq, path) do
      {vq, _} when vq == vreq->
        receive do
          _ -> wait_for_version_after(vq, path, long_poll_timeout)
        after
          long_poll_timeout ->
            Hub.deltas(vq, path)
        end
      {vres, []} ->
          wait_for_version_after(vres, path, long_poll_timeout)
      {vres, change_tree} ->
        {vres, change_tree}
    end
  end

  defp erl_to_json(term) do
    :jsx.encode(term, [{:space, 1}, {:indent, 2}, {:pre_encode, &deatomize/1}])
  end

  defp json_to_erl(json) do
    case :jsx.decode(json, [:relax, {:labels, :atom}, {:post_decode, &atomize/1}]) do
      {:incomplete, _} -> throw(:error)
      erl -> erl
    end
  end

  # converts a binary to an atom
  defp atomize(<< h :: size(1)-binary, b :: binary>>) do
    case h do
      "#" -> String.to_atom b
      _ -> h <> b
    end
  end

  defp atomize(o), do: o

  # converts an atom to a binary
  defp deatomize(true), do: true
  defp deatomize(false), do: false
  defp deatomize(nil), do: nil
  defp deatomize(a) when is_atom(a), do: Atom.to_string a
  defp deatomize(o), do: o
end
