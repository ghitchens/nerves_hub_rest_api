defmodule JrtpBridge do

  alias :cowboy_req, as: CowboyReq

  def init(_transport, req, state) do
    {:upgrade, :protocol, :cowboy_rest, req, state}
  end

  def rest_init(req, handler_opts) do
    {:ok, req, handler_opts}
  end

  def resource_exists(req, state) do
    case Hub.fetch(request_path(req)) do
      {_, :error} -> {false, req, state}
      {_, _} -> {true, req, state}
    end
  end

  def allowed_methods(req, state) do
    {["GET", "PUT"], req, state}
  end

  def content_types_provided(req, state) do
    {[{"application/merge-patch+json", :rfc7386_provider},
      {"application/json", :json_provider},
      {"text/html", :html_provider},
      {"text/plain", :text_provider}
      ], req, state}
  end

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

    conditional_get = (vreq > 0)
    case {vreq, tree} do
      {{:undefined, 0}, []} -> {"", req, state}
      {_, []} ->
        {:ok, req} = CowboyReq.reply(304, [], req)
        {:halt, req, state}
      _ ->
        body = erl_to_json(tree)
        {"#{body}\n", req, state}
    end
  end

  def json_provider(req, state) do
    path = request_path(req)
    {vres, tree} = Hub.deltas({:undefined, 0}, path)
    req = CowboyReq.set_resp_header("x-version", ver_to_vheader(vres), req)
    req = invoke_response_hook_if_present(req, state)
    case tree do
      [] -> {"", req, state}
      _ ->
        body = erl_to_json tree
        {body <> "\n", req, state}
    end
  end

  def html_provider(req, state) do
    header = "<html><head><meta charset=\"utf-8\">#{Dict.get(state, :webpage_title)}</head><body><pre>"
    footer = "</pre></body></html>"
    {body, reply, state} = json_provider(req, state)
    {header <> body <> footer, reply, state}
  end

  def text_provider(req, state) do
    json_provider(req, state)
  end

  def vheader_to_ver(version_header_value) do
    case version_header_value do
      :undefined -> {:undefined, 0}
      s ->
        case String.split(s, ":") do
          [vlock, vs] -> {vlock, String.to_integer(vs)}
          _ -> {:undefined, 0}
        end
    end
  end

  def ver_to_vheader({vlock, ver}) do
    bver = :erlang.list_to_binary(:erlang.integer_to_list(ver))
    "#{vlock}:#{bver}"
  end

  def content_types_accepted(req, state) do
    {[
      {{"application", "merge-patch+json", []}, :rfc7386_acceptor},
      {{"application", "json", []}, :json_acceptor},
    ], req, state}
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
      {:nochanges, vres, changes} ->
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

  def invoke_response_hook_if_present(req, state) do
    case Dict.get(state, :json_provider_hook) do
      nil -> req
      resp_hook_fn -> resp_hook_fn.(req)
    end
  end

  def request_path(req) do
    {tokens, _} = CowboyReq.path_info(req)
    tokens
  end

  def wait_for_version_after(vreq, path, long_poll_timeout) do
    case Hub.deltas(vreq, path) do
      {vreq, _} ->
        receive do
          _ -> wait_for_version_after(vreq, path, long_poll_timeout)
        after
          long_poll_timeout ->
            Hub.deltas(vreq, path)
        end
      {vres, []} ->
        wait_for_version_after(vres, path, long_poll_timeout)
      {vres, change_tree} ->
        {vres, change_tree}
    end
  end

  def erl_to_json(term) do
    :jsx.encode(term, [{:space, 1}, {:indent, 2}, {:pre_encode, &deatomize/1}])
  end

  def json_to_erl(json) do
    case :jsx.decode(json, [:relax, {:labels, :atom}, {:post_decode, &atomize/1}]) do
      {:incomplete, _} -> throw(:error)
      erl -> erl
    end
  end

  def atomize(<< h :: size(1)-binary, b :: binary>>) do
    case h do
      "#" -> :erlang.binary_to_atom(b, :utf8)
      _ -> h <> b
    end
  end

  def atomize(o), do: o

  def deatomize(true), do: true
  def deatomize(false), do: false
  def deatomize(nil), do: nil
  def deatomize(a) when is_atom(a), do: :erlang.atom_to_binary(a, :utf8)
  def deatomize(o), do: o
end
