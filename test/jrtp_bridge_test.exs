defmodule JrtpBridgeTest do

  use ExUnit.Case

  HTTPotion.start  

  # configure and start a basic http server on port 8088 using cowboy 
  # that puts the jrtp bridge at the /jrtp/ directory purposes.

  @test_http_port 8088
  @test_http_root "localhost:#{@test_http_port}/jrtp/"
  @silly_path [:tests, :silly] 
  @silly_http @test_http_root <> "tests/silly/"

  def connect_pinger do
    #IO.write "Keepalive Called"
    #Led.alive :client, 5000
    :timer.sleep 1000
    connect_pinger
  end

  dispatch = :cowboy_router.compile [ 
    {:_, [ 
      {"/jrtp/[...]", :jrtp_bridge, %{ 
        on_wait_start: (fn -> spawn(&JrtpBridgeTest.connect_pinger/0) end),
        on_wait_end:   &(:erlang.exit(&1, :disconnected)) 
      }} ]} ]

  {:ok, _pid} = :cowboy.start_http :http, 10, [port: @test_http_port], 
                                              [env: [dispatch: dispatch] ]

  :hub.start


  test "webserver is up and returns root of url space as json when asked for json" do
    # need to put a key there otherwise can get a 304
    :hub.update [ "testing" ], [ "a_key": "a_value" ], []
    resp = HTTPotion.get @test_http_root, headers: ["Accept": "application/json"]
    headers = resp.headers
    assert resp.status_code == 200
    assert {:ok, "Cowboy"} = Keyword.fetch(headers, :server)
    assert {:ok, "application/json"} = Keyword.fetch(headers, :'content-type')
  end

  test "webserver is up and returns root of url space as merge-patch+json if allowed" do
    # need to put a key there otherwise can get a 304
    :hub.update [ "testing" ], [ "a_key": "another_value" ], []
    resp = HTTPotion.get @test_http_root
    headers = resp.headers
    assert resp.status_code == 200
    assert {:ok, "Cowboy"} = Keyword.fetch(headers, :server)
    assert {:ok, "application/json"} = Keyword.fetch(headers, :'content-type')
  end

  test "webserver returns merge-patch+json when changes occur changes" do
    # need to put a key there otherwise can get a 304
    {{vlock, vnum}, _} = :hub.fetch []
    :hub.update [ "testing" ], [ "a_key": "yet_another_value" ], []
    resp = HTTPotion.get @test_http_root, headers: ["x-since-version": "#{vlock}:#{vnum}"]
    headers = resp.headers
    assert resp.status_code == 200
    assert {:ok, "Cowboy"} = Keyword.fetch(headers, :server)
    assert {:ok, "application/merge-patch+json"} = Keyword.fetch(headers, :'content-type')
  end

  # helpers for the JSON tests

  @doc "Return the value of a header on an integer - key can be atom or str"
  def header(resp, key) do
    unless is_atom(key) do
      key = String.to_atom(key)
    end
    assert {:ok, result} = Keyword.fetch(resp.headers, key)
    {:ok, result}
  end

  @doc "Return the integer value of the version header value"
  def iver(vhdr) do
    [_, ver] = String.split(vhdr, ":")
    :erlang.binary_to_integer(ver)
  end

  @doc "Return term from decoded json body of the response"
  def jterm(resp) do
    #{:ok, content_type} = header resp, "content-type"
    :jrtp_bridge.json_to_erl(resp.body)
  end

  test "JSON interface to querying hub including versions and deltas" do

    test_data1 = [ basic_key: "yes", another_key: "no", with: 3 ]
    test_data2 = [ basic_key: "yes", another_key: "maybe", with: 3 ]
    test_rootkey = :jrtp_temp_test_key
    test_subkey = :webtest
    test_path = [ test_rootkey, test_subkey ]
    test_uri = "#{@test_http_root}#{test_rootkey}/#{test_subkey}"
    test_root_uri = "#{@test_http_root}#{test_rootkey}"
    
    # attempt to get data from our test area.  This should generate
    # a 404 since we haven't created the data there yet

    resp = HTTPotion.get test_uri
    assert 404 == resp.status_code  # shouldn't be there yet

    # now write some test data to the hub, and then query the HTTP
    # interface to see if we actually get the data back

    :hub.update test_path, test_data1
    resp = HTTPotion.get test_uri
    assert 200 == resp.status_code  # should be there now
    assert {:ok, "application/json"} = header resp, "content-type"
    assert {:ok, first_vers} = header resp, "x-version"
    assert Keyword.equal?(jterm(resp), test_data1)
    
    # test updating the hub and seeing if we can query the HTTP
    # interface to see the updated version and updated data

    :hub.update test_path, test_data2
    resp = HTTPotion.get test_uri
    assert 200 == resp.status_code
    assert Keyword.equal?(jterm(resp), test_data2)
    assert {:ok, second_vers} = header resp, "x-version"
    assert second_vers != first_vers
    [a, _b] = String.split(second_vers, ":")
    assert String.length(a) > 5
    assert iver(second_vers) == (iver(first_vers) + 1)
    
    # now make sure that if we ask for changes since a particular 
    # version, that we only get back changes since that version

    resp = HTTPotion.get test_uri, headers: [ "x-since-version": first_vers]
    assert 200 == resp.status_code
    assert {:ok, second_vers} == header resp, "x-version"
    assert {:ok, "application/merge-patch+json"} = header resp, "content-type"
    assert jterm(resp) == [ another_key: "maybe" ]

		# make sure we timeout properly by reqesting a uri with long polling, 
		# and sending a change with no changes, and hopefully we don't get response
		
		spawn fn ->
			:timer.sleep 300
			:hub.update test_path, test_data2
			:timer.sleep 100
			:hub.update test_path, test_data2
			:timer.sleep 100
			:hub.update test_path, test_data2
			:timer.sleep 100
			:hub.update test_path, test_data2
		end
		
    resp = HTTPotion.get test_root_uri, headers: [ 
			"x-since-version": second_vers, 
			"x-long-poll": true, 
			"x-long-poll-timeout": "3000"
		]
    assert {:ok, second_vers} == header resp, "x-version"
		assert 304 == resp.status_code			 # should be NOT MODIFIED

		# now make the same change, and ask if changes happened

    :hub.update test_path, test_data2
    resp = HTTPotion.get test_uri, headers: [ "x-since-version": second_vers]
    assert 304 == resp.status_code			 # should be NOT MODIFIED
    assert {:ok, second_vers} == header resp, "x-version"

  end
end
