%%%=============================================================================
%%% lswitch_ofsh.erl
%%%
%%% Module implementing the callback functions required by 'ofs_handler'.
%%%
%%% Every time a switch is connected to this controller, a learning switch
%%% process is fired up for that switch.
%%%
%%% This module is responsible for forwarding all relevant events to their
%%% corresponding learning switch process.
%%%
%%% Author: Enrique Fernandez <efcasado@gmail.com>
%%% Date:   June, 2014
%%%
%%% License:
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014 Enrique Fernández
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%=============================================================================
-module(lswitch_ofsh).

-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

%% ofs_handler callback functions
-export([init/7, connect/8,
         terminate/1, disconnect/1,
         handle_message/2, handle_error/2]).

%% ofs_handler's state record
-record(state,
        {
          datapath_id,
          lswitch_pid :: pid()
        }).

%% Type definitions
-type state() :: #state{}.


%% ===============================
%%  ofs_handler callback functions
%% ===============================

-spec init(handler_mode(), ipaddress(), datapath_id(), features(),
           of_version(), connection(), options()) ->
                  {'ok', state()}.
init(_Mode, IPAddr, DPID, _Features, _Vsn, _Conn, _Opts) ->
    lager:info("Switch connected: ~p ~p", [DPID, IPAddr]),
    {ok, PID} = lswitch:start(DPID),
    {ok, #state{datapath_id = DPID, lswitch_pid = PID}}.

-spec connect(handler_mode(), ipaddress(), datapath_id(), features(),
              of_version(), connection(), auxid(), options()) ->
                     {'ok', state()}.
connect(_Mode, _IpAddr, DPID, _Features, _Vsn, _Conn, _AuxId, _Opts) ->
    {ok, #state{datapath_id = DPID}}.

-spec handle_message(ofp_message(), state()) -> 'ok'.
handle_message({packet_in, _Xid, Body} = Msg, #state{lswitch_pid = PID}) ->
    lager:info("Message received: ~p", [Msg]),
    lswitch:packet_in(eth_frame(Body), port_in(Body), PID),
    ok.

-spec handle_error(error_reason(), state()) -> 'ok'.
handle_error(_Reason, _State) ->
    'ok'.

-spec disconnect(state()) -> 'ok'.
disconnect(_State) ->
    'ok'.

-spec terminate(state()) -> 'ok'.
terminate(_State) ->
    'ok'.

eth_frame(Packet) ->
    RawEthFrame = proplists:get_value(data, Packet),
    eth:decode(RawEthFrame).

port_in(Packet) ->
    PortIn = proplists:get_value(in_port, proplists:get_value(match, Packet)),
    %% Note: PortIn should be already decoded.
    binary:decode_unsigned(PortIn).
