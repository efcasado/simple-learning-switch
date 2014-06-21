%%%=============================================================================
%%% lswitch.erl
%%%
%%% Implementation of the learning switch logic.
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
-module(lswitch).

-behaviour(gen_server).

%% API
-export([start/1,
         packet_in/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2,
         code_change/3]).

%% Macro definitions
-define(SERVER, ?MODULE).
-define(PRIORITY, 16#3000).

%% Record definitions
-record(state,
        {
          datapath_id,
          mac_to_port = #{}
        }).


%% =====
%%  API
%% =====

start(DPID) ->
    gen_server:start(?MODULE, [DPID], []).

packet_in(EthFrame, PortIn, To) ->
    gen_server:cast(To, {packet_in, EthFrame, PortIn}).


%% ======================
%%  gen_server callbacks
%% ======================

init([DPID]) ->
    gen_server:cast(self(), init),
    {ok, #state{datapath_id = DPID}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(init, State = #state{datapath_id = DPID}) ->
    ok = ofs_handler:subscribe(DPID, lswitch_ofsh, packet_in),
    {noreply, State};
handle_cast({packet_in, EthFrame, PortIn}, State) ->
    NewState = packet_in_(EthFrame, PortIn, State),
    {noreply, NewState}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% =================
%%  Local functions
%% =================

packet_in_(EthFrame, PortIn, State = #state{mac_to_port = MacToPort,
                                            datapath_id = DPID}) ->
    case eth:is_multicast(EthFrame) of
        true  ->
            ok = flood(EthFrame, PortIn, DPID),
            State;
        false ->
            Src = eth:src(EthFrame),
            Dst = eth:dst(EthFrame),
            NewMacToPort = maps:put(Src, PortIn, MacToPort),
            try
                PortOut = maps:get(Dst, MacToPort),
                ok = add_flow(Dst, PortOut, DPID),
                State#state{mac_to_port = NewMacToPort}
            catch
                %% Dst does not exist in MacToPort
                _:_ ->
                    ok = flood(EthFrame, PortIn, DPID),
                    State#state{mac_to_port = NewMacToPort}
            end
    end.

flood(EthFrame, PortIn, DPID) ->
    Actions = [{output, flood, no_buffer}],
    %% Note #1: We shouldn't have to specify the OF version here
    %% Note #2: We shouldn't have to specify output as an action when
    %% using send_packet
    Request = of_msg_lib:send_packet(4, eth:encode(EthFrame), PortIn, Actions),
    ofs_handler:send(DPID, Request).

add_flow(Dst, PortOut, DPID) ->
    Matches = [{eth_dst, Dst}],
    Instructions = [{apply_actions, [{output, PortOut, no_buffer}]}],
    Request = of_msg_lib:flow_add(4, Matches, Instructions, []),
    ofs_handler:send(DPID, Request).
