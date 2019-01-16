% Copyright (C) 2003-2016 Olivier Boudeville
%
% This file is part of the Ceylan Erlang library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville (olivier.boudeville@esperide.com)
% Creation date: July 1, 2007.


% Defines some macros and functions useful for trace-using cases.
% This is thus the main/only header file such cases should include.

% Note: directly obtained from traces_for_tests.hrl.

% We have kept macros for all the traces (including the ones for cases, and
% start/stop) for the sake of consistency. Moreover doing so allows to
% communicate more easily with agents like the trace aggregator (as we can then
% share discretly variables like TraceAggregatorPid).


% Defines everything regarding application traces:
-include("traces_case_header.hrl").


% For export of run/0:
-include("case_facilities.hrl").


% To avoid warnings if not used:
-export([ case_receive/0, case_receive/1, test_receive/0, test_receive/1,
		  case_failed/1, case_failed/2, test_failed/1, test_failed/2 ]).


% For notify_* and al:
-include("traces.hrl").







% Start/stop section.


-ifdef(tracing_activated).


% TraceAggregatorPid voluntarily exported from case_start, for case_stop:

-define( case_start,
	% No supervisor wanted from scratch, as their trace file will have to be
	% renamed and the LogMX supervisor cannot change the file it is tracking:
	%
	TraceAggregatorPid = traces_for_cases:case_start( ?MODULE,
											 _InitTraceSupervisor=false )
).


-define( case_stop,
	traces_for_cases:case_stop( ?MODULE, TraceAggregatorPid )
).


-else. % tracing_activated


% Here, even if the trace sending is deactivated, a trace aggregator is created,
% as some processes nevertheless expect to find one at start-up, or some of them
% may have been recompiled to be trace-enabled.
%
% However no trace supervisor is needed here.
-define( case_start,
	TraceAggregatorPid = traces_for_cases:case_start( ?MODULE,
											 _InitTraceSupervisor=false ) ).


-define( case_stop,
	% No supervisor to wait for, here:
	traces_for_cases:case_immediate_stop( ?MODULE, TraceAggregatorPid ) ).


-endif. % tracing_activated



% Valid whether or not tracing is activated:

-define( case_stop_without_waiting_for_trace_supervisor,
	traces_for_cases:case_immediate_stop( ?MODULE, TraceAggregatorPid ) ).


-define( case_stop_on_shell,
	traces_for_cases:case_stop_on_shell( ?MODULE, TraceAggregatorPid ) ).







%%%%%%%%%%%%%%%%%%%%%%%%% Between header and footer %%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Defines everything regarding application traces:
-include("traces_case_footer.hrl").



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( case_receive, case_receive() ).



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( case_receive( AnyMessage ), case_receive( AnyMessage ) ).


% For test support:
-define( test_receive, case_receive() ).
-define( test_receive( AnyMessage ), case_receive( AnyMessage ) ).



% Helper function to write receive clauses in cases which cannot interfere with
% trace supervision, as a case may also receive trace control message the case
% code should be unware of.
%
% Returns the received value.
%
% Ex: Pid ! { getBaz, [], self() }, MyBaz = case_receive(), ...
%
% to be used instead of:
%
% Pid ! { getBaz, [], self() },
% receive
%
%   { wooper_result, V } ->
%			V
%
% end,
% ...
%
-spec case_receive() -> any().
case_receive() ->
	traces:receive_applicative_message().



% Helper function to write receive clauses for specific messages in cases while
% not interfering with trace supervision.
%
-spec case_receive( any() ) -> basic_utils:void().
case_receive( Message ) ->
	traces:receive_applicative_message( Message ).



% Test support:
-spec test_receive() -> any().
test_receive() ->
	case_receive().


-spec test_receive( any() ) -> basic_utils:void().
test_receive( Message ) ->
	case_receive( Message ).



% Helper macro for those who would not know they could have called the
% corresponding function directly:
%
-define( case_failed, case_failed() ).



% Handles a case failure, using specified string as advertised reason.
%
-spec case_failed( string() ) -> no_return().
case_failed( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	Message = io_lib:format( "Case ~s failed, reason: ~s.~n",
							 [ ?MODULE, Reason ] ),

	error_logger:error_msg( Message ),
	?case_fatal( Message ),
	% Needed, otherwise error_logger may not display anything:
	system_utils:await_output_completion(),
	erlang:error( "Case ~s failed.", [ ?MODULE ] ).



% Handles a case failure, using specified first string as an advertised reason
% with format characters (ex: '~w') and specified list as actual values to be
% formatted.
%
-spec case_failed( text_utils:format_string(), [ any() ] ) ->
						 no_return().
case_failed( Reason, FormattedValue ) ->
	case_failed( io_lib:format( Reason, FormattedValue ) ).



% Test support:

-spec test_failed( string() ) -> no_return().
test_failed( Reason ) ->
	case_failed( Reason ).


-spec test_failed( text_utils:format_string(), [ any() ] ) ->
						 no_return().
test_failed( Reason, FormattedValue ) ->
	case_failed( Reason, FormattedValue ).
