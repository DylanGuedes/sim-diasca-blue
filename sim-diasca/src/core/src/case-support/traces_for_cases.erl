% Copyright (C) 2007-2017 Olivier Boudeville
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


% Directly obtained from traces_for_tests.erl.


% This module gathers all code that allows to lighten the trace macros for
% cases.
%
-module(traces_for_cases).


-export([ case_start/2, case_stop/2, case_immediate_stop/2,
		  case_stop_on_shell/2 ]).


-define( trace_emitter_categorization, "case.life-cycle" ).


% For case_info_fmt and al:
-include("traces_case_header.hrl").


% For TraceType:
-include("traces.hrl").


-include("class_TraceSupervisor.hrl").


-include("traces_case_footer.hrl").




% To be called from the counterpart macro.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through "erl -eval" (like our cases) or through "erl -run" will be executed in
% a process which will silently trap EXIT events, which would mean that the
% crash of any process created from the case, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the case process).
%
% Returns TraceAggregatorPid.
%
-spec case_start( basic_utils:module_name(), boolean() ) -> pid().
case_start( ModuleName, _InitTraceSupervisor=true ) ->

	% First jump to the other clause:
	TraceAggregatorPid = case_start( ModuleName, false ),

	% For a simulation case, we do not consider to launch the trace supervisor
	% this early, as we would need to change its trace filename, which is not
	% supported on some back-ends (ex: LogMX).

	?case_info_fmt( "Starting case ~s.", [ ModuleName ] ),

	TraceAggregatorPid;


case_start( ModuleName, _InitTraceSupervisor=false ) ->

	% See comments above about:
	erlang:process_flag( trap_exit, false ),

	% Create first, synchronously (to avoid race conditions), a trace aggregator
	% (false is to specify a non-private i.e. global aggregator).
	%
	% Race conditions could occur at least with trace emitters (they would
	% create their own aggregator, should none by found) and with trace
	% supervisor (which expects a trace file to be already created at start-up).
	%
	% Goes back to the beginning of line:
	%
	io:format( "~n" ),

	CaseIsBatch = executable_utils:is_batch(),

	TraceFilename = traces:get_trace_filename( ModuleName ),

	TraceAggregatorPid = class_TraceAggregator:synchronous_new_link(
		TraceFilename, ?TraceType, ?TraceTitle, _TraceIsPrivate=false,
		CaseIsBatch ),

	?case_info_fmt( "Starting case ~s.", [ ModuleName ] ),

	TraceAggregatorPid.




% To be called from the counterpart macro.
-spec case_stop( basic_utils:module_name(), pid() ) -> no_return().
case_stop( ModuleName, TraceAggregatorPid ) ->

	class_TraceSupervisor:wait_for(),

	% Stop trace sent there:
	case_immediate_stop( ModuleName, TraceAggregatorPid ).



% To be called from the counterpart macro.
-spec case_immediate_stop( basic_utils:module_name(), pid() ) -> no_return().
case_immediate_stop( ModuleName, TraceAggregatorPid ) ->

	case_stop_on_shell(  ModuleName, TraceAggregatorPid ),

	case_facilities:finished().



% To be called from the counterpart macro.
-spec case_stop_on_shell( basic_utils:module_name(), pid() ) -> no_return().
case_stop_on_shell( ModuleName, TraceAggregatorPid ) ->

	?case_info_fmt( "Stopping case ~s.", [ ModuleName ] ),

	% Variable shared through macro use:
	TraceAggregatorPid ! { synchronous_delete, self() },

	receive

		{ deleted, TraceAggregatorPid } ->
			ok

	end,

	traces:check_pending_wooper_results(),

	class_TraceAggregator:remove(),

	case_facilities:display( "End of case ~s", [ ModuleName ] ).
