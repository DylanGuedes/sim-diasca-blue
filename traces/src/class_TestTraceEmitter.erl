% Copyright (C) 2003-2017 Olivier Boudeville
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


% Test of TraceEmitter class.
%
% See class_TraceEmitter.hrl and class_TraceEmitter.erl.
%
-module(class_TestTraceEmitter).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ]).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, TraceEmitterName ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, destruct/1 ).



% Member method declarations.
-define( wooper_method_export, sendTraces/1, sendAsyncTraces/1 ).

-export([ send_traces/1, send_fatal_trace/1, send_debug_trace/1,
		  send_traces_benchmark/1 ]).


% Used by the trace_categorize/1 macro to use the right emitter:
%-define( trace_emitter_categorization, "TraceEmitter.Test" ).

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Only to test the trace system:
-define(LogPrefix,"[Test TraceEmitter]").


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").



% Constructs a new test trace emitter.
%
-spec construct( wooper:state(), class_TraceEmitter:emitter_init() ) ->
					   wooper:state().
construct( State, TraceEmitterName ) ->

	io:format( "~s Creating a new test trace emitter, whose name is ~p, "
			   "whose PID is ~w.~n", [ ?LogPrefix, TraceEmitterName, self() ] ),

	% First the direct mother classes, then this class-specific actions:
	TraceState = class_TraceEmitter:construct( State,
									   ?trace_categorize( TraceEmitterName ) ),

	% From now on, traces can be sent (but from the constructor send_* traces
	% only should be sent, to be able to refer to a trace-enabled state):
	?send_fatal(   TraceState, "Hello fatal world!"   ),
	?send_error(   TraceState, "Hello error world!"   ),
	?send_warning( TraceState, "Hello warning world!" ),
	?send_info(    TraceState, "Hello info world!"    ),
	?send_trace(   TraceState, "Hello trace world!"   ),
	?send_debug(   TraceState, "Hello debug world!"   ),
	?send_void(    TraceState, "Hello void world!"    ),

	TraceState.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	io:format( "~s Deleting test trace emitter ~s.~n",
			   [ ?LogPrefix, ?getAttr(name) ] ),

	% Last moment to send traces:
	?fatal(   "Goodbye fatal world!"   ),
	?error(   "Goodbye error world!"   ),
	?warning( "Goodbye warning world!" ),
	?info(    "Goodbye info world!"    ),
	?trace(   "Goodbye trace world!"   ),
	?debug(   "Goodbye debug world!"   ),
	?void(    "Goodbye void world!"   ),

	io:format( "~s Test trace emitter ~s deleted.~n",
			   [ ?LogPrefix, ?getAttr(name) ] ),

	% Allows chaining:
	State.




% Methods section.


% (const request)
-spec sendTraces( wooper:state() ) -> request_return( 'ok' ).
sendTraces( State ) ->

	%io:format( "~s Sending some traces.~n", [ ?LogPrefix ] ),
	%send_traces(State),
	send_traces_benchmark( State ),
	?wooper_return_state_result( State, ok ).



% (const oneway)
-spec sendAsyncTraces( wooper:state() ) -> oneway_return().
sendAsyncTraces( State ) ->

	%io:format( "~s Sending some asynchronous traces.~n", [ ?LogPrefix ] ),
	%send_traces(State),
	send_traces_benchmark( State ),
	?wooper_return_state_only( State ).




% Helper functions.


% We should be testing all forms of traces here.
-spec send_traces( wooper:state() ) -> basic_utils:void().
send_traces( State ) ->

	%io:format( "~s Sending some traces.~n", [ ?LogPrefix ] ),

	% We finally replaced fatal and error traces by warning, as the former two
	% induce waitings (i.e. timer:sleep/1 calls):


	% With no formatting:

	?fatal(   "Still livin' in a fatal world! (plain)"   ),
	?error(   "Still livin' in a error world! (plain)"   ),
	?warning( "Still livin' in a warning world! (plain)" ),
	?info(    "Still livin' in a info world! (plain)"    ),
	?trace(   "Still livin' in a trace world! (plain)"   ),
	?debug(   "Still livin' in a debug world! (plain)"   ),
	?void(    "Still livin' in a void world! (plain)"   ),



	?fatal_cat(   "Still livin' in a fatal world! (cat)", ?application_start ),

	?error_cat(   "Still livin' in a error world! (cat)", ?application_save ),

	?warning_cat( "Still livin' in a warning world! (cat)", ?time ),

	?info_cat(    "Still livin' in a info world! (cat)",  ?execution ),

	?trace_cat(   "Still livin' in a trace world! (cat)", ?application_start ),

	?debug_cat(   "Still livin' in a debug world! (cat)", ?application_start ),

	?void_cat(   "Still livin' in a void world! (cat)", ?application_start ),



	?fatal_full(   "Still livin' in a fatal world! (full)",
				   ?application_start, 5 ),

	?error_full(   "Still livin' in a error world! (full)",
				   ?application_save, 6 ),

	?warning_full( "Still livin' in a warning world! (full)",
				   ?time, 7 ),

	?info_full(    "Still livin' in a info world! (full)",
				   ?execution, 8 ),

	?trace_full(   "Still livin' in a trace world! (full)",
				   ?application_start, 9 ),

	?debug_full(   "Still livin' in a debug world! (full)",
				   ?application_start, 10 ),

	?void_full(   "Still livin' in a void world! (full)",
				   ?application_start, 11 ),



	% With formatting:

	?fatal_fmt(   "Yes, still livin' in a ~w world! (plain)", [fatal]   ),
	?error_fmt(   "Yes, still livin' in a ~w world! (plain)", [error]   ),
	?warning_fmt( "Yes, still livin' in a ~w world! (plain)", [warning] ),
	?info_fmt(    "Yes, still livin' in a ~w world! (plain)", [info]    ),
	?trace_fmt(   "Yes, still livin' in a ~w world! (plain)", [trace]   ),
	?debug_fmt(   "Yes, still livin' in a ~w world! (plain)", [debug]   ),
	?void_fmt(    "Yes, still livin' in a ~w world! (plain)", [void]   ),


	?fatal_fmt_cat(    "Ouh-ouh-ouuuuuh ~w", [fatal],   ?application_start ),
	?error_fmt_cat(    "Ouh-ouh-ouuuuuh ~w", [error],   ?application_save  ),
	?warning_fmt_cat(  "Ouh-ouh-ouuuuuh ~w", [warning], ?time              ),
	?info_fmt_cat(     "Ouh-ouh-ouuuuuh ~w", [info],    ?execution         ),
	?trace_fmt_cat(    "Ouh-ouh-ouuuuuh ~w", [trace],   ?application_start ),
	?debug_fmt_cat(    "Ouh-ouh-ouuuuuh ~w", [debug],   ?application_start ),
	?void_fmt_cat(     "Ouh-ouh-ouuuuuh ~w", [void],    ?application_start ),


	?fatal_fmt_full(   "Oh yeah ~w", [fatal],   ?application_start,  5 ),
	?error_fmt_full(   "Oh yeah ~w", [error],   ?application_save,   6 ),
	?warning_fmt_full( "Oh yeah ~w", [warning], ?time,               7 ),
	?info_fmt_full(    "Oh yeah ~w", [info],    ?execution,          8 ),
	?trace_fmt_full(   "Oh yeah ~w", [trace],   ?application_start,  9 ),
	?debug_fmt_full(   "Oh yeah ~w", [debug],   ?application_start, 10 ),
	?void_fmt_full(    "Oh yeah ~w", [void],    ?application_start, 11 ).



% To test compilation problems when only one non-maskable trace is used (ex:
% variable unused, or term constructed whereas not used either).
%
-spec send_fatal_trace( wooper:state() ) -> basic_utils:void().
send_fatal_trace( State ) ->

	Message = "Unique ~w trace!",
	Format = [ fatal ],
	Categ = ?application_start,

	?fatal_fmt_cat( Message, Format, Categ ).



% To test compilation problems when only one maskable trace is used (ex:
% variable unused, or term constructed whereas not used either).
%
-spec send_debug_trace( wooper:state() ) -> basic_utils:void().
send_debug_trace( State ) ->

	Message = "Unique ~w trace!",
	Format = [ debug ],
	Categ = ?application_start,

	?debug_fmt_cat( Message, Format, Categ ).



% Fatal and error messages replaced by warning, as the former two induce sleeps,
% which distorts the benchmarks.
%
-spec send_traces_benchmark( wooper:state() ) -> basic_utils:void().
send_traces_benchmark( State ) ->

	%io:format( "~s Sending some traces.~n", [ ?LogPrefix ] ),

	% We finally replaced fatal and error traces by warning, as the former two
	% induce waitings (i.e. timer:sleep/1 calls):

	% With no formatting:

	?warning( "Still livin' in a fatal world! (plain)"   ),
	?warning( "Still livin' in a error world! (plain)"   ),
	?warning( "Still livin' in a warning world! (plain)" ),
	?info(    "Still livin' in a info world! (plain)"    ),
	?trace(   "Still livin' in a trace world! (plain)"   ),
	?debug(   "Still livin' in a debug world! (plain)"   ),
	?void(    "Still livin' in a void world! (plain)"   ),


	?fatal_cat( "Still livin' in a fatal world! (cat)",
				?application_start ),

	?error_cat( "Still livin' in a error world! (cat)",
				?application_save ),

	?warning_cat( "Still livin' in a warning world! (cat)",
				  ?time ),

	?info_cat( "Still livin' in a info world! (cat)",
			   ?execution ),

	?trace_cat( "Still livin' in a trace world! (cat)",
				?application_start ),

	?debug_cat( "Still livin' in a debug world! (cat)",
				?application_start ),

	?void_cat( "Still livin' in a void world! (cat)",
			   ?application_start ),



	?fatal_full( "Still livin' in a fatal world! (full)",
				 ?application_start, 5 ),

	?error_full( "Still livin' in a error world! (full)",
				 ?application_save, 6 ),

	?warning_full( "Still livin' in a warning world! (full)",
				   ?time, 7 ),

	% Useful also to test non-integer timestamps (works correctly with the trace
	% supervisors as they are):
	%
	?info_full( "Still livin' in a info world! (full)",
				?execution, {8,2} ),

	?trace_full( "Still livin' in a trace world! (full)",
				 ?application_start, 9 ),

	?debug_full( "Still livin' in a debug world! (full)",
				 ?application_start, 10 ),

	?void_full( "Still livin' in a void world! (full)",
				?application_start, 11 ),



	% With formatting:

	?fatal_fmt(   "Yes, still livin' in a ~w world! (plain)", [fatal]   ),
	?error_fmt(   "Yes, still livin' in a ~w world! (plain)", [error]   ),
	?warning_fmt( "Yes, still livin' in a ~w world! (plain)", [warning] ),
	?info_fmt(    "Yes, still livin' in a ~w world! (plain)", [info]    ),
	?trace_fmt(   "Yes, still livin' in a ~w world! (plain)", [trace]   ),
	?debug_fmt(   "Yes, still livin' in a ~w world! (plain)", [debug]   ),
	?void_fmt(    "Yes, still livin' in a ~w world! (plain)", [void]    ),



	?fatal_fmt_cat(   "Ouh-ouh-ouuuuuh ~w", [fatal],   ?application_start ),
	?error_fmt_cat(   "Ouh-ouh-ouuuuuh ~w", [error],   ?application_save  ),
	?warning_fmt_cat( "Ouh-ouh-ouuuuuh ~w", [warning], ?time              ),
	?info_fmt_cat(    "Ouh-ouh-ouuuuuh ~w", [info],    ?execution         ),
	?trace_fmt_cat(   "Ouh-ouh-ouuuuuh ~w", [trace],   ?application_start ),
	?debug_fmt_cat(   "Ouh-ouh-ouuuuuh ~w", [debug],   ?application_start ),
	?void_fmt_cat(    "Ouh-ouh-ouuuuuh ~w", [void],    ?application_start ),


	?fatal_fmt_full(   "Oh yeah ~w", [fatal],   ?application_start,  5 ),
	?error_fmt_full(   "Oh yeah ~w", [error],   ?application_save,   6 ),
	?warning_fmt_full( "Oh yeah ~w", [warning], ?time,               7 ),
	?info_fmt_full(    "Oh yeah ~w", [info],    ?execution,          8 ),
	?trace_fmt_full(   "Oh yeah ~w", [trace],   ?application_start,  9 ),
	?debug_fmt_full(   "Oh yeah ~w", [debug],   ?application_start, 10 ),
	?void_fmt_full(    "Oh yeah ~w", [void],    ?application_start, 11 ).
