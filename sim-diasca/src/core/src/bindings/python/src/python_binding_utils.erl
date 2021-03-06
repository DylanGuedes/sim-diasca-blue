% Copyright (C) 2016-2017 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Robin Huart (robin-externe.huart@edf.fr)



% Module storing all the helper functions easing the support of the Python
% binding API.
%
-module(python_binding_utils).



% Exports of helpers:
-export([ execute_request/4, execute_request_locally/3 ]).



% For trace generations (involving a WOOPER state):
-include("class_TraceEmitter.hrl").


% For trace notifications (stand-alone):
-include("traces.hrl").



% Implementation notes:
%
% Based on python_utils.


-type title() ::  atom().

-type body() :: [ any() ].

-type result() :: any().



% Executes a request in a Python interpreter, by sending a message decomposed as
% a title and a body.
%
% Note: trace messages received while this request is processed are managed as
% well.
%
-spec execute_request( python_utils:interpreter_pid(), title(), body(),
		traces:emitter_info() | wooper:state() ) -> result().
execute_request( InterpreterPid, MessageTitle, MessageBody,
				 TraceInfoOrState ) ->
	python_utils:send_oneway( InterpreterPid, MessageTitle, MessageBody ),
	handle_request_results( InterpreterPid, MessageTitle,
							TraceInfoOrState ).



% Executes the same kind of request as the method above, but in any (local)
% available Python interpreter.
%
% Hence this function is only relevant for requests that do not rely on the
% state of a particular interpreter (ex: static methods only).
%
-spec execute_request_locally( title(), body(),
			traces:emitter_info() | wooper:state() ) -> result().
execute_request_locally( MessageTitle, MessageBody, TraceInfoOrState ) ->

	% Gets the PID of the global instance managing Python interpreters:
	PythonManager = class_PythonBindingManager:get_registered_manager(),

	% Selects one of the active interpreters:
	InterpreterPid = class_PythonBindingManager:get_interpreter(
					   PythonManager ),

	% Executes a classical request in the selected interpreter:
	execute_request( InterpreterPid, MessageTitle, MessageBody,
					 TraceInfoOrState ).



% Recursive listener transmitting trace messages sent from Python, to be used
% while performing a request to a Python interpreter in order to wait for its
% corresponding answer.
%
% Stops as soon as the request is successfully completed, or an error message is
% received, or an exception has been raised in the interpreter.
%
-spec handle_request_results( python_utils:interpreter_pid(), title(),
					  traces:emitter_info() | wooper:state() ) -> result().
handle_request_results( InterpreterPid, MessageTitle,
		TraceEmitterInfo={ TraceEmitterName, TraceEmitterCategorization } ) ->

	case python_utils:wait_for_request_result( InterpreterPid,
											   MessageTitle ) of


		{ request_completed, ReceivedData } ->
			ReceivedData;


		{ trace_emitted, debug, TraceFormattedMessage } ->

			?notify_debug_named( TraceFormattedMessage, TraceEmitterName,
								 TraceEmitterCategorization ),

			handle_request_results( InterpreterPid, MessageTitle,
									TraceEmitterInfo );


		{ trace_emitted, trace, TraceFormattedMessage } ->

			?notify_trace_named( TraceFormattedMessage, TraceEmitterName,
								 TraceEmitterCategorization ),

			handle_request_results( InterpreterPid, MessageTitle,
									TraceEmitterInfo );


		{ trace_emitted, info, TraceFormattedMessage } ->

			?notify_info_named( TraceFormattedMessage, TraceEmitterName,
								TraceEmitterCategorization ),

			handle_request_results( InterpreterPid, MessageTitle,
									TraceEmitterInfo );


		{ trace_emitted, warning, TraceFormattedMessage } ->

			?notify_warning_named( TraceFormattedMessage, TraceEmitterName,
								   TraceEmitterCategorization ),

			handle_request_results( InterpreterPid, MessageTitle,
									TraceEmitterInfo );


		{ trace_emitted, error, TraceFormattedMessage } ->

			?notify_error_named( TraceFormattedMessage, TraceEmitterName,
								 TraceEmitterCategorization ),

			class_PythonBindingManager:get_registered_manager() ! delete,

			throw( { python_error_raised, TraceFormattedMessage } );


		{ trace_emitted, fatal, TraceFormattedMessage } ->

			?notify_fatal_named( TraceFormattedMessage, TraceEmitterName,
								 TraceEmitterCategorization ),

			class_PythonBindingManager:get_registered_manager() ! delete,

			throw( { python_fatal_error_raised, TraceFormattedMessage } );


		{ trace_emitted, OtherTraceType, TraceFormattedMessage } ->

			Message = text_utils:format( "Invalid trace received from Python: "
										 "the trace type '~p' is not known; "
										 "the original trace message is:"
										 "~n~n'~s'.", [ OtherTraceType,
													TraceFormattedMessage ] ),

			?notify_warning_named( Message, TraceEmitterName,
								   TraceEmitterCategorization ),

			handle_request_results( InterpreterPid, MessageTitle,
									TraceEmitterInfo );


		{ exception_raised, ExceptionType, ExceptionFormattedMessage } ->

			?notify_error_named( ExceptionFormattedMessage, TraceEmitterName,
								 TraceEmitterCategorization ),

			class_PythonBindingManager:get_registered_manager() ! delete,

			throw( { python_exception_raised, ExceptionType } )

	end;


% Here the third element is a state, not a traces:emitter_info():
%
handle_request_results( InterpreterPid, MessageTitle, State ) ->

	case python_utils:wait_for_request_result( InterpreterPid,
											   MessageTitle ) of


		{ request_completed, ReceivedData } ->
			ReceivedData;


		{ trace_emitted, debug, TraceFormattedMessage } ->

			?debug( TraceFormattedMessage ),

			handle_request_results( InterpreterPid, MessageTitle, State );


		{ trace_emitted, trace, TraceFormattedMessage } ->

			?trace( TraceFormattedMessage ),

			handle_request_results( InterpreterPid, MessageTitle, State );


		{ trace_emitted, info, TraceFormattedMessage } ->

			?info( TraceFormattedMessage ),

			handle_request_results( InterpreterPid, MessageTitle, State );


		{ trace_emitted, warning, TraceFormattedMessage } ->

			?warning( TraceFormattedMessage ),

			handle_request_results( InterpreterPid, MessageTitle, State );


		{ trace_emitted, error, TraceFormattedMessage } ->

			?error( TraceFormattedMessage ),

			class_PythonBindingManager:get_registered_manager() ! delete,

			throw( { python_error, TraceFormattedMessage } );


		{ trace_emitted, fatal, TraceFormattedMessage } ->

			?fatal( TraceFormattedMessage ),

			class_PythonBindingManager:get_registered_manager() ! delete,

			throw( { python_error_raised, TraceFormattedMessage } );


		{ trace_emitted, OtherTraceType, TraceFormattedMessage } ->

			?warning_fmt( "Invalid trace received from Python: the trace type "
						  "'~p' is not known; the original trace message is:"
						  "~n~n'~s'.",
						  [ OtherTraceType, TraceFormattedMessage ] ),

			handle_request_results( InterpreterPid, MessageTitle, State );


		{ exception_raised, ExceptionType, ExceptionFormattedMessage } ->

			?error( ExceptionFormattedMessage ),

			class_PythonBindingManager:get_registered_manager() ! delete,

			throw( { python_exception_raised, ExceptionType } )

	end.
