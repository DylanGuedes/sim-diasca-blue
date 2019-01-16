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



% Module storing all the helper functions easing the support of the Java binding
% API.
%
-module(java_binding_utils).


% Exports of helpers:
-export([ execute_request/4, execute_request_locally/3 ]).


% For trace generations (involving a WOOPER state):
-include("class_TraceEmitter.hrl").




% For trace notifications (stand-alone):
-include("traces.hrl").



% Implementation notes:
%
% Based on java_utils.


-type title() ::  atom().

-type body() :: [ any() ].

-type result() :: any().



% Sends a request to a Java OtpMailbox, by sending a message decomposed as a
% title and a body.
%
% Note: trace messages received while this request is processed are managed as
% well.
%
-spec execute_request( java_utils:java_mbox_pid(), title(), body(),
		traces:emitter_categorization() | wooper:state() ) -> result().
execute_request( MailboxPid, MessageTitle, MessageBody, TraceCat )
  when is_list( TraceCat ) ->
	java_utils:send_oneway( MailboxPid, MessageTitle, MessageBody ),
	handle_request_results( MailboxPid, MessageTitle, TraceCat );

execute_request( MailboxPid, MessageTitle, MessageBody, State ) ->
	java_utils:send_oneway( MailboxPid, MessageTitle, MessageBody ),
	handle_request_results( MailboxPid, MessageTitle, State ).



% Executes the same kind of request as the method above, but in any locally
% available Java Mailbox.
%
% Hence this function is only relevant for requests that do not rely on the
% state of a particular Jinterface Mailbox (ex: static methods only).
%
-spec execute_request_locally( title(), body(),
			traces:emitter_categorization() | wooper:state() ) -> result().
execute_request_locally( MessageTitle, MessageBody, TraceCatOrState ) ->

	% Gets the PID of the global instance managing Java resources:
	JavaManagerPid = class_JavaBindingManager:get_registered_manager(),

	% Selects one of the active local mailboxes:
	MailboxPid = class_JavaBindingManager:get_mailbox( JavaManagerPid ),

	% Executes a classical request in the selected interpreter:
	execute_request( MailboxPid, MessageTitle, MessageBody, TraceCatOrState ).



% Recursive listener transmitting trace messages sent from Java, to be used
% while performing a request to a Java mailbox in order to wait for its
% corresponding answer.
%
% Stops as soon as the request is successfully completed, or an error message is
% received, or an exception has been raised by the process hosting the mailbox.
%
-spec handle_request_results( java_utils:java_mbox_pid(), title(),
				traces:emitter_categorization() | wooper:state() ) -> result().
handle_request_results( MailboxPid, MessageTitle, TraceEmitterCategorization )
  when is_list( TraceEmitterCategorization ) ->

	case java_utils:wait_for_request_result( MailboxPid, MessageTitle ) of

		{ request_completed, ReceivedData } ->
			ReceivedData;

		{ trace_emitted, debug, TraceFormattedMessage } ->
			?notify_debug_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			handle_request_results( MailboxPid, MessageTitle,
									TraceEmitterCategorization );

		{ trace_emitted, trace, TraceFormattedMessage } ->
			?notify_trace_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			handle_request_results( MailboxPid, MessageTitle,
									TraceEmitterCategorization );

		{ trace_emitted, info, TraceFormattedMessage } ->
			?notify_info_cat( TraceFormattedMessage,
							  TraceEmitterCategorization ),
			handle_request_results( MailboxPid, MessageTitle,
									TraceEmitterCategorization );

		{ trace_emitted, warning, TraceFormattedMessage } ->
			?notify_warning_cat( TraceFormattedMessage,
								 TraceEmitterCategorization ),
			handle_request_results( MailboxPid, MessageTitle,
									TraceEmitterCategorization );

		{ trace_emitted, error, TraceFormattedMessage } ->
			?notify_error_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_error_raised, TraceFormattedMessage } );

		{ trace_emitted, fatal, TraceFormattedMessage } ->
			?notify_fatal_cat( TraceFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_fatal_error_raised, TraceFormattedMessage } );

		{ trace_emitted, OtherTraceType, TraceFormattedMessage } ->
			?notify_warning_fmt_cat(
			   "Invalid trace received from Java: the trace type '~p' is not "
			   "known; the original trace message is:~n~n'~s'.",
			   [ OtherTraceType, TraceFormattedMessage ],
			   TraceEmitterCategorization ),
			handle_request_results( MailboxPid, MessageTitle,
									TraceEmitterCategorization );

		{ exception_raised, ExceptionType, ExceptionFormattedMessage } ->
			?notify_error_cat( ExceptionFormattedMessage,
							   TraceEmitterCategorization ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_exception_raised, ExceptionType } )

	end;

% Here the third element is a state, not a TraceEmitterCategorization:
%
handle_request_results( MailboxPid, MessageTitle, State ) ->

	case java_utils:wait_for_request_result( MailboxPid, MessageTitle ) of

		{ request_completed, ReceivedData } ->
			ReceivedData;

		{ trace_emitted, debug, TraceFormattedMessage } ->
			?debug( TraceFormattedMessage ),
			handle_request_results( MailboxPid, MessageTitle, State );

		{ trace_emitted, trace, TraceFormattedMessage } ->
			?trace( TraceFormattedMessage ),
			handle_request_results( MailboxPid, MessageTitle, State );

		{ trace_emitted, info, TraceFormattedMessage } ->
			?info( TraceFormattedMessage ),
			handle_request_results( MailboxPid, MessageTitle, State );

		{ trace_emitted, warning, TraceFormattedMessage } ->
			?warning( TraceFormattedMessage ),
			handle_request_results( MailboxPid, MessageTitle, State );

		{ trace_emitted, error, TraceFormattedMessage } ->
			?error( TraceFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_error, TraceFormattedMessage } );

		{ trace_emitted, fatal, TraceFormattedMessage } ->
			?fatal( TraceFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_error_raised, TraceFormattedMessage } );

		{ trace_emitted, OtherTraceType, TraceFormattedMessage } ->
			?warning_fmt( "Invalid trace received from Java: the trace type "
						  "'~p' is not known; the original trace message is:"
						  "~n~n'~s'.",
						  [ OtherTraceType, TraceFormattedMessage ] ),
			handle_request_results( MailboxPid, MessageTitle, State );

		{ exception_raised, ExceptionType, ExceptionFormattedMessage } ->
			?error( ExceptionFormattedMessage ),
			class_JavaBindingManager:get_registered_manager() ! delete,
			throw( { java_exception_raised, ExceptionType } )

	end.
