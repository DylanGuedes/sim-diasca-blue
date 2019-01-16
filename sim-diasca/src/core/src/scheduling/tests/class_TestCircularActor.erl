% Copyright (C) 2008-2017 EDF R&D

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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% Test of Actor class, regarding time management.
%
-module(class_TestCircularActor).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, ActorName, Message ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).



% Member method declarations.
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1, addPeer/2,
		 receiveMessage/4 ).


% Exported helpers:
-export([ output/2, output/3 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Actor.CircularTest" ).

% Allows to use macros for trace sending:
-include("sim_diasca_for_models.hrl").



% Constructs a new test actor.
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_Actor:name(), string() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( ActorName ) ),

	?send_info( ActorState, "Creating a new test circular actor." ),

	%io:format( "- creating a new class_TestCircularActor: PID: ~w, AAI: ~B, "
	%		   "seed: ~w~n",
	%		   [ self(), getAttribute( ActorState, actor_abstract_id ),
	%			 getAttribute( ActorState, random_seed ) ] ),

	%ReverseInitialCreation = true,
	ReverseInitialCreation = false,

	case ReverseInitialCreation of

		true ->
			_TestActorPid = class_Actor:create_initial_actor( class_TestActor,
				[ _Rev="Reverse test actor",
				  _FirstSchedulingSettings={ erratic, 3 },
				  _FirstCreationSettings=no_creation,
				  _FirstTerminationTickOffset=107 ] );

		false ->
			ok

	end,

	setAttributes( ActorState, [
		{ message, Message },
		{ initialization_status, in_progress },
		%{ talkative, true },
		{ talkative, false } ] ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:
	?debug( "Test circular actor deleted." ),

	% Then allow chaining:
	State.




% Management section of the actor.



% This actor oneway is automatically called the next diasca after an actor is
% created or, if the simulation was not running, on diasca 1 (i.e. just after
% the spontaneous behaviours) of tick offset #0.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),
	?wooper_return_state_only( ScheduledState ).



% The core of the test actor behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	?info( "Test Circular Actor acting." ),

	output( "Test Circular Actor acting.", State ),

	ActedState = case ?getAttr(initialization_status) of

		completed ->
			say_something( State );

		_OtherStatus ->
			State

	end,

	NextScheduleOffset = class_Actor:get_current_tick_offset( ActedState ) + 10,

	PlanState = executeOneway( ActedState, addSpontaneousTick,
							   NextScheduleOffset ),

	?wooper_return_state_only( PlanState ).



% Adds specified peer to known peers.
%
% (actor oneway)
%
-spec addPeer( wooper:state(), pid() ) -> oneway_return().
addPeer( State, PeerPid ) ->

	?info_fmt( "Chaining to ~w.", [ PeerPid ] ),

	?wooper_return_state_only( setAttribute( State, peer, PeerPid ) ).



% Receives a hello message.
%
% (actor oneway)
%
-spec receiveMessage( wooper:state(), class_Actor:name(), string(), pid() ) ->
							oneway_return().
receiveMessage( State, SenderName, Message , SenderPid ) ->

	?info_fmt( "Received following message from ~s (~w): '~s', "
			   "using this message from now on.",
			   [ SenderName, SenderPid, Message ] ),

	?wooper_return_state_only( setAttribute( State, message, Message ) ).




% Section for helper functions (not methods).


% Says hello to all peers.
% Returns an updated state.
%
% (helper function)
%
say_something( State ) ->

	Peer = ?getAttr(peer),

	?info_fmt( "Sending '~s' to ~w.", [ ?getAttr(message), Peer ] ),

	case Peer of

		Peer when is_pid( Peer ) ->
			class_Actor:send_actor_message( Peer,
			 { receiveMessage, [ ?getAttr(name), ?getAttr(message) ] }, State );

		undefined ->
			State

	end.



% Outputs specified message in console, iff talkative.
%
% (helper)
%
output( Message, State ) ->

	case ?getAttr(talkative) of

		true ->
			TickOffset = class_Actor:get_current_tick_offset( State ),
			io:format( " [~s (~w) at ~p] " ++ Message ++ "~n",
					   [ ?getAttr(name), self(), TickOffset ] );

		false ->
			ok

	end.



% Outputs specified formatted message in console, iff talkative.
%
% (helper)
%
output( Format, Values, State ) ->
	Message = text_utils:format( Format, Values ),
	output( Message, State ).
