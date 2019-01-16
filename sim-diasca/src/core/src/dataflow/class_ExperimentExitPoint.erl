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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)



% The experiment exit point is a singleton instance in charge of being the
% (logical) stopping point that terminates the evaluation of the registered
% dataflows, possibly at each timestep; technically it is run first
% (spontaneously), and once done triggers the experiment entry point.
%
-module(class_ExperimentExitPoint).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, Dataflows,
		 ExperimentEntryPointPid,
		 ExperimentManagerPid, WorldManagerPid ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2, actSpontaneous/1,
		 declareExperimentTermination/1 ).



% Design notes:
%
% The Dataflow Exit Point knows the various dataflows involved, but this may not
% be useful (and thus may be removed in the future).
%
% Please refer to the 'Dataflow Exit & Exit Points' and 'Scheduling Cycle of
% Experiments' sections of the Dataflow HOWTO in order to understand why this
% actor is spontaneously scheduled and triggers its ExperimentEntryPoint
% counterpart component.




% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Experiment.ExitPoint" ).

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For world_manager_pid() and all:
-include("dataflow_defines.hrl").



% Implementation notes:
%
% Currently the only termination criterion is reaching the final step. At this
% positional parameter other criteria may be supported in the future.


% Attributes that are specific to the experiment exit point are:
%
% - entry_point_pid :: experiment_entry_point_pid() is the PID of the entry
% point that this exit point will trigger
%
% - experiment_manager_pid :: experiment_manager_pid() is the PID of the
% experiment manager
%
% - world_manager_pid :: world_manager_pid() is the PID of the world manager
%
% - dataflows :: [ dataflow_pid() ] is a list of the dataflow instances known by
% this experiment manager
%
% - current_step :: class_ExperimentManager:step_count() is the current step at
% which the experiment is
%
% - max_step :: class_ExperimentManager:step_count() is the maximum step
% that the experiment may reach
%
% - phase :: class_ExperimentManager:phase() tells at which step of
% its behaviour this exit point is



% Constructs the experiment exit point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this exit point should drive
%
% - ExperimentEntryPointPid is the PID of the entry point of the experiment
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 [ dataflow_pid() ], 
				 experiment_entry_point_pid(), experiment_manager_pid(),
				 world_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, Dataflows, ExperimentEntryPointPid,
		   ExperimentManagerPid, WorldManagerPid ) ->

	
	% Auto-subscribing:
	RegistrationMessage = { registerExperimentExitPoint, [], self() },

	ExperimentManagerPid ! RegistrationMessage,
	WorldManagerPid ! RegistrationMessage,

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
								?trace_categorize( "ExperimentExitPoint" ) ),

	?send_debug_fmt( ActorState, "Constructing an Experiment Exit Point", [] ),

	% Then the class-specific actions:
	FinalState = setAttributes( ActorState, [
				{ entry_point_pid, ExperimentEntryPointPid },
				{ experiment_manager_pid, ExperimentManagerPid },
				{ world_manager_pid, WorldManagerPid },
				{ dataflows, Dataflows },
				{ phase, initialisation }
				] ),

	?send_info_fmt( ActorState, "Registering the Experiment Exit Point", [] ),

	% From both registerExperimentExitPoint requests:
	wooper:wait_for_request_acknowledgements( _Count=2,
				_AckAtom=experiment_exit_point_registered ),

	?send_info_fmt( ActorState, "Finished the initialisation of an Experiment Exit Point", [] ),

	FinalState.



% Methods section.



% Callback executed on the first diasca of existence of this manager.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created ~s.", [ to_string( State ) ] ),

	% Start from this very first diasca:
	ActState = executeOneway( State, actSpontaneous ),

	?wooper_return_state_only( ActState ).



% The core of the behaviour of this exit point.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	Phase = ?getAttr(phase),


	?debug_fmt( "Acting spontaneously, in ~s phase, "
				"on behalf of (any) previous one.",
				[ Phase ] ),

	FinalState = case Phase of

		initialisation ->
			EntryPointPid = ?getAttr(entry_point_pid),

			?info_fmt( "Initialisation over, performing first trigger "
					   "of entry point ~p.", [ EntryPointPid ] ),

			SentState = class_Actor:send_actor_message( EntryPointPid,
							  startExperimentTick, State ),

			NextState = executeOneway( SentState,
									   scheduleNextSpontaneousTick ),

			setAttribute( NextState, phase, simulation);

		simulation ->
			EntryPointPid = ?getAttr(entry_point_pid),

			%?debug_fmt( "Normal trigger of entry point ~p.",
			%			[ EntryPointPid ] ),

			SentState = class_Actor:send_actor_message( EntryPointPid,
									 startExperimentTick, State ),

			NextState =executeOneway( SentState,
									  scheduleNextSpontaneousTick ),

			NextState;


		termination ->
			?trace( "Termination reached, no more trigger of entry "
					"point nor additional scheduling." ),
			State

	end,

	?wooper_return_state_only( FinalState ).



% Declares the termination of the experiment.
%
% Note: usually this is determined internally.
%
% (oneway)
%
-spec declareExperimentTermination( wooper:state() ) -> oneway_return().
declareExperimentTermination( State ) ->

	NewState = case ?getAttr(phase) of

		termination ->
			throw( already_terminated);

		_ ->
			?trace( "Experiment terminating now." ),
			setAttribute( State, phase, termination )

	end,

	?wooper_return_state_only( NewState ).





% Helper functions.


% Returns a textual description of this exit point.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	DataflowString = case ?getAttr(dataflows) of

		[] ->
			"not referencing any dataflow";

		[ Dataflow ] ->
			text_utils:format( "referencing a single dataflow instance: ~p",
							   [ Dataflow ] );

		Dataflows ->
			text_utils:format( "referencing ~B dataflow instances: ~p",
							   [ length( Dataflows ), Dataflows ] )

	end,

	text_utils:format( "Experiment exit point in ~s phase "
					   "referencing its entry point counterpart ~p, "
					   "associated to the experiment manager ~w, to the "
					   "world manager ~w and ~s",
					   [ ?getAttr(phase), ?getAttr(entry_point_pid),
						 ?getAttr(experiment_manager_pid),
						 ?getAttr(world_manager_pid), DataflowString ] ).
