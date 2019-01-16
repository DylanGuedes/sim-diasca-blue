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



% The experiment entry point is a singleton instance in charge of being the
% (logical) starting point that impulses the evaluation of the registered
% dataflows, possibly at each timestep; technically it is run in second
% position, just after the experiment exit point that triggers it.
%
-module(class_ExperimentEntryPoint).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, Dataflows,
		 ExperimentManagerPid, WorldManagerPid ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2, startExperimentTick/2 ).



% Design notes:
%
% The Dataflow Entry Point knows the various dataflows involved, which is useful
% whenever having to create, update, delete, etc. dataflow blocks (which each
% pertains to a given dataflow).
%
% This (experiment) entry point knows the experiment manager (as it may have to
% perform experiment-level operations) but also the world manager (as it may
% typically impulse changes in the simulation world).


% Please refer to the 'Dataflow Entry & Exit Points' and 'Scheduling Cycle of
% Experiments' sections of the Dataflow HOWTO in order to understand why this
% actor is purely passive and (only) triggered by its ExperimentExitPoint
% counterpart component.



% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Experiment.EntryPoint" ).

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For types and shorthands:
-include("dataflow_defines.hrl").


% Implementation notes:
%


% Attributes that are specific to the experiment entry point are:
%
% - experiment_manager_pid :: experiment_manager_pid() is the PID of the
% experiment manager
%
% - world_manager_pid :: world_manager_pid() is the PID of the world manager
%
% - current_step :: class_ExperimentManager:step_count() is the current step at
% which the experiment is
%
% - max_step :: class_ExperimentManager:step_count() is the maximum step
% that the experiment may reach
%
% - dataflows :: [ dataflow_pid() ] is a list of the dataflow instances known by
% this experiment manager



% Constructs the experiment entry point, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Dataflows is a list of the dataflows that this entry point should drive
%
% - ExperimentStepStart is the step at which the experiment shall start
%
% - ExperimentStepStop is the step at which the experiment shall stop
%
% - ExperimentManagerPid is the PID of the experiment manager
%
% - WorldManagerPid is the PID of the world manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 [ dataflow_pid() ], 
				 experiment_manager_pid(), world_manager_pid() ) ->
					   wooper:state().
construct( State, ActorSettings, Dataflows,
		 ExperimentManagerPid, WorldManagerPid ) ->

	% Auto-subscribing:
	RegistrationMessage = { registerExperimentEntryPoint, [], self() },

	ExperimentManagerPid ! RegistrationMessage,
	WorldManagerPid ! RegistrationMessage,

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
								?trace_categorize( "ExperimentEntryPoint" ) ),

	% Then the class-specific actions:
	FinalState = setAttributes( ActorState, [
		{ experiment_manager_pid, ExperimentManagerPid },
		{ world_manager_pid, WorldManagerPid },
		{ dataflows, Dataflows } ] ),

	% From both registerExperimentEntryPoint requests:
	wooper:wait_for_request_acknowledgements( _Count=2,
				_AckAtom=experiment_entry_point_registered ),

	FinalState.



% Methods section.



% Callback executed on the first diasca of existence of this manager.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created ~s.", [ to_string( State ) ] ),

	?wooper_return_state_only( State ).



% Starts the evaluation of the experiment for the current tick.
%
% Typically called by the experiment exit point.
%
% Mostly empty implementation, meant to be overridden.

% (actor oneway)
%
-spec startExperimentTick( wooper:state(), actor_pid() ) ->
								 class_Actor:actor_oneway_return().
startExperimentTick( State, _SenderActorPid ) ->

	?warning_fmt( "Default implementation of startExperimentTick called, but it does nothing....",
				[ ] ),

	% This is an empty implementation.
	%
	% Actual ones may fetch information from any source (ex: thanks to a REST
	% call), and may update accordingly the corresponding dataflow elements
	% (typically dataflow actors), possibly directly or through the various
	% registered dataflows.
	%
	% Then corresponding blocks may be activated, and the dataflow evaluated.

	%SentState = class_Actor:send_actor_messages( ?getAttr(dataflows),
	%						{ startExperimentTick, [...], State },

	%?wooper_return_state_only( SentState ).

	?wooper_return_state_only( State ).




% Helper functions.


% Returns a textual description of this entry point.
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
			text_utils:format( "referencing ~B dataflow instances: ~w",
							   [ length( Dataflows ), Dataflows ] )

	end,

	text_utils:format( "Experiment entry point, associated to "
					   "the experiment manager ~w, to the world manager ~w, "
					   "and ~s",
					   [ ?getAttr(experiment_manager_pid),
						 ?getAttr(world_manager_pid), DataflowString ] ).
