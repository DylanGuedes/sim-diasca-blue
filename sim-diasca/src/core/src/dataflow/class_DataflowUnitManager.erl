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



% The dataflow unit manager is the abstract class from which each actual manager
% for a given set of types of dataflow units shall inherit.
%
% Indeed, if relevant, a given unit manager may take care of multiple types of
% units (ex: a urban manager may manage energy demand units, pollution units,
% etc.).
%
% For example, by design the Foo, Bar and Baz processing units may be
% interlinked, in which case a FooBarBazUnitManager class can be defined,
% inheriting from this DataflowUnitManager class and in charge of the life cycle
% and connectivity of the instances of these three kinds of units.
%
% Each unit manager is registered to the (parent, top-level) experiment manager.
%
% Each unit manager is a singleton and registers itself globally under its name,
% which is, conventionally, its actual classname (ex:
% 'class_EnergyDemandManager' or, if needing more clarity,
% 'class_EnergyDemandUnitManager').
%
% As a unit manager may have to create dataflow units at runtime (for the types
% of units it is in charge of), it must itself be a (simulation) actor.
%
-module(class_DataflowUnitManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, Name, ManagedUnitSpecs,
		 ListenedEventMatches, ExperimentManagerPid, BindingManagers,
		 LoadBalancerPid, IdentificationServerPid ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/8, new_link/8,
		 synchronous_new/8, synchronous_new_link/8,
		 synchronous_timed_new/8, synchronous_timed_new_link/8,
		 remote_new/9, remote_new_link/9, remote_synchronous_new/9,
		 remote_synchronous_new_link/9, remote_synchronisable_new_link/9,
		 remote_synchronous_timed_new/9, remote_synchronous_timed_new_link/9,
		 construct/9, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2,

		 % Actor oneway drivers, called by the experiment manager:
		 processAnyEventMatched/3,
		 processCreationEventMatched/3, processDestructionEventMatched/3,
		 processAssociationEventMatched/3,
		 processBinaryAssociationEventMatched/3,
		 processDisassociationEventMatched/3,
		 processConnectionEventMatched/3, processDisconnectionEventMatched/3,
		 processUpdateEventMatched/3,

		 % Oneway event matches, meant to be overridden:
		 onAnyEventMatched/2,
		 onCreationEventMatched/2, onDestructionEventMatched/2,
		 onAssociationEventMatched/2, onBinaryAssociationEventMatched/2,
		 onDisassociationEventMatched/2,
		 onConnectionEventMatched/2, onDisconnectionEventMatched/2,
		 onUpdateEventMatched/2,

		 % Unit creations:
		 createInitialUnitInstance/4, createInitialUnitInstances/4,
		 createInitialMockupUnitInstance/4, createInitialMockupUnitInstances/4,
		 createUnit/5, destructUnit/3,

		 connectToIteratedInitially/3,

		 onActorCreated/4, onUnitCreated/6,
		 onConnectionsCreated/5, onUnitDestructed/4 ).


% Exported helpers:
%
-export([ create_channels_for/5, create_output_ports/2,
		  event_clauses_to_string/1, event_clause_to_string/1,
		  connection_specs_to_string/1, connection_spec_to_string/1,
		  upstream_spec_to_string/1, downstream_spec_to_string/1 ]).



% Design notes:
%
% Dataflow units may be created statically (whereas the simulation is not
% started, i.e. as initial units, typically from the simulation case) or
% dynamically (while the simulation is running, i.e. as runtime units, typically
% from dataflow unit managers).
%
% At runtime, a unit manager will receive from the experiment manager the world
% events matching the event clauses this unit manager declared.
%
% The processing of each of these events (designated by its identifier) will be
% done based on a series of actions (each also designated by identifiers of
% their own).



% Helpers:
-export([ to_string/1 ]).


% Records all instances of a managed unit type:
-type unit_table() :: table:table( dataflow_unit_type(), [ unit_pid() ] ).



% Describes a type of processing unit that is to be managed by a given unit
% manager.
%
% It may be:
%
% - either directly the classname of that unit (ex:
% 'class_TransportationDemandUnit') - implying it is an Erlang-based unit
%
% - or a { Classname, ImplementationLanguage } pair, meaning said unit is
% defined in specified class, and implemented in specified programming language
% (ex: { 'class_VehicleTypeUnit', 'python' }, or { 'class_EnergyDemandUnit',
% 'erlang' }
%
-type managed_unit_spec() :: dataflow_unit_type() |
					 { dataflow_unit_type(), language_utils:language() }.



% Section about actions.


% The types of actions that a unit manager may track:
-type action_type() :: 'unit_creation' | 'unit_destruction'
					 | 'unit_connection'
					 | 'unit_disconnection'.


% Any contextual information about an action:
-type context() :: any().


% Allows a unit manager to record a pending action in the context of the
% processing of a given world event:
%
-type action() :: unit_creation_action()
				| unit_destruction_action()
				| unit_connection_action()
				| unit_disconnection_action().


% Action corresponding to the creation of a unit.
%
-type unit_creation_action() :: { 'unit_creation', dataflow_unit_type(),
	   wooper:construction_parameters(), event_id(), unit_creation_context() }.

% Typically the PID of an upstream dataflow object:
-type unit_creation_context() :: context(). % dataflow_object_pid()




% Action corresponding to the destruction of a unit.
%
-type unit_destruction_action() :: { 'unit_destruction', unit_pid(),
									 event_id() }.




% Information about the (upstream, outgoing) part of a connection:
%
% (if the kind of port is not specified, a standard port is assumed)
%
-type upstream_port_spec() ::
		output_port_string_name()
	  | { 'output_port_name', output_port_string_name() }
	  | { 'output_iteration_name', output_iteration_string_name() }.


% Canonical form of upstream_port_spec/0:
%
-type canonical_upstream_port_spec() ::
		{ 'output_port_name', output_port_name() }
	  | { 'output_iteration_name', output_iteration_name() }.



% Information about the (downstream, ingoing) part of a connection:
%
% (if the kind of port is not specified, a standard port is assumed)
%
-type downstream_port_spec() ::
		input_port_string_name()
	  | { 'input_port_name', input_port_string_name() }
	  | { 'input_iteration_name', input_iteration_string_name() }.



% Canonical form of downstream_port_spec/0:
%
-type canonical_downstream_port_spec() ::
		{ 'input_port_name', input_port_name() }
	  | { 'input_iteration_name', input_iteration_name() }.


% Allows to specify, regarding an upstream block and a downstream one, a
% connection between two ports that shall be made.
%
% Note: such connections are typically aggregated into lists.
%
% (if only a port name is specified, an identically named standard port in both
% ends is assumed)
%
-type connection_spec() :: { upstream_port_spec(), downstream_port_spec() }
						   | port_string_name().


% Canonical form of connection_spec/0:
%
-type canonical_connection_spec() :: { canonical_upstream_port_spec(),
									   canonical_downstream_port_spec() }.



% Action corresponding to the connection of a unit to the dataflow, i.e. the
% creation of a set of channels, from output ports to input ones, ports being
% standard or iterated ones.
%
-type unit_connection_action() :: { 'unit_connection', event_id(),
		upstream_block_pid(), downstream_block_pid(),
		[ canonical_connection_spec() ], unit_connection_context() }.


-type unit_connection_context() :: context(). % 'undefined'



% Action corresponding to the disconnection of a unit to the dataflow, i.e. the
% removal of a set of channels.
%
-type unit_disconnection_action() :: { 'unit_disconnection', event_id(),
	   block_pid(), unit_disconnection_context() }.

-type unit_disconnection_context() :: context(). % 'undefined'



% Allows to keep track of the actions performed by this unit manager:
%
-type action_id() :: basic_utils:count().


% Keeps track of the actions in progress:
-type action_table() :: table:table( action_id(), action() ).


% Allows to set action identifiers:
-type action_count() :: basic_utils:count().


% Records, for a given world event, all the pending actions still currently
% in progress regarding that unit manager:
%
-type event_table() :: table:table( event_id(), [ action_id() ] ).


-export_type([ unit_table/0, managed_unit_spec/0, action_type/0,
			   action/0, action_id/0, action_table/0, action_count/0,
			   event_table/0 ]).


% Shorthand:
-type connection_info() :: class_DataflowBlock:connection_info().


-define( wooper_static_method_export, create_managers/4, create_managers/5,
		 create_initial_unit/4, create_initial_units/4,
		 create_initial_mockup_unit/4, create_initial_mockup_units/4,
		 create_runtime_unit/4, connect_to_iterated_initially/3 ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.UnitManager" ).



% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For dataflow-related types and names:
-include("dataflow_defines.hrl").


% For all bindings-related types:
-include("bindings.hrl").



% Implementation notes:
%
% A unit manager is not linked to any dataflow (as there may exist multiple
% dataflow instances anyway).



% Attributes that are specific to a unit manager instance are:
%
% - unit_table :: unit_table() is a table associating to each supported type of
% units a list of the corresponding instances created by this unit manager
%
% - event_matches :: [ event_match() ] is a list describing the dataflow
% synchronization events that this unit manager is interested in
%
% - experiment_manager_pid :: experiment_manager_pid() is the PID of the
% experiment manager
%
% - binding_managers :: binding_managers() is a record storing the PIDs of all
% the binding managers corresponding to the activated language bindings; useful
% whenever an instance implemented in one of these languages has to be created
%
% - load_balancer_pid :: load_balancer_pid() is the PID of the load balancer,
% useful to create new units for example
%
% - identification_server_pid :: basic_utils:maybe( identification_server_pid()
% ) is the PID of the identification server, if enabled by the case
%
% - event_table :: event_table() records, for each world event (designated by
% its identifier) to be processed, the identifiers of the pending actions still
% currently in progress, as triggered by this unit manager (typically unit or
% channel waited creations or destructions); now that the processing of events
% is serialised (to avoid the pitfalls of event interleaving, i.e. some events
% may need that past ones are fully processed - typically a unit being fully
% created - for their own processing), this table is expected to be either empty
% or holding one event
%
% - action_table :: action_table() allows to keep track of all pending actions,
% by associating to an action identifier a full description to the corresponding
% action
%
% - action_count :: action_count() is the count of all the actions already
% declared (and also the identifier of the last allocated action)


% Constructs a unit manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - Name, an atom designating the classname of this singleton manager (ex:
% 'class_FooBarBazUnitManager')
%
% - ManagedUnitSpecs specifies the types of units that this manager is to take
% care of, together with their implementation language (default being Erlang)
%
% - ListenedEventMatches is a list of the dataflow synchronization events that
% this unit manager is interested in
%
% - ExperimentManagerPid is the PID of the parent manager of this one
%
% - BindingManagers, a record storing the PIDs of all the binding managers
% corresponding to the activated language bindings; useful whenever an instance
% implemented in one of these languages has to be created
%
% - LoadBalancerPid, the PID of the load balancer, useful when instances have to
% be created
%
% - IdentificationServerPid, the PID of the identification server (if any)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
		class_Actor:name(), [ managed_unit_spec() ], [ event_match() ],
		experiment_manager_pid(), binding_managers(), load_balancer_pid(),
		basic_utils:maybe( identification_server_pid() ) ) ->  wooper:state().
construct( State, ActorSettings, Name, ManagedUnitSpecs, ListenedEventMatches,
		   ExperimentManagerPid, BindingManagers, LoadBalancerPid,
		   IdentificationServerPid ) ->

	binding_utils:check_implementation_language( ManagedUnitSpecs,
												 BindingManagers ),

	ManagedUnitTypes = dataflow_binding_utils:get_unit_types(
						 ManagedUnitSpecs ),

	% Auto-subscribing, and declaring our own event matches (based on a
	% request):
	%
	ExperimentManagerPid ! { registerUnitManager,
					 [ ManagedUnitTypes, ListenedEventMatches ], self() },

	% We expect child classes to pass atom-based names:
	{ RegistrationName, TraceInit } = case Name of

		{ AtomName, TraceCateg } ->
			StringName = text_utils:atom_to_string( AtomName ),
			{ AtomName, { StringName, TraceCateg } };

		% Emitter categorization added later:
		AtomName ->
			StringName = text_utils:atom_to_string( AtomName ),
			{ AtomName, StringName }

	end,

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( TraceInit ) ),

	?send_trace_fmt( ActorState, "Linked to experiment manager ~w and defining "
					 "following ~s", [ ExperimentManagerPid,
						event_clauses_to_string( ListenedEventMatches ) ] ),

	% All unit managers register themselves that way:
	% (ensures uniqueness as well)
	%
	naming_utils:register_as( RegistrationName, global_only ),

	PreparedUnitTable = prepare_for_units( ManagedUnitSpecs, ActorState ),

	EmptyTable = table:new(),

	% Then the class-specific actions:
	FinalState = setAttributes( ActorState, [
		{ unit_table, PreparedUnitTable },
		{ event_matches, ListenedEventMatches },
		{ experiment_manager_pid, ExperimentManagerPid },
		{ binding_managers, BindingManagers },
		{ load_balancer_pid, LoadBalancerPid },
		{ identification_server_pid, IdentificationServerPid },
		{ event_table, EmptyTable },
		{ action_table, EmptyTable },

		% One may prefer starting counting the actions from an easily-spotted
		% offset (ex: to better discriminate actions from event identifiers):
		%
		%{ action_count, 100 } ] ),
		{ action_count, 0 } ] ),

	% Interleaving of registerUnitManager/2 is over:
	receive

		{ wooper_result, unit_manager_registered } ->
			ok

	end,

	FinalState.



% Prepares the management of the specified types of units.
%
% (helper)
%
-spec prepare_for_units( [ managed_unit_spec() ], wooper:state() ) ->
							   unit_table().
prepare_for_units( UnitSpecs, State ) ->

	% Gets the list of unit (Erlang) classnames:
	UnitTypes = dataflow_binding_utils:get_unit_types( UnitSpecs ),

	case class_DataflowBlock:declare_static_information_for( UnitSpecs ) of

		ok ->
			?info_fmt( "All semantics and types for units ~p successfully "
					   "declared statically.", [ UnitTypes ] );

		{ error, Reason } ->
			?error_fmt( "Static declaration of semantics and types failed for "
						"unit data ~p. Reason: ~p", [ UnitSpecs, Reason ] ),
			throw( { static_declaration_failed, UnitTypes, Reason } )

	end,

	% Initially all unit types know none of their instances:
	EmptyEntries = [ { Type, [] } || Type <- UnitTypes ],

	table:addEntries( EmptyEntries, table:new() ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	?trace_fmt( "Being deleted, while still ~s",
				[ unit_table_to_string( State ) ] ),

	State.





% Methods section.



% Callback executed on the first diasca of existence of this unit manager.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?trace_fmt( "Created a ~s", [ to_string( State ) ] ),

	?wooper_return_state_only( State ).




% Event processing section.



% Called (most probably by the experiment manager) to notify this unit manager
% (having requested to be notified of all events) that a world event has been
% received (and successfully matched).
%
% (actor oneway)
%
-spec processAnyEventMatched( wooper:state(), world_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processAnyEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onAnyEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a world event happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onAnyEventMatched( wooper:state(), world_event() ) -> oneway_return().
onAnyEventMatched( State, Event ) ->

	?warning_fmt( "Default onAnyEventMatched/2 implementation ignoring ~s.",
				  [ dataflow_support:world_event_to_string( Event ) ] ),

	?wooper_return_state_only( State ).




% Called (most probably by the experiment manager) to notify this unit manager
% that a creation event has been received and successfully matched against a
% clause specified by this unit manager.
%
% (actor oneway)
%
-spec processCreationEventMatched( wooper:state(), creation_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processCreationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onCreationEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching creation happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onCreationEventMatched( wooper:state(), creation_event() ) ->
									oneway_return().
onCreationEventMatched( State, CreationEvent ) ->

	?warning_fmt( "Default onCreationEventMatched/2 implementation "
				  "ignoring ~s.",
				  [ dataflow_support:world_event_to_string( CreationEvent ) ] ),

	?wooper_return_state_only( State ).




% Called (most probably by the experiment manager) to notify this unit manager
% that a destruction event has been received and successfully matched against a
% clause specified by this unit manager.
%
% (actor oneway)
%
-spec processDestructionEventMatched( wooper:state(), destruction_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processDestructionEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onDestructionEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching destruction happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onDestructionEventMatched( wooper:state(), destruction_event() ) ->
									   oneway_return().
onDestructionEventMatched( State, DestructionEvent ) ->

	?warning_fmt( "Default onDestructionEventMatched/2 implementation "
				  "ignoring ~s.", [ dataflow_support:world_event_to_string(
									  DestructionEvent ) ] ),

	?wooper_return_state_only( State ).




% Called (most probably by the experiment manager) to notify this unit manager
% that an association event has been received and successfully matched against a
% clause specified by this unit manager.
%
% (actor oneway)
%
-spec processAssociationEventMatched( wooper:state(), association_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processAssociationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onAssociationEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching association happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onAssociationEventMatched( wooper:state(), association_event() ) ->
									   wooper:state().
onAssociationEventMatched( State, AssociationEvent ) ->

	?warning_fmt( "Default onAssociationEventMatched/2 implementation "
				  "ignoring ~s.", [ dataflow_support:world_event_to_string(
									  AssociationEvent ) ] ),

	?wooper_return_state_only( State ).




% Called (most probably by the experiment manager) to notify this unit manager
% that a binary association event has been received and successfully matched
% against a clause specified by this unit manager.
%
% (actor oneway)
%
-spec processBinaryAssociationEventMatched( wooper:state(),
	binary_association_event(), sending_actor_pid() ) -> actor_oneway_return().
processBinaryAssociationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onBinaryAssociationEventMatched,
								 [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching binary association happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onBinaryAssociationEventMatched( wooper:state(),
							   binary_association_event() ) -> wooper:state().
onBinaryAssociationEventMatched( State, BinaryAssociationEvent ) ->

	?warning_fmt( "Default onBinaryAssociationEventMatched/2 implementation "
				  "ignoring ~s.", [ dataflow_support:world_event_to_string(
									  BinaryAssociationEvent ) ] ),

	?wooper_return_state_only( State ).



% Called (most probably by the experiment manager) to notify this unit manager
% that a disassociation event has been received and successfully matched against
% a clause specified by this unit manager.
%
% (actor oneway)
%
-spec processDisassociationEventMatched( wooper:state(), disassociation_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processDisassociationEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onDisassociationEventMatched,
								 [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching disassociation happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onDisassociationEventMatched( wooper:state(), disassociation_event() ) ->
										  oneway_return().
onDisassociationEventMatched( State, DisassociationEvent ) ->

	?warning_fmt( "Default onDisassociationEventMatched/2 implementation "
				  "ignoring ~s.", [ dataflow_support:world_event_to_string(
									  DisassociationEvent ) ] ),

	?wooper_return_state_only( State ).




% Called (most probably by the experiment manager) to notify this unit manager
% that a connection event has been received and successfully matched against a
% clause specified by this unit manager.
%
% (actor oneway)
%
-spec processConnectionEventMatched( wooper:state(), connection_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processConnectionEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onConnectionEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching connection happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onConnectionEventMatched( wooper:state(), connection_event() ) ->
									  oneway_return().
onConnectionEventMatched( State, ConnectionEvent ) ->

	?warning_fmt( "Default onConnectionEventMatched/2 implementation "
				  "ignoring ~s.", [ dataflow_support:world_event_to_string(
									  ConnectionEvent ) ] ),

	?wooper_return_state_only( State ).




% Called (most probably by the experiment manager) to notify this unit manager
% that a disconnection event has been received and successfully matched against
% a clause specified by this unit manager.
%
% (actor oneway)
%
-spec processDisconnectionEventMatched( wooper:state(), connection_event(),
							  sending_actor_pid() ) -> actor_oneway_return().
processDisconnectionEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onDisconnectionEventMatched,
								 [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching disconnection happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onDisconnectionEventMatched( wooper:state(), disconnection_event() ) ->
										 oneway_return().
onDisconnectionEventMatched( State, DisconnectionEvent ) ->

	?warning_fmt( "Default onDisconnectionEventMatched/2 implementation "
				  "ignoring ~s.", [ dataflow_support:world_event_to_string(
									  DisconnectionEvent ) ] ),

	?wooper_return_state_only( State ).



% Called (most probably by the experiment manager) to notify this unit manager
% that an update event has been received and successfully matched against a
% clause specified by this unit manager.
%
% (actor oneway)
%
-spec processUpdateEventMatched( wooper:state(), disassociation_event(),
								 sending_actor_pid() ) -> actor_oneway_return().
processUpdateEventMatched( State, Event, _SendingActorPid ) ->

	CalledState = executeOneway( State, onUpdateEventMatched, [ Event ] ),

	EventState = manage_possible_event_completion( Event, CalledState ),

	?wooper_return_state_only( EventState ).



% Called so that this unit manager can perform domain-specific actions of its
% choice whenever a matching update happened.
%
% Note: catch-all placeholder implementation, meant to be overridden.
%
% (oneway)
%
-spec onUpdateEventMatched( wooper:state(), update_event() ) -> oneway_return().
onUpdateEventMatched( State, UpdateEvent ) ->

	?warning_fmt( "Default onUpdateEventMatched/2 implementation "
				  "ignoring ~s.",
				  [ dataflow_support:world_event_to_string( UpdateEvent ) ] ),

	?wooper_return_state_only( State ).





% Creates, synchronously and while the simulation is not running, an (initial)
% instance of the specified unit type, associated to specified dataflow, using
% specified core construction parameters for that, and returning the
% corresponding instance PID.
%
% (request)
%
-spec createInitialUnitInstance( wooper:state(), managed_unit_spec(),
				dataflow_pid(), construction_parameters() ) ->
									   request_return( unit_pid() ).
createInitialUnitInstance( State, _UnitSpec={ UnitType, _Language=erlang },
						   DataflowPid, CoreConstructionParameters ) ->

	% Clause for standard (Erlang-based) units.

	% Building the full construction parameters for the new unit:
	?debug_fmt( "Creating an initial instance of unit type '~s', associated "
				"to dataflow ~w, and based on following core construction "
				"parameters:~n~p.",
				[ UnitType, DataflowPid, CoreConstructionParameters ] ),

	FullConstructParams = list_utils:append_at_end( DataflowPid,
												CoreConstructionParameters ),

	% Creating the unit with these parameters:
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPid = class_Actor:create_initial_actor( UnitType, FullConstructParams,
												LoadBalancerPid ),

	% Will register itself to its dataflow at the first diasca of this unit.

	% May create a new entry for this unit type:
	NewUnitTable = table:appendToEntry( _K=UnitType, UnitPid,
										?getAttr(unit_table) ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	?wooper_return_state_result( NewState, UnitPid );


% Created through a binding:
createInitialUnitInstance( State, _UnitSpec={ UnitType, Language },
						   DataflowPid, CoreConstructionParameters ) ->

	% Building the full construction parameters for the new unit:
	?debug_fmt( "Creating an initial instance of unit type '~s', relying on "
				"the ~s binding, associated to dataflow ~w, and based on "
				"following core construction parameters:~n~p.",
				[ UnitType, language_utils:language_to_string( Language ),
				  DataflowPid, CoreConstructionParameters ] ),

	% Per-binding generic unit type (ex: class_DataflowPythonProcessingUnit):
	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	% Binding manager in charge of that language (ex: the PythonBindingManager):
	BindingManagerPid = binding_utils:get_binding_manager( Language,
										   ?getAttr(binding_managers) ),

	FullConstructParams = [ UnitType, CoreConstructionParameters, DataflowPid,
							BindingManagerPid ],

	% Creating the unit with these parameters:
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPid = class_Actor:create_initial_actor( ActualUnitType,
												FullConstructParams,
												LoadBalancerPid ),

	% Will register itself to its dataflow at the first diasca of this unit.

	% May create a new entry for this unit type:
	NewUnitTable = table:appendToEntry( _K=UnitType, UnitPid,
										?getAttr(unit_table) ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	?wooper_return_state_result( NewState, UnitPid );


createInitialUnitInstance( State, _UnitSpec=UnitType, DataflowPid,
						   CoreConstructionParameters )
  when is_atom( UnitType ) ->

	FullUnitSpec={ UnitType, _Language=erlang },

	createInitialUnitInstance( State, FullUnitSpec, DataflowPid,
							   CoreConstructionParameters );

createInitialUnitInstance( State, UnitSpec, _DataflowPid,
							_CoreConstructParameters ) ->

	?error_fmt( "Impossible to instantiate processing units according to the "
				"spec:~n   ~p~n The specification of a processing unit must "
				"take either the form of an atom (WOOPER class name), or the "
				"one of a 2-tuple, which first element is the same kind of "
				"atom (WOOPER class), followed by an atom referring to its "
				"implementation language.", [ UnitSpec ] ),

	throw( { bad_initial_unit_spec, UnitSpec } ).



% Creates, synchronously and while the simulation is not running, an (initial)
% instance of the specified unit type, using specified core construction
% parameters for that, and returning the corresponding instance PID.
%
% (request)
%
-spec createInitialMockupUnitInstance( wooper:state(), mockup_unit_spec(),
		   dataflow_pid(), class_DataflowProcessingUnit:unit_name() ) ->
											 request_return( unit_pid() ).
createInitialMockupUnitInstance( State, MockupUnitSpec, DataflowPid,
								 UnitName ) ->

	UnitType = MockupUnitSpec#mockup_unit_spec.unit_type,

	?debug_fmt( "Creating an initial instance of mockup unit type '~s', named "
				"~s and based on the following specification record: ~p.",
				[ UnitType, UnitName, MockupUnitSpec ] ),

	% Building the full construction parameters for the new unit:
	FullConstructParams = [ UnitName, MockupUnitSpec, DataflowPid ],

	% Creating the unit with these parameters:
	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPid = class_Actor:create_initial_actor( class_DataflowMockupUnit,
												FullConstructParams,
												LoadBalancerPid ),

	% Will register itself to its dataflow at the first diasca of this unit.

	% May create a new entry for this unit type:
	NewUnitTable = table:appendToEntry( _K=UnitType, UnitPid,
										 ?getAttr(unit_table) ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	?wooper_return_state_result( NewState, UnitPid ).



% Creates, synchronously and while the simulation is not running, a set of
% (initial) instances of the specified unit type, using the specified list of
% core construction parameters for that, and returning the corresponding
% instance PIDs, in the same order.
%
% (request)
%
-spec createInitialUnitInstances( wooper:state(), managed_unit_spec(),
							dataflow_pid(), [ construction_parameters() ] ) ->
										request_return( [ unit_pid() ] ).
createInitialUnitInstances( State, _UnitSpec={ UnitType, _Language=erlang },
							DataflowPid, CoreConstructParamLists ) ->

	ParamStrings = [ text_utils:format( "~p", [ CPL ] )
					 || CPL <- CoreConstructParamLists ],

	?debug_fmt( "Creating ~B initial instances of unit type '~s', associated "
				"to dataflow ~w, based on following list of core construction "
				"parameters:~s",
				[ length( CoreConstructParamLists ), UnitType, DataflowPid,
				  text_utils:strings_to_string( ParamStrings ) ] ),

	% Prepares a list of { Classname, FullConstructParams }:
	ConstructEntries = [ { UnitType,
						   list_utils:append_at_end( DataflowPid, CPL ) }
						 || CPL <- CoreConstructParamLists ],

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPidList = class_Actor:create_initial_actors( ConstructEntries,
													 LoadBalancerPid ),

	UnitTable = ?getAttr(unit_table),

	UnitList = table:getEntry( _K=UnitType, UnitTable ),

	NewUnitTable = table:addEntry( UnitType, UnitPidList ++ UnitList,
								   UnitTable ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	?wooper_return_state_result( NewState, UnitPidList );


createInitialUnitInstances( State, _UnitSpec={ UnitType, Language },
							DataflowPid, CoreConstructParamLists ) ->

	ParamStrings = [ text_utils:format( "~p", [ CPL ] )
					 || CPL <- CoreConstructParamLists ],

	?debug_fmt( "Creating ~B initial instances of unit type '~s', relying on "
				"the ~s binding, associated to dataflow ~w, based on following "
				"list of core construction parameters:~s",
				[ length( CoreConstructParamLists ), UnitType,
				  language_utils:language_to_string( Language ), DataflowPid,
				  text_utils:strings_to_string( ParamStrings ) ] ),

	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	BindingManagerPid = binding_utils:get_binding_manager( Language,
												?getAttr(binding_managers) ),

	% Prepares a list of { Classname, FullConstructParams }:
	ConstructEntries = [ { ActualUnitType,
						   [ UnitType, CPL, DataflowPid, BindingManagerPid ] }
						 || CPL <- CoreConstructParamLists ],

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPidList = class_Actor:create_initial_actors( ConstructEntries,
													 LoadBalancerPid ),

	UnitTable = ?getAttr(unit_table),

	UnitList = table:getEntry( _K=UnitType, UnitTable ),

	NewUnitTable = table:addEntry( UnitType, UnitPidList ++ UnitList,
								   UnitTable ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	?wooper_return_state_result( NewState, UnitPidList );


createInitialUnitInstances( State, _UnitSpec=UnitType, DataflowPid,
							CoreConstructParamLists )
  when is_atom( UnitType ) ->

	FullUnitSpec={ UnitType, _Language=erlang },

	createInitialUnitInstances( State, FullUnitSpec, DataflowPid,
								CoreConstructParamLists );


createInitialUnitInstances( State, UnitSpec, _DataflowPid,
							_CoreConstructParamLists ) ->

	?error_fmt( "Impossible to instantiate processing units according to the "
				"spec:~n   ~p~n The specification of a processing unit must "
				"take either the form of an atom (WOOPER class name), or the "
				"one of a 2-tuple, which first element is the same kind of "
				"atom (WOOPER class), followed by an atom referring to its "
				"implementation language.", [ UnitSpec ] ),

	throw( { bad_initial_units_spec, UnitSpec } ).



% Creates, synchronously and while the simulation is not running, a set of
% (initial) instances of the specified mockup unit type, using a specification
% parameters record and the unit names for that, and returning the corresponding
% instance PIDs, in the same order.
%
% (request)
%
-spec createInitialMockupUnitInstances( wooper:state(), mockup_unit_spec(),
		   dataflow_pid(), [ class_DataflowProcessingUnit:unit_name() ] ) ->
											  request_return( [ unit_pid() ] ).
createInitialMockupUnitInstances( State, MockupUnitSpec, DataflowPid,
								  UnitNames ) ->

	UnitType = MockupUnitSpec#mockup_unit_spec.unit_type,

	?debug_fmt( "Creating ~B initial instances of mockup unit type '~s' based "
				"on the following specification parameters:~s",
				[ length( UnitNames ), UnitType,
				  text_utils:format( "~p", [ MockupUnitSpec ] ) ] ),

	% Prepares a list of { Classname, FullConstructParams }:
	CoreConstructParamList = [ [ UN, MockupUnitSpec ] || UN <- UnitNames ],

	ConstructEntries = [ { class_DataflowMockupUnit,
						   list_utils:append_at_end( DataflowPid, CP ) }
						 || CP <- CoreConstructParamList ],

	LoadBalancerPid = ?getAttr(load_balancer_pid),

	UnitPidList = class_Actor:create_initial_actors( ConstructEntries,
													 LoadBalancerPid ),

	UnitTable = ?getAttr(unit_table),

	UnitList = table:getEntry( _K=UnitType, UnitTable ),

	NewUnitTable = table:addEntry( UnitType, UnitPidList ++ UnitList,
								   UnitTable ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	?wooper_return_state_result( NewState, UnitPidList ).



% Creates specified unit.
%
% Will trigger back a call to onUnitCreated/6.
%
% Like create_runtime_unit/4, except operating with the changeset system (rather
% than in a programmatic setting).
%
% (oneway)
%
-spec createUnit( wooper:state(), managed_unit_spec(),
				  wooper:construction_parameters(), event_id(),
				  unit_creation_context() ) -> oneway_return().
createUnit( State, _UnitSpec={ UnitType, erlang }, UnitConstructParams,
			EventId, Context ) ->

	NewActionId = ?getAttr(action_count) + 1,

	?debug_fmt( "Creating a '~s' unit, implemented in ~s,  with construction "
				"parameters ~p for event #~B (action #~B; context: ~p).",
				[ UnitType, language_utils:language_to_string( erlang ),
				  UnitConstructParams, EventId, NewActionId, Context ] ),

	CreatedState = class_Actor:create_actor( UnitType, UnitConstructParams,
											 _Tag=NewActionId, State ),

	% Registers the pending creation (corresponding to this new action), so that
	% its completion makes the processing of the overall event progress:
	%
	NewAction = { unit_creation, UnitType, UnitConstructParams, EventId,
				  Context },

	NewEventTable = register_action_for_event( NewActionId, EventId,
											   CreatedState ),

	NewActionTable = table:addNewEntry( NewActionId, NewAction,
										?getAttr(action_table) ),

	FinalState = setAttributes( CreatedState, [
					{ action_count, NewActionId },
					{ event_table, NewEventTable },
					{ action_table, NewActionTable } ] ),

	?wooper_return_state_only( FinalState );


createUnit( State, _UnitSpec={ UnitType, Language }, UnitConstructParams,
			EventId, Context ) ->

	NewActionId = ?getAttr(action_count) + 1,

	?trace_fmt( "Creating a '~s' unit, implemented in ~s,  with construction "
				"parameters ~p for event #~B (action #~B; context: ~p).",
				[ UnitType, language_utils:language_to_string( Language ),
				  UnitConstructParams, EventId, NewActionId, Context ] ),

	% Per-binding generic unit type (ex: class_DataflowPythonProcessingUnit):
	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	% Binding manager in charge of that language (ex: the PythonBindingManager):
	BindingManagerPid = binding_utils:get_binding_manager( Language,
												?getAttr(binding_managers) ),

	%UnitConstructParams = [ UnitName, _Year=2020, 0.5, 1.0, DataflowPid ],

	% By convention, DataflowPid is the last element of the construction
	% parameters:
	%
	{ DataflowPid, OtherParams } = list_utils:extract_last_element(
									 UnitConstructParams ),

	FullUnitConstructParams = [ UnitType, OtherParams ]
		++ [ DataflowPid, BindingManagerPid ],

	%trace_utils:debug_fmt( "ActualUnitType: '~p', "
	%					   "FullUnitConstructParams: '~p'.",
	%					   [ ActualUnitType, FullUnitConstructParams ] ),

	CreatedState = class_Actor:create_actor( ActualUnitType,
					   FullUnitConstructParams, _Tag=NewActionId, State ),

	% Registers the pending creation (corresponding to this new action), so that
	% its completion makes the processing of the overall event progress:
	%
	NewAction = { unit_creation, UnitType, UnitConstructParams, EventId,
				  Context },

	NewEventTable = register_action_for_event( NewActionId, EventId,
											   CreatedState ),

	NewActionTable = table:addNewEntry( NewActionId, NewAction,
										?getAttr(action_table) ),

	FinalState = setAttributes( CreatedState, [
					{ action_count, NewActionId },
					{ event_table, NewEventTable },
					{ action_table, NewActionTable } ] ),

	?wooper_return_state_only( FinalState );


createUnit( State, _UnitSpec=UnitType, UnitConstructParams, EventId,
			Context ) ->

	FullUnitSpec = { UnitType, _Language=erlang },

	createUnit( State, FullUnitSpec, UnitConstructParams, EventId, Context ).




% Destructs specified unit.
%
% Will trigger back a call to onUnitDestructed/4.
%
% (oneway)
%
-spec destructUnit( wooper:state(), unit_pid(), event_id() ) -> oneway_return().
destructUnit( State, UnitPid, EventId ) ->

	NewActionId = ?getAttr(action_count) + 1,

	?debug_fmt( "Destructing unit ~p for event #~B (action #~B).",
				[ UnitPid, EventId, NewActionId ] ),

	% Will trigger a onUnitDestructed/4 callback:
	DestructedState = class_Actor:send_actor_message( UnitPid,
		{ triggerDestruction, [ NewActionId ] }, State ),

	NewAction = { unit_destruction, UnitPid, EventId },

	NewEventTable = register_action_for_event( NewActionId, EventId,
											   DestructedState ),

	NewActionTable = table:addNewEntry( NewActionId, NewAction,
										?getAttr(action_table) ),

	FinalState = setAttributes( DestructedState, [
					{ action_count, NewActionId },
					{ event_table, NewEventTable },
					{ action_table, NewActionTable } ] ),

	?wooper_return_state_only( FinalState ).



% Connects directly (i.e. thanks to an unsynchronised request - thus to be done
% initially) the named output port of each of the specified upstream blocks to
% the specified iteration of the specified downstream unit, creating the
% corresponding iterated input ports for that.
%
% (request)
%
-spec connectToIteratedInitially( wooper:state(),
	{ [ upstream_block_pid() ], output_port_name() }, iteration_port_target() )
					   -> request_return( 'connected_to_iterated' ).
connectToIteratedInitially( State, { UpstreamBlocks, OutputPortName },
							{ DownstreamUnitPid, InputIterationName } )
  when is_binary( OutputPortName ) andalso is_binary( InputIterationName ) ->

	ChannelCount = length( UpstreamBlocks ),

	?debug_fmt( "Creating ~B channels, from the output port '~s' of each of "
				"the upstream units ~p to the input port iteration '~s' of "
				"the downstream unit ~p.",
				[ ChannelCount, OutputPortName, UpstreamBlocks,
				  InputIterationName, DownstreamUnitPid ] ),

	% For that we have to request from the iteration the right number of input
	% iterated ports:
	%
	DownstreamUnitPid ! { createInputIteratedPorts,
						  [ InputIterationName, ChannelCount ], self() },

	InputPortNames = receive

		{ wooper_result, PortNames } ->
			PortNames

	end,

	% By design the waited units are exactly the upstream ones:
	request_initial_connections_to_iterated( UpstreamBlocks, OutputPortName,
									 DownstreamUnitPid, InputPortNames ),

	% Answers to the connectOutputPortInitially/4 requests:
	wooper:wait_for_request_acknowledgements( ChannelCount,
											  output_port_connected ),

	?wooper_return_state_result( State, connected_to_iterated ).



% Called automatically after (generally after two diascas) this manager created
% a unit instance.
%
% Parameters are:
%
% - CreatedUnitPid the PID of the just created unit
%
% - CreatedActorTag the tag used for this actor creation so that it is able to
% discriminate among the multiple creations it might have requested; this is
% here the identifier of the corresponding action

% (actor oneway)
%
-spec onActorCreated( wooper:state(), unit_pid(), action_id(),
					  load_balancer_pid() ) -> oneway_return().
onActorCreated( State, CreatedUnitPid, _CreatedActorTag=ActionId,
				_LoadBalancerPid ) ->

	% This is the generic part of any runtime unit creation, dispatching
	% relevant information to the (most probably overridden) onUnitCreated/6
	% oneway.

	ActionTable = ?getAttr(action_table),

	{ _Action={ unit_creation, UnitType, ConstructParams, EventId, Context },
	  ShrunkActionTable } = table:extractEntry( ActionId, ActionTable ),

	?void_fmt( "Recording newly created unit instance ~p of type ~s "
				"for event #~B (created from ~p, with context ~p, "
				"through action #~B, in the course of the simulation).",
				[ CreatedUnitPid, UnitType, EventId, ConstructParams, Context,
				  ActionId ] ),

	ShrunkState = setAttribute( State, action_table, ShrunkActionTable ),

	% Domain-specific actions done there:
	UnitState = executeOneway( ShrunkState, onUnitCreated, [ UnitType,
		ConstructParams, CreatedUnitPid, EventId, Context ] ),

	% The onUnitCreated/6 method might have decided for more actions:
	DeclaredState = declare_action_performed( ActionId, EventId, UnitState ),

	UnitTable = getAttribute( DeclaredState, unit_table ),

	% Updates already-existing entry:
	NewUnitTable = table:appendToExistingEntry( _K=UnitType, CreatedUnitPid,
												UnitTable ),

	FinalState = setAttribute( DeclaredState, unit_table, NewUnitTable ),

	?wooper_return_state_only( FinalState ).



% Called whenever a unit has been created; meant to be overridden with any
% action needed (typically creating channels between this new unit and the rest
% of the dataflow).
%
% Parameters are:
%
% - CreatedUnitType is the type (classname) of the just created unit
%
% - CreatedUnitConstructionParameters is the construction parameters
% corresponding to this new creation
%
% - CreatedUnitPid is the PID of the just created unit
%
% - EventId is the identifier of the corresponding overall world event
%
% - CreationContext is the context of this creation
%
% (oneway)
%
-spec onUnitCreated( wooper:state(), dataflow_unit_type(),
					 wooper:construction_parameters(), unit_pid(), event_id(),
					 unit_creation_context() ) -> oneway_return().
onUnitCreated( State, _CreatedUnitType, _CreatedUnitConstructionParameters,
			   _CreatedUnitPid, _EventId, _CreationContext ) ->

	?warning( "Default onUnitCreated/6 oneway not overridden." ),

	?wooper_return_state_only( State ).



% Called whenever a unit has been destructed (typically from the
% triggerDestruction/3 actor oneway of that unit).
%
% May be overridden if needed (in that case this base implementation shall be
% called from there).
%
% Parameters are:
%
% - DestructedUnitPid is the PID of the just destructd unit
%
% - ActionId is the identifier of the corresponding action
%
% - CreationContext is the context of this creation
%
% Note: it is an actor oneway, not a mere oneway like for onUnitCreated/6.
%
% (actor oneway)
%
-spec onUnitDestructed( wooper:state(), action_id(), dataflow_unit_type(),
						unit_pid() ) -> actor_oneway_return().
onUnitDestructed( State, ActionId, UnitType,
				  _SendingActorPid=DestructedUnitPid ) ->

	% This is the generic part of any runtime unit destruction.
	% Note: very much like onConnectionsCreated/6.

	ActionTable = ?getAttr(action_table),

	% Includes a match-based check on unit PID:
	{ _Action={ unit_destruction, DestructedUnitPid, EventId },
	  ShrunkActionTable } = table:extractEntry( ActionId, ActionTable ),

	?debug_fmt( "Recording the destruction of unit ~w "
				"(in the context of action #~B of event #~B)",
				[ DestructedUnitPid, ActionId, EventId ] ),

	% Generally nothing domain-specific to be done here, thus no call to a
	% oneway in the spirit of onUnitCreated/6.

	DeclaredState = declare_action_performed( ActionId, EventId, State ),

	% Removes already-existing entry:
	NewUnitTable = table:deleteExistingFromEntry( _K=UnitType,
							  DestructedUnitPid, ?getAttr(unit_table) ),

	FinalState = setAttributes( DeclaredState, [
						{ unit_table, NewUnitTable },
						{ action_table, ShrunkActionTable } ] ),

	?wooper_return_state_only( FinalState ).




% Helper functions.


% Requests the specified upstream units to connect their output port named as
% specified to a specific one among the provided iterated input ports of the
% downstream unit.
%
% (helper)
%
-spec request_initial_connections_to_iterated( [ unit_pid() ],
		  output_port_name(), unit_pid(), [ input_port_name() ] ) ->
													 basic_utils:void().
% Exhausted:
request_initial_connections_to_iterated( _UpstreamUnits=[], _OutputPortName,
								 _DownstreamUnitPid, _InputPortNames=[] ) ->
	ok;

request_initial_connections_to_iterated( _UpstreamUnits=[ UpUnitPid | TUnit ],
								 OutputPortName, DownstreamUnitPid,
								 _InputPortNames=[ InputPortName | TName ] ) ->

	UpUnitPid ! { connectOutputPortInitially,
				 [ OutputPortName, DownstreamUnitPid, InputPortName ], self() },

	request_initial_connections_to_iterated( TUnit, OutputPortName,
											 DownstreamUnitPid, TName );

% Both lists expected to be exhausted simultaneously:
request_initial_connections_to_iterated( UpstreamUnits, OutputPortName,
										 DownstreamUnitPid, InputPortNames ) ->
	throw( { inconsistent_internal_state, { UpstreamUnits, OutputPortName },
			 { DownstreamUnitPid, InputPortNames } } ).



% Returns a textual description of the unit instances currently managed.
%
-spec unit_table_to_string( wooper:state() ) -> string().
unit_table_to_string( State ) ->

	case table:enumerate( ?getAttr(unit_table) ) of

		[] ->
			 "not managing any unit type";

		Types ->

			StringEntries = [
				case IList of

					[] ->
						text_utils:format( "no instance of unit type '~s'",
										   [ UName ] );

					_ ->
						text_utils:format( "~B instance(s) of unit "
										   "type '~s': ~w",
										   [ length( IList ), UName, IList ] )

				end || { UName, IList } <- Types ],

			text_utils:format( "managing ~B unit types:", [ length( Types ) ] )
				++ text_utils:strings_to_string( StringEntries )

	end.



% Declares that specified action, in the context of specified event, has been
% performed.
%
% Possibly reports that this event is fully processed by this unit manager.
%
-spec declare_action_performed( action_id(), event_id(), wooper:state() ) ->
									  wooper:state().
declare_action_performed( ActionId, EventId, State ) ->

	EventTable = ?getAttr(event_table),

	case table:lookupEntry( EventId, EventTable ) of

		{ value, ActionList } ->
			ShrunkActionList = list_utils:delete_existing( ActionId,
														   ActionList ),

			manage_possible_event_completion( ShrunkActionList, EventId,
											  EventTable, State );

		key_not_found ->
			erlang:error( { event_not_known, EventId } )

	end.






% Here, all actions (if any) for said event have been processed:
%
manage_possible_event_completion( _ActionList=[], EventId, EventTable,
								  State ) ->

	% Event may not be in table if called from a match clause not declaring any
	% action:
	%
	NewEventTable = table:removeEntry( EventId, EventTable ),

	% Just for traces here:
	case table:keys( NewEventTable ) of

		[] ->
			?void_fmt( "Reporting that event #~B has been fully "
						"processed; no more pending event.",
						[ EventId ] );

		EventList ->
			?void_fmt( "Reporting that event #~B has been fully "
						"processed; still ~B pending events: ~w.",
						[ EventId, length( EventList ), EventList ] )

	end,

	SentState = class_Actor:send_actor_message(
				  ?getAttr(experiment_manager_pid),
				  { onEventProcessed, [ EventId ] }, State ),

	setAttribute( SentState, event_table, NewEventTable );


% Here, at least one action is remaining:
%
manage_possible_event_completion( ActionList, EventId, EventTable, State ) ->
	NewEventTable = table:addEntry( EventId, ActionList, EventTable ),
	setAttribute( State, event_table, NewEventTable ).



% To be called typically from one of the process*Matched/3 actor oneways, to
% determine automatically whether the specified event is fully processed.
%
% (helper)
%
-spec manage_possible_event_completion( world_event(), wooper:state() ) ->
											  wooper:state().
manage_possible_event_completion( Event, State ) ->

	EventId = dataflow_support:get_event_id( Event ),

	EventTable = ?getAttr(event_table),

	ActionList = get_actions_for_event( EventId, EventTable ),

	manage_possible_event_completion( ActionList, EventId, EventTable,
									  State ).



% Section for exported helpers.



% Creates a set of channels, in the context of the processing of the specified
% event, between specified upstream and downstream blocks, based on the
% specified port names (be they the same on both sides or not, be there standard
% or iterated ones), and returns an updated state.
%
% If just a name PortName is specified, then it is assumed that both endpoints
% are standard ports, and that they bear that same port name.
%
% Otherwise the complete form is to be used, a pair describing the output port
% and the input one. Not specifying the kind of port (standard or iteration)
% defaults to standard.
%
% Will ultimately trigger back a call to onConnectionsCreated/5.
%
% Note: to be used even if a single channel is to be created.
%
% (exported helper)
%
-spec create_channels_for( event_id(), upstream_block_pid(),
						   downstream_block_pid(), [ connection_spec() ],
						   wooper:state() ) -> wooper:state().
create_channels_for( EventId, UpstreamBlockPid, DownstreamBlockPid,
					 ConnectionSpecs, State ) ->

	% Block endpoints and state specified to report clearer errors:
	%
	CanonicalConnectionSpecs = canonicalize_connection_specs( ConnectionSpecs,
								 UpstreamBlockPid, DownstreamBlockPid, State ),

	?trace_fmt( "Creating ~B channels in the context of event #~B, from "
				"upstream block ~w to downstream one ~w, using ~s",
				[ length( CanonicalConnectionSpecs ), EventId, UpstreamBlockPid,
				  DownstreamBlockPid,
				  connection_specs_to_string( CanonicalConnectionSpecs ) ] ),

	NewActionId = ?getAttr(action_count) + 1,

	% Requests the upstream block to create these downstream channels:
	Oneway = { connectToDownstreamBlock,
			   [ CanonicalConnectionSpecs, DownstreamBlockPid, NewActionId ] },

	SentState = class_Actor:send_actor_message( UpstreamBlockPid, Oneway,
												State ),

	NewEventTable = register_action_for_event( NewActionId, EventId,
											   SentState ),

	% Records that action for a later acknowledgment thereof:
	NewAction = { unit_connection, EventId, UpstreamBlockPid,
				  DownstreamBlockPid, CanonicalConnectionSpecs,
				  _Context=undefined },


	NewActionTable = table:addNewEntry( NewActionId, NewAction,
										?getAttr(action_table) ),

	setAttributes( SentState, [ { event_table, NewEventTable },
								{ action_table, NewActionTable },
								{ action_count, NewActionId } ] ).



% Called (by an upsteam block) once a set of channels from this upstream block
% to a downstream one has been created, as requested by the
% create_channels_for/5 helper of this unit manager.
%
% May be overridden if needed (in that case this base implementation shall be
% called from there).
%
% Parameters are:
%
% - PortPairs is a list of the actual names of the output and input ports
% created for the requested channels; these names correspond to standard ports,
% possibly created from any outut or input iteration
%
% - DownstreamBlockPid is the PID of the target block to which all channels are
% drawn
%
% - ActionId is the identifier of the corresponding action, as known by the unit
% manager that triggered it
%
% - UpstreamBlockPid is the PID of the source block from which all channels are
% drawn; it happens also to be the sending actor
%
% Note: it is an actor oneway, not a mere oneway like for onUnitCreated/6.
%
% (actor oneway)
%
-spec onConnectionsCreated( wooper:state(), [ connection_info() ], actor_pid(),
					action_id(), sending_actor_pid() ) -> actor_oneway_return().
onConnectionsCreated( State, PortPairs, DownstreamBlockPid, ActionId,
					  _SenderPid=UpstreamBlockPid ) ->

	ActionTable = ?getAttr(action_table),

	% Includes a match-based check on block PIDs:
	{ _Action={ unit_connection, EventId, UpstreamBlockPid, DownstreamBlockPid,
				CanonicalConnectionSpecs, _Context=undefined },
	  ShrunkActionTable } = table:extractEntry( ActionId, ActionTable ),

	?void_fmt( "Recording ~B channel connections through action #~B, from "
				"upstream block ~w to downstream one ~w, involving following "
				"ports: ~s~n(canonical connection specs were:~s)",
				[ length( PortPairs ), ActionId, UpstreamBlockPid,
				  DownstreamBlockPid, text_utils:strings_to_string(
					  [ text_utils:format(
						  "from output port '~s' to input one '~s'",
						 [ OutputPortName, InputPortName ] )
					|| { OutputPortName, InputPortName } <- PortPairs ] ),
				  connection_specs_to_string( CanonicalConnectionSpecs ) ] ),


	% Generally nothing domain-specific to be done here, thus no call to a
	% oneway in the spirit of onUnitCreated/6.

	DeclaredState = declare_action_performed( ActionId, EventId, State ),

	FinalState = setAttribute( DeclaredState, action_table, ShrunkActionTable ),

	?wooper_return_state_only( FinalState ).



% Creates a set of output ports on the specified block.
%
% This is a synchronous call: ports are already created whe it returns.
%
-spec create_output_ports( block_pid(), [ output_port_spec() ] ) ->
								 basic_utils:void().
create_output_ports( BlockPid, OutputPortSpecs ) ->
	wooper:execute_request( createOutputPorts, [ OutputPortSpecs ], BlockPid,
							_ExpectedResult=output_ports_created ).




% Stringification section.



% Returns a textual description of the specified synchronization event matches.
%
-spec event_clauses_to_string( [ event_match() ] ) -> string().
event_clauses_to_string( EventMatches ) ->

	EventString = text_utils:strings_to_string(
					[ event_clause_to_string( E ) || E <- EventMatches ] ),

	text_utils:format( "~B synchronization event matches:~s",
					   [ length( EventMatches ), EventString ] ).



% Returns a textual description of the specified synchronization event clause.
%
-spec event_clause_to_string( event_match() ) -> string().
event_clause_to_string( EventMatch=#creation_event_match{} ) ->
	text_utils:format( "creation clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#destruction_event_match{} ) ->
	text_utils:format( "destruction clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#association_event_match{} ) ->
	text_utils:format( "association clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#binary_association_event_match{} ) ->
	text_utils:format( "binary association clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#disassociation_event_match{} ) ->
	text_utils:format( "disassociation clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#connection_event_match{} ) ->
	text_utils:format( "connection clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#disconnection_event_match{} ) ->
	text_utils:format( "disconnection clause ~p", [ EventMatch ] );

event_clause_to_string( EventMatch=#update_event_match{} ) ->
	text_utils:format( "update clause ~p", [ EventMatch ] );

event_clause_to_string( _EventMatch=any_event_type ) ->
	"clause corresponding to any type of event".




% Returns a textual description of the specified action.
%
-spec action_to_string( action() ) -> string().
action_to_string( { unit_creation, EventId, UnitType, ConstructParams,
					Context } ) ->
	text_utils:format( "unit creation for event #~B, "
					   "for unit type ~s, construction parameters ~p "
					   "and context ~p",
					   [ EventId, UnitType, ConstructParams, Context ] );

action_to_string( { unit_connection, EventId, OutputPortId, InputPortId,
					Context } ) ->
	text_utils:format( "channel creation for event #~B, from ~s to ~s, "
					   " context ~p", [ EventId,
					   dataflow_support:port_id_to_string( OutputPortId ),
					   dataflow_support:port_id_to_string( InputPortId ),
					   Context ] ).



% Returns a textual description of this unit manager.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	IdString = case ?getAttr(identification_server_pid) of

		undefined ->
			"no identification server";

		IdPid ->
			text_utils:format( "identification server ~w", [ IdPid ] )

	end,

	UnitTypeString = unit_table_to_string( State ),

	ClauseString = event_clauses_to_string( ?getAttr(event_matches) ),

	EventString = case table:enumerate( ?getAttr(event_table) ) of

		[] ->
			"no pending event";

		EventPairs ->
			EvStrings = [ text_utils:format( "the processing of event #~B "
											 "involves following ~B actions: "
											 "~w", [ EvId, length( Actions ),
													 Actions ] )
						  || { EvId, Actions } <- EventPairs ],

			text_utils:format( "following ~B events pending:~s",
							   [ length( EventPairs ),
								 text_utils:strings_to_string( EvStrings ) ] )

	end,

	ActionString = case table:enumerate( ?getAttr(action_table) ) of

		[] ->
			"no pending action";

		ActionPairs ->

			AcStrings = [ text_utils:format( "action #~B: ~s",
							 [ AcId, action_to_string( Action ) ] )
							   || { AcId, Action } <- ActionPairs ],

			text_utils:format( "following ~B actions pending:~s",
							   [ length( ActionPairs ),
								 text_utils:strings_to_string( AcStrings ) ] )

	end,

	text_utils:format( "unit manager linked to experiment manager ~p, "
					   "knowing ~s, ~sListening to ~s; having ~s and "
					   "having ~s",
					   [ ?getAttr(experiment_manager_pid),
						 IdString, UnitTypeString, ClauseString, EventString,
						 ActionString ] ).



% Static section.



% Creates (initially, i.e. before the simulation is started) the specified unit
% managers, supposing here that they accept exactly four construction
% parameters, i.e.:
%
% - the PID of their parent manager
% - the PID of the load balancer
% - a binding_managers record referencing, for each of the supported programming
% languages, the PID of the associated binding manager (if any)
% - the PID of any identification server in use (here: none)
%
% Returns the list of their PID in the same order as the one of their names.
%
% (static method)
%
-spec create_managers( [ wooper:classname() ], experiment_manager_pid(),
					   binding_managers(), load_balancer_pid() ) ->
							 [ unit_manager_pid() ].
create_managers( UnitManagerNames, ExperimentManagerPid, BindingManagers,
				 LoadBalancerPid ) ->

	IdentificationServerPid = undefined,

	create_managers( UnitManagerNames, ExperimentManagerPid, BindingManagers,
					 LoadBalancerPid, IdentificationServerPid ).



% Creates (initially, i.e. before the simulation is started) the specified unit
% managers, supposing here that they accept exactly four construction
% parameters, i.e.:
%
% - the PID of their parent manager
% - the PID of the load balancer
% - a binding_managers record referencing, for each of the supported programming
% languages, the PID of the associated binding manager (if any)
% - the PID of the identification server in use
%
% Returns the list of their PID in the same order as the one of their names.
%
% (static method)
%
-spec create_managers( [ wooper:classname() ], experiment_manager_pid(),
					   binding_managers(), load_balancer_pid(),
					   basic_utils:maybe( identification_server_pid() ) ) ->
							 [ unit_manager_pid() ].
create_managers( UnitManagerNames, ExperimentManagerPid, BindingManagers,
				 LoadBalancerPid, IdentificationServerPid ) ->

	ConstructionParameters = [ ExperimentManagerPid, BindingManagers,
							   LoadBalancerPid, IdentificationServerPid ],

	% By convention the name of a unit manager is its classname:
	[ class_Actor:create_initial_actor( Classname, ConstructionParameters )
	  || Classname <- UnitManagerNames ].



% Requests the synchronous creation by specified unit manager of an initial
% (i.e. not dynamic, at runtime) instance of specified unit type, using
% specified core construction parameters for that, and returns the PID of the
% created unit instance.
%
% Note: only the core, unit-specific construction parameters shall be
% specified; the others (actor-specific ones, dataflow PID, etc.) will be added
% automatically.
%
% For example, for a call to class_Foobar:construct( State, ActorSettings, A, B,
% C, DataflowPid ) to happen, [ A, B, C ] shall be specified as core
% construction parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
% (static method)
%
-spec create_initial_unit( unit_manager_pid(), managed_unit_spec(),
						   dataflow_pid(), construction_parameters() ) ->
								 unit_pid().
create_initial_unit( UnitManagerPid, UnitSpec, DataflowPid,
					 CoreConstructionParameters ) ->

	UnitManagerPid ! { createInitialUnitInstance,
					   [ UnitSpec, DataflowPid, CoreConstructionParameters ],
					   self() },

	receive

		{ wooper_result, UnitPid } when is_pid( UnitPid ) ->
			UnitPid

	end.



-spec create_initial_mockup_unit( unit_manager_pid(), mockup_unit_spec(),
				dataflow_pid(), class_DataflowProcessingUnit:unit_name() ) ->
										[ unit_pid() ].
create_initial_mockup_unit( UnitManagerPid, MockupUnitSpec, DataflowPid,
							UnitName ) ->

	UnitManagerPid ! { createInitialMockupUnitInstance,
					   [ MockupUnitSpec, DataflowPid, UnitName ], self() },

	receive

		{ wooper_result, UnitPid } when is_pid( UnitPid ) ->
			UnitPid

	end.



% Requests the synchronous creations by specified unit manager of a set of
% initial (i.e. not dynamic, at runtime) instances of the specified unit type,
% associated to specified dataflow, using specified list of core construction
% parameters for that, and returns the list of the PIDs of the created unit
% instances, in the order of their construction parameters.
%
% Note: only the core, unit-specific construction parameters shall be specified
% (actor-specific ones, dataflow PID, etc.) will be added automatically.
%
% For example, for a call to class_Foobar:construct( State, ActorSettings, A, B,
% C, DataflowPid ) to happen, [ A, B, C ] shall be specified as core
% construction parameters.
%
% Defined for convenience, typically when implementing a simulation case.
%
% (static method)
%
-spec create_initial_units( unit_manager_pid(), managed_unit_spec(),
							dataflow_pid(), [ construction_parameters() ] ) ->
								  [ unit_pid() ].
create_initial_units( UnitManagerPid, UnitSpec, DataflowPid,
					  CoreConstructionParamLists ) ->

	UnitManagerPid ! { createInitialUnitInstances,
					   [ UnitSpec, DataflowPid, CoreConstructionParamLists ],
					   self() },

	receive

		{ wooper_result, UnitPidList } when is_list( UnitPidList ) ->
			UnitPidList

	end.



-spec create_initial_mockup_units( unit_manager_pid(), mockup_unit_spec(),
			dataflow_pid(), [ class_DataflowProcessingUnit:unit_name() ] ) ->
										 [ unit_pid() ].
create_initial_mockup_units( UnitManagerPid, MockupUnitSpec, DataflowPid,
							 UnitNames ) ->

	UnitManagerPid ! { createInitialMockupUnitInstances,
					   [ MockupUnitSpec, DataflowPid, UnitNames ], self() },

	receive

		{ wooper_result, UnitPidList } when is_list( UnitPidList ) ->
			UnitPidList

	end.



% Creates, at runtime (i.e. in the course of the simulation), a unit of
% specified type (classname), associated with specified dataflow, based on
% specified list of core construction parameters, and returns an updated state.
%
% To be called from an actor, typically a specialised unit manager.
%
% (helper)
%
-spec create_runtime_unit( managed_unit_spec(), dataflow_pid(),
		   [ construction_parameters() ], wooper:state() ) -> wooper:state().
create_runtime_unit( _UnitSpec={ UnitType, erlang }, DataflowPid,
					 CoreConstructionParameters, State ) ->

	?debug_fmt( "Creating a runtime instance of unit type '~s', implemented in "
				"~s, associated to dataflow ~w, based on following core "
				"construction parameters: ~p.",
				[ UnitType, language_utils:language_to_string( erlang ),
				  DataflowPid, CoreConstructionParameters ] ),

	% Building the full construction parameters for the new unit:

	FullConstructParams =
		list_utils:append_at_end( DataflowPid, CoreConstructionParameters ),

	% Returns an updated state; the PID of the created actor will be recorded in
	% onActorCreated/4.
	%
	class_Actor:create_actor( UnitType, FullConstructParams, State );


create_runtime_unit( _UnitSpec={ UnitType, Language }, DataflowPid,
					 CoreConstructionParameters, State ) ->

	?debug_fmt( "Creating a runtime instance of unit type '~s', implemented in "
				"~s, associated to dataflow ~w, based on following core "
				"construction parameters: ~p.",
				[ UnitType, language_utils:language_to_string( Language ),
				  DataflowPid, CoreConstructionParameters ] ),

	% Per-binding generic unit type (ex: class_DataflowPythonProcessingUnit):
	ActualUnitType = dataflow_binding_utils:get_erlang_unit_type( Language ),

	% Binding manager in charge of that language (ex: the PythonBindingManager):
	BindingManagerPid = binding_utils:get_binding_manager( Language,
												?getAttr(binding_managers) ),

	% Building the full construction parameters for the new unit:

	FullConstructParams = [ UnitType, CoreConstructionParameters, DataflowPid,
							BindingManagerPid ],

	% Returns an updated state; the PID of the created actor will be recorded in
	% onActorCreated/4.
	%
	class_Actor:create_actor( ActualUnitType, FullConstructParams, State );


create_runtime_unit( _UnitSpec=UnitType, DataflowPid,
					 CoreConstructionParameters, State ) ->

	FullUnitSpec = { UnitType, _Language=erlang },

	create_runtime_unit( FullUnitSpec, DataflowPid, CoreConstructionParameters,
						 State ).



% Connects and directly (i.e. based on a direct request, not on an actor message
% - thus to be done initially), thanks to the specified unit manager, the named
% output port of the listed upstream blocks to a target port iteration,
% specified thanks to the target (downstream) unit and the name of its
% iteration.
%
% Defined for convenience.
%
% (static method)
%
-spec connect_to_iterated_initially( unit_manager_pid(),
	{ [ upstream_block_pid() ], output_port_string_name() },
	iteration_port_string_target() ) -> basic_utils:void().
connect_to_iterated_initially( UnitManagerPid,
		_SourcePorts={ UpstreamBlocks, OutputPortName },
		_TargetIteration={ DownstreamUnitPid, InputIterationName } ) ->

	BinSourcePorts = { UpstreamBlocks,
					   text_utils:string_to_binary( OutputPortName ) },

	BinTargetIteration = { DownstreamUnitPid,
						   text_utils:string_to_binary( InputIterationName ) },

	UnitManagerPid ! { connectToIteratedInitially,
					   [ BinSourcePorts, BinTargetIteration ], self() },

	receive

		{ wooper_result, connected_to_iterated } ->
			ok

	end.





% Canonicalization section.



% Canonicalizes specified connection specs.
%
% (helper)
%
-spec canonicalize_connection_specs( [ connection_spec() ],
		  upstream_block_pid(), downstream_block_pid(), wooper:state() ) ->
										   [ canonical_connection_spec() ].
canonicalize_connection_specs( ConnectionSpecs, UpstreamBlockPid,
							   DownstreamBlockPid, State )
  when is_list( ConnectionSpecs ) ->
	[ canonicalize_connection_spec( Spec, UpstreamBlockPid, DownstreamBlockPid,
									State ) || Spec <- ConnectionSpecs ];

canonicalize_connection_specs( Other, UpstreamBlockPid, DownstreamBlockPid,
							   State ) ->

	?error_fmt( "Invalid connection specification: ~p (not a list), "
				"from upstream block ~w to downstream one ~w.",
				[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_connection_specs, Other, UpstreamBlockPid,
			 DownstreamBlockPid } ).



% Canonicalizes specified connection spec.
%
% (helper)
%
-spec canonicalize_connection_spec( connection_spec(), upstream_block_pid(),
		 downstream_block_pid(), wooper:state()) -> canonical_connection_spec().
canonicalize_connection_spec( { UpstreamSpec, DownstreamSpec },
							  UpstreamBlockPid, DownstreamBlockPid, State ) ->
	{ canonicalize_upstream_connection_spec( UpstreamSpec, UpstreamBlockPid,
											 DownstreamBlockPid, State  ),
	  canonicalize_downstream_connection_spec( DownstreamSpec, UpstreamBlockPid,
											   DownstreamBlockPid, State  ) };

% A single name means it is to apply to both endpoints:
canonicalize_connection_spec( PortStringName, UpstreamBlockPid,
				  DownstreamBlockPid, State ) when is_list( PortStringName ) ->
	canonicalize_connection_spec( { PortStringName, PortStringName },
								  UpstreamBlockPid, DownstreamBlockPid, State );

canonicalize_connection_spec( Other, UpstreamBlockPid, DownstreamBlockPid,
							  State ) ->

	?error_fmt( "Invalid connection specification: port name '~p' is not a "
				"string (upstream block ~w, downstream one ~w).",
				[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_connection_spec, Other } ).




% Canonicalizes specified upstream connection spec.
%
% (helper)
%
canonicalize_upstream_connection_spec(
  { output_port_name, OutputPortStringName }, _UpstreamBlockPid,
  _DownstreamBlockPid, _State ) when is_list( OutputPortStringName ) ->
	{ output_port_name, text_utils:string_to_binary( OutputPortStringName ) };

canonicalize_upstream_connection_spec( { output_iteration_name,
		   OutputIterationStringName }, _UpstreamBlockPid, _DownstreamBlockPid,
		   _State ) when is_list( OutputIterationStringName ) ->
	{ output_iteration_name,
	  text_utils:string_to_binary( OutputIterationStringName ) };

% Not specified means standard port:
canonicalize_upstream_connection_spec( OutputPortStringName, UpstreamBlockPid,
	   DownstreamBlockPid, State ) when is_list( OutputPortStringName ) ->
	canonicalize_upstream_connection_spec(
	  { output_port_name, OutputPortStringName }, UpstreamBlockPid,
	  DownstreamBlockPid, State );

canonicalize_upstream_connection_spec( Other, UpstreamBlockPid,
									   DownstreamBlockPid, State ) ->

	?error_fmt( "Invalid upstream connection specification: port name '~p' is "
				"not a string (upstream block ~w, downstream one ~w).",
				[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_upstream_connection_spec, Other } ).



% Canonicalizes specified downstream connection spec.
%
% (helper)
%
canonicalize_downstream_connection_spec(
  { input_port_name, InputPortStringName }, _UpstreamBlockPid,
  _DownstreamBlockPid, _State ) when is_list( InputPortStringName ) ->
	{ input_port_name, text_utils:string_to_binary( InputPortStringName ) };

canonicalize_downstream_connection_spec( { input_iteration_name,
		InputIterationStringName }, _UpstreamBlockPid, _DownstreamBlockPid,
		_State  ) when is_list( InputIterationStringName ) ->
	{ input_iteration_name,
	  text_utils:string_to_binary( InputIterationStringName ) };

% Not specified means standard port:
canonicalize_downstream_connection_spec( InputPortStringName, UpstreamBlockPid,
										 DownstreamBlockPid, State )
  when is_list( InputPortStringName )->
	canonicalize_downstream_connection_spec(
	  { input_port_name, InputPortStringName }, UpstreamBlockPid,
	  DownstreamBlockPid, State );

canonicalize_downstream_connection_spec( Other, UpstreamBlockPid,
										 DownstreamBlockPid, State ) ->

	?error_fmt( "Invalid downstream connection specification: port name "
				"'~p' is not a string (upstream block ~w, downstream one ~w).",
				[ Other, UpstreamBlockPid, DownstreamBlockPid ] ),

	throw( { invalid_downstream_connection_spec, Other } ).



% Associates specified action identifier to specified event being processed, by
% returning an updated event table.
%
% (helper)
%
-spec register_action_for_event( action_id(), event_id(), wooper:state() ) ->
									   event_table().
register_action_for_event( ActionId, EventId, State ) ->

	EventTable = ?getAttr(event_table),

	case table:lookupEntry( EventId, EventTable ) of

		{ value, ActionList } ->
			case lists:member( ActionId, ActionList ) of

				false ->
					ok;

				true ->
					throw( { duplicated_action_id, ActionId, EventId,
							 ActionList } )

			end,

			NewActionList = [ ActionId | ActionList ],
			table:addEntry( EventId, NewActionList, EventTable );

		key_not_found ->
			table:addEntry( EventId, _NewActionList=[ ActionId ], EventTable )

	end.



% Returns a (possibly empty) list of the actions associated to specified event.
%
% (helper)
%
-spec get_actions_for_event( event_id(), event_table() ) -> [ action_id() ].
get_actions_for_event( EventId, EventTable ) ->

	% As long as no related action has been declared for an event, that event is
	% not known in the event table:

	case table:lookupEntry( EventId, EventTable ) of

		{ value, ActionList } ->
			ActionList;

		key_not_found ->
			[]

	end.



% to_string section.


% (helper)
-spec connection_specs_to_string( [ canonical_connection_spec() ] ) ->
										string().
connection_specs_to_string( _ConnectionSpecs=[] ) ->
	"empty connection specification";

connection_specs_to_string( ConnectionSpecs ) ->

	ConnectionString = text_utils:strings_to_string(
		  [ connection_spec_to_string( Spec ) || Spec <- ConnectionSpecs ] ),

	text_utils:format( "following ~B connection specifications:~s",
					   [ length( ConnectionSpecs ), ConnectionString ] ).



% (helper)
-spec connection_spec_to_string( canonical_connection_spec() ) -> string().
connection_spec_to_string( { UpstreamPortSpec, DownstreamPortSpec } ) ->
	text_utils:format( "connection from ~s to ~s", [
		upstream_spec_to_string( UpstreamPortSpec ),
		downstream_spec_to_string( DownstreamPortSpec ) ] ).



% (helper)
upstream_spec_to_string( { output_port_name, BinPortName } ) ->
	text_utils:format( "standard output port named '~s'", [ BinPortName ] );

upstream_spec_to_string( { output_iteration_name, BinIterName } ) ->
	text_utils:format( "output port iteration named '~s'", [ BinIterName ] ).



% (helper)
downstream_spec_to_string( { input_port_name, BinPortName } ) ->
	text_utils:format( "standard input port named '~s'", [ BinPortName ] );

downstream_spec_to_string( { input_iteration_name, BinIterName } ) ->
	text_utils:format( "input port iteration named '~s'", [ BinIterName ] ).
