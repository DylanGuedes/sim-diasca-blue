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

% Authors:	Olivier Boudeville (olivier.boudeville@edf.fr)
%			Samuel Thiriot (samuel.thiriot@edf.fr)



% The world manager (WM) is a singleton instance in charge of federating all the
% object managers that account, in a dataflow context, for the simulated world.
%
-module(class_WorldManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, registerObjectManager/2, setExperimentManager/2,
		 registerExperimentEntryPoint/1, registerExperimentExitPoint/1,
		 onFirstDiasca/2, injectChangeset/3, reportChangesetCompletion/4,
		 notifyAllChangesetsInjected/2, checkChangesetsCompletion/2 ).



% Design notes:
%
% The world manager is in charge of all the object managers. It is not itself an
% object manager, as it has sufficiently different features to cover.
%
% This singleton additionally interacts with its computational counterpart, the
% experiment manager (see class_ExperimentManager), which is expected to declare
% itself to this world manager.
%
% We also prefer buffering in the world manager all world events until they are
% all completed, and then only send the whole to the experiment manager, rather
% than streaming them on the fly: the unit managers are then easier to develop,
% as they operate on a stable, final state of the world, rather than having to
% specify the completion of all the events they have to rely on.
%
% As a consequence, this world manager must be told (typically by the experiment
% entry point) that all changesets have been transmitted.
%
% The world manager applies each received changeset directly, and dispatches
% each of the corresponding events to the object manager in charge of it, based
% on the object type specified in the event (for binary association events, the
% dispatching is performed based on the *source* object type).

% Object managers do not send back to the world manager the whole events that
% completed, to avoid sending superfluous information. As a result they send
% back only a pair, made of the identifier of the completed event and of any
% completion-related extra information. This corresponds typically to, for
% example, the PID of a newly created dataflow object (so that the corresponding
% creation event gets complete).

% The extra information that an object manager sends back once a creation event
% has been processed: the PID of a created dataflow object.
%
-type creation_completion_extra_info() :: object_pid().


% The extra information that an object manager sends back once a binary
% association (having thus a source and a target) event has been processed,
% listing (in that order):
%
% - the PID of the source dataflow object of the association
%
% - the PID of the target dataflow object of the association
%
% (no external identifier of the source dataflow object of the association is
% specified, as the information is already provided with

-type binary_association_completion_extra_info() ::
		{ object_pid(), object_pid() }.


% The extra information that an object manager sends back once an association
% event (of any kind) has been processed.
%
-type association_completion_extra_info() ::
		binary_association_completion_extra_info() | any().


% The extra information that an object manager may send back once a world event
% has been processed (ex: the PID of a created dataflow object).
%
-type completion_extra_info() :: 'undefined' | creation_completion_extra_info()
								 | association_completion_extra_info().


-export_type([ completion_extra_info/0 ]).



% Helpers exported for convenience:
%
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.World" ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For types and shorthands:
-include("dataflow_defines.hrl").



% Implementation notes:
%
% The world manager is, directly or not, the parent of all object managers, and
% is an object manager itself.



% To determine the object manager in charge of a given object type, to dispatch
% events:
%
-type object_type_table() :: table:table( dataflow_object_type(),
										  [ object_manager_pid() ] ).



% Table allowing to keep track of the world events that have been dispatched to
% a given object manager (and hence are waited for their report completion).
%
% Note: keeping track of the events *per object manager* is not necessary (a
% direct, plain list of events could have sufficed)
%
-type dispatch_table() :: table:table( object_manager_pid(),
									   [ world_event() ] ).



% A set of dataflow objects that shall be suspended.
%
-type suspend_set() :: [ object_pid() ].


% Allows to maintain, for each of the listed dataflow, which of their (dataflow)
% objects are to be suspended.
%
-type suspend_table() :: list_hashtable:list_hashtable( dataflow_pid(),
														suspend_set() ).



% Attributes that are specific to the world manager are:
%
% - object_type_table :: object_type_table() is an associative table allowing to
% determine the object managers in charge of any given object type (expected to
% be constant)
%
% - object_managers :: [ object_manager_pid() ] is a list of the PID of the
% known object managers
%
% - experiment_manager_pid :: experiment_manager_pid() is the PID of the
% experiment manager, once it has subscribed
%
% - entry_point_pid :: basic_utils:maybe( dataflow_entry_point_pid() is the PID
% of the experiment entry point (if any)
%
% - exit_point_pid :: basic_utils:maybe( dataflow_exit_point_pid() ) is the PID
% of the experiment exit point (if any)
%
% - dispatch_table :: dispatch_table() allows to keep track of which world event
% has been dispatched to which object manager; we consider that all known object
% managers are registered once for all in this table (associated first to an
% empty list); as a result their key is supposed to always exist (hence
% appendToExistingEntry/3 can be used for more checking)
%
% - all_changesets_injected :: boolean() tells whether all changesets have been
% already injected (thus no one is expected anymore)
%
% - last_changeset_injection_tick :: 'undefined' |
% class_TimeManager:tick_offset() records the tick of the last changeset that
% has not been processed yet, so that any (series of) calls to injectChangeset/3
% not followed by one to notifyAllChangesetsInjected/2 can be detected
%
% - completed_events :: [ world_event() ] is a flat list (i.e. with no more
% induced events) of the world events that have been currently reported as done
% by the object managers; will ultimately be sent to the experiment manager
%
% - event_count :: basic_utils:count() is the count maintained by the event
% manager in order to provide a simple identifier to the events being handled;
% it corresponds to the last identifier set (hence the next event is to use this
% value once incremented); events happen to be dispatched in the order of their
% identifier



% Constructs the world manager, from:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
-spec construct( wooper:state(), class_Actor:actor_settings() ) ->
					   wooper:state().
construct( State, ActorSettings ) ->

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( "WorldManager" ) ),

	?send_trace( ActorState, "World manager being created." ),

	naming_utils:register_as( ?world_manager_name, global_only ),

	EmptyTable = table:new(),

	% Then the class-specific actions:
	setAttributes( ActorState, [
		{ object_type_table, EmptyTable },
		{ object_managers, [] },
		{ experiment_manager_pid, undefined },
		{ entry_point_pid, undefined },
		{ exit_point_pid, undefined },
		{ dispatch_table, EmptyTable },
		{ all_changesets_injected, false },
		{ last_changeset_injection_tick, undefined },
		{ completed_events, [] },
		{ event_count, 0 } ] ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	ObjectManagers = ?getAttr(object_managers),

	?trace_fmt( "Being deleted (its ~B object managers will be "
				"automatically be deleted as well).",
				[ length( ObjectManagers ) ] ),

	% No need to remove these managers explicitly, as they are actors (hence
	% will be removed automatically at simulation end):

	State.



% Methods section.


% Callback executed on the first diasca of existence of this manager.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), actor_pid() ) -> oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created ~s.", [ to_string( State ) ] ),

	?wooper_return_state_only( State ).



% Declares the specified experiment manager.
%
% (oneway)
%
-spec setExperimentManager( wooper:state(), experiment_manager_pid() ) ->
		oneway_return().
setExperimentManager( State, ExperimentManagerPid ) ->

	wooper_check_undefined( State, experiment_manager_pid ),

	?trace_fmt( "Experiment manager set, is ~p.", [ ExperimentManagerPid ] ),

	NewState = setAttribute( State, experiment_manager_pid,
							 ExperimentManagerPid ),

	?wooper_return_state_only( NewState ).



% Registers (synchronously) specified (optional) experiment entry point.
%
% (request)
%
-spec registerExperimentEntryPoint( wooper:state() ) ->
						  request_return( 'experiment_entry_point_registered' ).
registerExperimentEntryPoint( State ) ->

	EntryPointPid = ?getSender(),

	% Check:
	undefined = ?getAttr(entry_point_pid),

	NewState = setAttribute( State, entry_point_pid, EntryPointPid ),

	?wooper_return_state_result( NewState, experiment_entry_point_registered ).



% Registers (synchronously) specified (optional) experiment exit point.
%
% (request)
%
-spec registerExperimentExitPoint( wooper:state() ) ->
						  request_return( 'experiment_exit_point_registered' ).
registerExperimentExitPoint( State ) ->

	ExitPointPid = ?getSender(),

	% Check:
	undefined = ?getAttr(exit_point_pid),

	NewState = setAttribute( State, exit_point_pid, ExitPointPid ),

	?wooper_return_state_result( NewState, experiment_exit_point_registered ).



% Registers specified object manager, in charge of the specified object types.
% One might register several managers for the same type; in this case they will
% be randomly used when an event has to be processed. In this case, the object
% manager should obviously be stateless.
%
% (request, to run prior to simulation start)
%
-spec registerObjectManager( wooper:state(),
								  [ dataflow_object_type() ] ) ->
				request_return( 'object_manager_registered' ).
registerObjectManager( State, ManagedObjectTypes ) ->

	% Sitting at the top of the hierarchy, storing all object associations:

	ObjectManagerPid = ?getSender(),

	ObjectManagers = ?getAttr(object_managers),

	NewObjectManagers = case lists:member( ObjectManagerPid, ObjectManagers ) of

		true ->
			throw( { object_manager_already_registered, ObjectManagerPid } );

		false ->
			[ ObjectManagerPid | ObjectManagers ]

	end,

	% Associating each listed type of object to this object manager:
	NewTypeTable = register_type_associations( ManagedObjectTypes,
							   ObjectManagerPid, ?getAttr(object_type_table), State ),


	% Let's populate now the dispatch table accordingly:
	DispatchTable = ?getAttr(dispatch_table),

	NewDispatchTable = table:addNewEntry( ObjectManagerPid,
										  _NoEventYet=[], DispatchTable ),

	NewState = setAttributes( State, [ { object_managers, NewObjectManagers },
									   { object_type_table, NewTypeTable },
									   { dispatch_table, NewDispatchTable } ] ),

	?wooper_return_state_result( NewState, object_manager_registered ).



% Registers the associations regarding specified object types.
%
% (helper)
%
-spec register_type_associations( [ dataflow_object_type() ],
		object_manager_pid(), object_type_table(), wooper:state() ) -> object_type_table().
register_type_associations( _ManagedObjectTypes=[], _ObjectManagerPid,
							TypeTable, _State ) ->
	TypeTable;


register_type_associations( _ManagedObjectTypes=[ ObjectType | T ],
							ObjectManagerPid, TypeTable, State ) ->

	MaybeListOfManagers = table:lookupEntry( ObjectType, TypeTable ),

	case MaybeListOfManagers of

		key_not_found ->

			% we had no list yet for this type of object type
			% ... create a novel list for this type with the novel manager
			NewListOfManagers = [ ObjectManagerPid ],
			% ... add this list in the type table
			TypeTableUpdated = table:addNewEntry( ObjectType, NewListOfManagers, TypeTable ),
			% ... recursively register the remaining types
			register_type_associations( T, ObjectManagerPid, TypeTableUpdated, State );

		{ value, OldListOfManagers } ->

			% we already have a list for this object type

			% Note we do not ensure we are not storing the same manager twice

			% ... add this manager to the ring (in fact it requires constructing a novel ring)
			UpdatedListOfManagers = [ ObjectManagerPid | OldListOfManagers ],

			% ... store this novel list in the type table
			TypeTableUpdated = table:updateEntry( ObjectType, UpdatedListOfManagers, TypeTable ),
			% recursiverly register the remaining types
			register_type_associations( T, ObjectManagerPid, TypeTableUpdated, State )

	end.



% Actor oneways.


% Injects the specified incoming changeset (a list of world events, possibly
% including induced, nested ones).
%
% This changeset is usually directly sent by the experiment entry point.
%
% Any number of calls to this oneway can be done; once all changesets have been
% transmitted, the notifyAllChangesetsInjected/2 oneway shall then be called.
%
% This scheme (multiple changeset injections followed by a final notification)
% has been chosen so that multiple changeset emitters can coexist, and also to
% be able to inject changesets over diascas, so that parallel processing and/or
% step-by-step dataflow modifications can be performed (possibly even from a
% single changeset source, provided it schedules itself from a diasca to
% another).

% (actor oneway)
%
-spec injectChangeset( wooper:state(), changeset(), actor_pid() ) ->
							actor_oneway_return().
injectChangeset( State, Changeset, SendingActorPid ) ->

	CurrentTimestamp = { CurrentTickOffset, _CurrentDiasca } =
		class_Actor:get_current_logical_timestamp( State ),

	NewInjectionTickOffset = case ?getAttr(last_changeset_injection_tick) of

		undefined ->
			CurrentTickOffset;

		% Pattern-matched (already bound):
		CurrentTickOffset ->
			CurrentTickOffset;

		PastTickOffset when PastTickOffset < CurrentTickOffset ->

			?error_fmt( "A changeset for tick offset ~p has just been "
						"injected, whereas the last injected and "
						"not-yet-processed changeset relates to past "
						"tick offset ~p. It is likely that during this "
						"last tick the end of injections has not been "
						"notified to the world manager (lacking call to the "
						"notifyAllChangesetsInjected/2 method?).",
						[ CurrentTickOffset, PastTickOffset ] ),

			throw( { lacking_changeset_notification, PastTickOffset,
					 CurrentTickOffset } );

		% By design FutureTickOffset > CurrentTickOffset:
		FutureTickOffset ->

			?error_fmt( "A changeset for tick offset ~p has just been "
						"injected, whereas the last injected and "
						"not-yet-processed changeset relates to future "
						"tick offset ~p (abnormal).",
						[ CurrentTickOffset, FutureTickOffset ] ),

			throw( { invalid_changeset_notification, FutureTickOffset,
					 CurrentTickOffset } )

	end,


	% Here we set the identifiers of the events and prepare their dispatching:

	LastId = ?getAttr(event_count),

	% First-level induced events to be sent when their parent event will have
	% been fully processed:
	%
	{ IdentifiedChangeset, UpdatedLastId } =
		assign_identifiers_to_top_level_events( Changeset, LastId,
												CurrentTimestamp ),

	?trace_fmt( "Changeset injected by ~w; top-level events have been "
				"identified: ~s", [ SendingActorPid,
									dataflow_support:changeset_to_string(
									  IdentifiedChangeset, _IsVerbose=false ) ] ),

	% Let's then dispatch these top-level events:

	DispatchTable = ?getAttr(dispatch_table),

	{ NewDispatchTable, DispatchState } = dispatch_top_level_events(
							 IdentifiedChangeset, DispatchTable, State ),

	?trace_fmt( "Changeset received from ~w applied, "
				"waiting for its completion.", [ SendingActorPid ] ),

	FinalState = setAttributes( DispatchState, [
						{ event_count, UpdatedLastId },
						{ dispatch_table, NewDispatchTable },
						{ last_changeset_injection_tick,
						  NewInjectionTickOffset } ] ),

	?wooper_return_state_only( FinalState ).



% Assigns identifiers to all top-level events (and only them) in the specified
% changeset.
%
% Also sets timestamps and normalises external identifiers.
%
% (helper)
%
-spec assign_identifiers_to_top_level_events( changeset(), event_id(),
		class_TimeManager:logical_timestamp() ) -> { changeset(), event_id() }.
assign_identifiers_to_top_level_events( Changeset, LastId, CurrentTimestamp ) ->
	assign_identifiers_to_top_level_events( Changeset, LastId, CurrentTimestamp,
											_Acc=[] ).


assign_identifiers_to_top_level_events( _Changeset=[], LastId,
										_CurrentTimestamp, Acc ) ->
	% Preserve event order:
	{ lists:reverse( Acc ), LastId };


% Having to list all kinds of events:
assign_identifiers_to_top_level_events( _Changeset=[ Event=#creation_event{
					id=undefined,
					timestamp=undefined,
					external_id=ExtId } | T ],
										LastId, CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#creation_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 external_id=normalise_external_id( ExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] );


assign_identifiers_to_top_level_events( _Changeset=[ Event=#destruction_event{
					id=undefined,
					timestamp=undefined,
					external_id=ExtId } | T ],
										LastId, CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#destruction_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 external_id=normalise_external_id( ExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] );


assign_identifiers_to_top_level_events( _Changeset=[
				Event=#association_event{
						 id=undefined,
						 timestamp=undefined,
						 external_id=ExtId } | T ],
										LastId, CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#association_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 external_id=normalise_external_id( ExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] );


assign_identifiers_to_top_level_events( _Changeset=[
				Event=#binary_association_event{
						 id=undefined,
						 timestamp=undefined,
						 source_external_id=SourceExtId,
						 target_external_id=TargetExtId } | T ],
										LastId, CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#binary_association_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 source_external_id=normalise_external_id( SourceExtId ),
				 target_external_id=normalise_external_id( TargetExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] );


assign_identifiers_to_top_level_events( _Changeset=[
			Event=#disassociation_event{
					 id=undefined,
					 timestamp=undefined,
					 external_id=ExtId } | T ],
										LastId, CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#disassociation_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 external_id=normalise_external_id( ExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] );


assign_identifiers_to_top_level_events( _Changeset=[
				Event=#connection_event{
						 id=undefined,
						 timestamp=undefined,
						 source_external_id=SourceExtId,
						 target_external_id=TargetExtId } | T ],
										LastId, CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#connection_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 source_external_id=normalise_external_id( SourceExtId ),
				 target_external_id=normalise_external_id( TargetExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] );


assign_identifiers_to_top_level_events( _Changeset=[
			Event=#disconnection_event{ id=undefined,
										timestamp=undefined,
										source_external_id=SourceExtId,
										target_external_id=TargetExtId } | T ],
										LastId, CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#disconnection_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 source_external_id=normalise_external_id( SourceExtId ),
				 target_external_id=normalise_external_id( TargetExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] );


assign_identifiers_to_top_level_events( _Changeset=[
				Event=#update_event{ id=undefined,
									 timestamp=undefined,
									 external_id=ExtId } | T ], LastId,
										CurrentTimestamp, Acc ) ->
	NewId = LastId + 1,

	NewEvent = Event#update_event{
				 id=NewId,
				 timestamp=CurrentTimestamp,
				 external_id=normalise_external_id( ExtId ) },

	assign_identifiers_to_top_level_events( T, NewId, CurrentTimestamp,
											[ NewEvent | Acc ] ).




% Dispatches specified top-level events to their respective object managers,
% prepare to wait for their completion - and send then their induced events (if
% any).
%
% Returns an updated state, comprising an updated list of the identifiers of the
% (top-level) dispatched events (that will be then waited for).
%
% (helper)
%
-spec dispatch_top_level_events( changeset(), dispatch_table(),
				 wooper:state() ) -> { dispatch_table(), wooper:state() }.
dispatch_top_level_events( Changeset, DispatchTable, State ) ->

	ObjectTypeTable = ?getAttr(object_type_table),

	dispatch_top_level_events( Changeset, DispatchTable, ObjectTypeTable,
							   State ).


dispatch_top_level_events( _Changeset=[], DispatchTable, _ObjectTypeTable,
						   State ) ->
	{ DispatchTable, State };

dispatch_top_level_events( _Changeset=[ E | T ], DispatchTable,
						   ObjectTypeTable, State ) ->

	{ UpdatedDispatchTable, SentState } = dispatch_event( E, DispatchTable,
												 ObjectTypeTable, State ),


	dispatch_top_level_events( T, UpdatedDispatchTable, ObjectTypeTable,
							   SentState ).



% Puts specified external identifier in canonical form.
%
% The special case of strings results in returning binaries.
%
% Implemented so that only having strings leads to significant processing
% (typically no real overhead if already binary)
%
-spec normalise_external_id( external_id() ) -> external_id().
normalise_external_id( ExtId ) when is_list( ExtId ) ->

	case text_utils:is_string( ExtId ) of

		true ->
			text_utils:string_to_binary( ExtId );

		false ->
			ExtId

	end;

normalise_external_id( ExtId ) ->
	ExtId.


% Notifies this world manager that all changesets have been transmitted for this
% tick.
%
% Allows to detect, afterwards, that all changesets have been processed,
% allowing to notify in turn the experiment manager with a single, complete list
% of world events.
%
% Note: an issue is to be specifically handled there, as typically there is a
% single changeset emitter which first sends a series of changesets and then
% calls this method - usually all during the same diasca.
% However, due to the automatic message reordering, this world manager will
% receive all the corresponding actor messages in any, arbitrary, order.
% As a result, in the general case, at the diasca at which this method is
% executed, further changesets might still be received afterwards.
%
% A correct design is thus to consider that all changesets have been injected as
% the diasca immediately following the one at which this method is called (hence
% the sending of an actor message from the world manager to itself).
%
% User code is then free to inject changesets and their final notification
% regardless of this issue.
%
% (actor oneway)
%
-spec notifyAllChangesetsInjected( wooper:state(), actor_pid() ) ->
										 actor_oneway_return().
notifyAllChangesetsInjected( State, _SendingActorPid ) ->

	false = ?getAttr(all_changesets_injected),

	?info( "End of changesets notified; when their processing will be "
		   "completed, all unitary changesets will be sent to the "
		   "experiment manager." ),

	NotifiedState = setAttribute( State, all_changesets_injected, true ),

	% Note: the injection of changesets and this notification are all triggered
	% thanks to actor messages, possibly at the same diasca (ex: as a series
	% sent from a single function of the entry point); as a result of message
	% reordering, injection(s) can be processed by this world manager *after*
	% this notification method; this notification shall thus be actually
	% processed at the next diasca, to ensure that no more injections can be
	% expected.
	%
	% We have to ensure that the completion will be checked in all cases,
	% including the one where no changeset at all is injected (in which case the
	% checking that is done once some events are completed is of no use).
	%
	% As a consequence, we plan this delayed checking for the next diasca:
	%
	SelfSentState = class_Actor:send_actor_message( self(),
								checkChangesetsCompletion, NotifiedState ),

	?wooper_return_state_only( SelfSentState ).



% Checks whether at least a world event remains to be fully processed.
%
% Called by the world manager itself, so that this check is done at the diasca
% immediately following the notification that all changesets were injected.

% (actor oneway)
%
-spec checkChangesetsCompletion( wooper:state(), actor_pid() ) ->
										 actor_oneway_return().
checkChangesetsCompletion( State, _SendingActorPid ) ->

	?debug( "Checking the completion of changesets." ),

	% By design true:
	true = ?getAttr(all_changesets_injected),


	% We must handle the case where no changeset was injected, and the one where
	% any number were injected, possibly in previous diascas.

	NewState = case no_more_dispatched_event( ?getAttr(dispatch_table) ) of

		true ->
			% Possibly here no event at all were ever injected, thus no
			% processing thereof result to a call as:
			%
			on_all_changesets_processed( ?getAttr(completed_events), State );

		false ->
			% Then wait for the completion of these events (resulting in a later
			% call to on_all_changesets_processed/2:
			%
			State

	end,

	?wooper_return_state_only( NewState ).




% Reports (from an object manager most probably) that specified events
% (designated by their identifier) have completed (these events have been
% applied for good, opening the possibility of applying their induced events in
% turn), and possibly reports new events that shall be injected as well.
%
% (actor oneway)
%
-spec reportChangesetCompletion( wooper:state(),
	 [ { event_id(), completion_extra_info() } ], changeset(), actor_pid() ) ->
									   class_Actor:actor_oneway_return().
reportChangesetCompletion( State, CompletedEventInfos, InjectedChangeset,
						   SendingObjectManagerPid ) ->

	?void_fmt( "Received a notification of changeset completion "
				"from object manager ~p; the information relative to the "
				"corresponding completed events are ~w, while the newly "
				"injected changeset is: ~s",
				[ SendingObjectManagerPid, CompletedEventInfos,
				  dataflow_support:changeset_to_string( InjectedChangeset,
														_IsVerbose=false) ] ),

	% Based on their IDs, we extract the corresponding waited events, append
	% them to the completed ones, and possibly inject their induced events, if
	% any, augmented with extra events that may be introduced by the object
	% manager (ex: if the creation of a car leads, according to the car object
	% manager, to the creation of 4 wheels):

	DispatchTable = ?getAttr(dispatch_table),

	{ CompletedEvents, ShrunkDispatchTable, SuspendTable } =
		extract_and_complete_events( CompletedEventInfos,
									 SendingObjectManagerPid, DispatchTable ),


	% Let's declare the suspensions to the corresponding dataflows:
	SuspendState = declare_suspensions( SuspendTable, State ),

	% We separate the top-level events (the roots) from their induced ones (the
	% subtrees); a partition a bit like the consing of a forest (a set of trees:
	% [Root|Subtree] for each of them):
	%
	{ BaseCompletedEvents, InducedEvents } = split_events( CompletedEvents ),

	% We have to assign an identifier to the top-level events of both the
	% induced events and the newly injected ones:

	LastId = ?getAttr(event_count),

	% The induced events shall in general be more numerous than the injected
	% ones, and we prefer assigning lower identifiers to the former:
	%
	NewChangeset = InjectedChangeset ++ InducedEvents,

	CurrentTimestamp = class_Actor:get_current_logical_timestamp( State ),

	{ NewChangesetWithIds, UpdatedLastId } =
		assign_identifiers_to_top_level_events( NewChangeset, LastId,
												CurrentTimestamp ),

	{ NewDispatchTable, DispatchedState } = dispatch_top_level_events(
		NewChangesetWithIds, ShrunkDispatchTable, SuspendState ),

	ReadyState = setAttributes( DispatchedState, [
						{ dispatch_table, NewDispatchTable },
						{ event_count, UpdatedLastId } ] ),

	NewCompletedEvents = BaseCompletedEvents ++ ?getAttr(completed_events),

	% We have to wait for all events to complete; none shall arrive next, and
	% all pending ones (decided here or formerly) shall be over:
	%
	FinalState = case ?getAttr(all_changesets_injected)
		andalso no_more_dispatched_event( NewDispatchTable ) of

		% Here no report of changeset completion can be expected anymore:
		true ->

			FullOrderedChangeset = lists:reverse( NewCompletedEvents ),

			on_all_changesets_processed( FullOrderedChangeset, ReadyState );


		false ->
			% Still have to wait for at least one event to complete:
			setAttribute( ReadyState, completed_events, NewCompletedEvents )

	end,

	?wooper_return_state_only( FinalState ).



% Extracts, for a given object manager, the events specified by their identifier
% from the entry of the specified dispatch table (this entry being handled by
% this object manager), and completes them with the specified extra information
% (specific to that event type).
%
% Returns the requested events, an updated dispatch table and a newly created
% suspend table.
%
% (helper)
%
-spec extract_and_complete_events( [ { event_id(), completion_extra_info() } ],
		object_manager_pid(), dispatch_table() ) ->
			{ changeset(), dispatch_table(), suspend_table() }.
extract_and_complete_events( EventInfos, ObjectManagerPid, DispatchTable ) ->

	EventList = table:getEntry( ObjectManagerPid, DispatchTable ),

	{ ExtractedEvents, RemainingEvents, SuspendTable } =
		extract_and_complete_events( EventInfos, EventList, _AccEvents=[],
									 _EmptySuspendTable=list_hashtable:new() ),

	NewDispatchTable = table:addEntry( ObjectManagerPid, RemainingEvents,
									   DispatchTable ),

	{ ExtractedEvents, NewDispatchTable, SuspendTable }.



% (helper)
extract_and_complete_events( _EventInfos=[], EventList, AccEvents,
							 SuspendTable ) ->
	{ AccEvents, EventList, SuspendTable };

extract_and_complete_events( _EventInfos=[ { Id, ExtraInfo } | T ],
									EventList, AccEvents, SuspendTable ) ->

	% Gets the event for which extra information are specified:
	{ TargetEvent, RemainingEventList } = dataflow_support:find_event_by_id( Id,
												EventList ),

	% Applies these information, returns an updated event and suspend table:
	{ FullEvent, NewSuspendedTable } = case finalise_event( TargetEvent,
															ExtraInfo ) of

		{ CompletedEvent, DataflowPid, SuspendSet } ->
			{ CompletedEvent, list_hashtable:appendListToEntry( DataflowPid,
												SuspendSet, SuspendTable ) };

		CompletedEvent ->
			{ CompletedEvent, SuspendTable }

	end,

	extract_and_complete_events( T, RemainingEventList,
								 [ FullEvent | AccEvents ], NewSuspendedTable ).



% Finalises (completes) the specified event with specified extra information: we
% kept the full event here, at the level of the world manager, yet during its
% actual processing (done by an object manager), extra information may be
% available, in which case we update accordingly that reference view on said
% event, so that it becomes complete.
%
% (helper)
%
-spec finalise_event( world_event(), completion_extra_info() ) ->
							{ world_event(), [ object_pid() ] }.
finalise_event( CreationEvent=#creation_event{ object_pid=undefined },
				_CreationExtraInfo=CreatedObjectPid ) ->

	CompletedCreationEvent = CreationEvent#creation_event{
							   object_pid=CreatedObjectPid },

	% Just created dataflow objects automatically consider themselves as
	% suspended, yet their dataflow must be notified of it, so that they can
	% resume them when appropriate.
	%
	% However, no need to declare that suspension here, as when the dataflow
	% object will join the simulation (i.e. when its onFirstDiasca/2 oneway will
	% be executed), it will register to the dataflow, which implies that the
	% dataflow will automatically include it among its suspended blocks.
	%

	% So:
	%{ CompletedCreationEvent, DataflowPid, _SuspendSet=[ CreatedObjectPid ] };
	CompletedCreationEvent;


finalise_event( DestructionEvent=#destruction_event{},
				_DestructionExtraInfo=DestructedObjectPid ) ->
	% No object suspension here:
	DestructionEvent#destruction_event{ object_pid=DestructedObjectPid };

finalise_event( AssociationEvent=#association_event{},
				_AssocExtraInfo=undefined ) ->
	% No object suspension here:
	AssociationEvent;

finalise_event( BinAssociationEvent=#binary_association_event{},
				_BinAssocExtraInfo={ SourceObjectPid, TargetObjectPid } ) ->
	% No object suspension here:
	BinAssociationEvent#binary_association_event{
	  source_object_pid=SourceObjectPid,
	  target_object_pid=TargetObjectPid };

finalise_event( DisassociationEvent=#disassociation_event{},
				_DisassocExtraInfo={ ObjectPid, DisassociationInfo } ) ->
	% No object suspension here:
	DisassociationEvent#disassociation_event{
	  object_pid=ObjectPid,
	  disassociation_information=DisassociationInfo };

finalise_event( ConnectionEvent=#connection_event{},
				_ConnectionExtraInfo=undefined ) ->
	% No object suspension here:
	ConnectionEvent;

finalise_event( DisconnectionEvent=#disconnection_event{},
				_DisconnectionExtraInfo=undefined ) ->
	% No object suspension here:
	DisconnectionEvent;

finalise_event( UpdateEvent=#update_event{
							  object_pid=undefined,
							  dataflow_pid=DataflowPid },
				_UpdateExtraInfo=ObjectPid ) ->

	% As for creations, just updated dataflow objects already recorded that they
	% were suspended, yet their dataflow must be notified of it, so that they
	% can resume them when appropriate.
	%
	% Not done elsewhere, hence done here:
	%
	{ UpdateEvent#update_event{ object_pid=ObjectPid }, DataflowPid,
	  _SuspendSet=[ ObjectPid ] }.



% Declares to specified dataflows (the keys of the table) which of their objects
% (the values) have been suspended (so that they can be resumed later).
%
% (helper)
%
-spec declare_suspensions( suspend_table(), wooper:state() ) -> wooper:state().
declare_suspensions( SuspendTable, State ) ->

	SuspendPairs = list_hashtable:enumerate( SuspendTable ),

	%trace_utils:debug_fmt( "Suspending ~p.", [ SuspendPairs ] ),

	lists:foldl(
	  fun( { DataflowPid, SuspendSet }, AccState ) ->
			  class_Actor:send_actor_message( DataflowPid,
					 { declareSuspendedBlocks, [ SuspendSet ] }, AccState )
	  end,
	  _Acc0=State,
	  _List=SuspendPairs ).



% Partitions specified list of events: returns a pair made of a list of the
% top-level events (stripped from their induced ones) and a (flat) list of their
% induced ones, as subtrees.
%
% (helper)
%
-spec split_events( changeset() ) -> { changeset(), changeset() }.
split_events( Changeset ) ->
	split_events( Changeset, _AccTop=[], _AccInduced=[] ).


split_events( _Changeset=[], AccTop, AccInduced ) ->
	{ AccTop, AccInduced };

% We have to special-case each event type:
split_events( _Changeset=[ E=#creation_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T, [ E#creation_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced );

split_events( _Changeset=[ E=#destruction_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T, [ E#destruction_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced );

split_events( _Changeset=[
					 E=#association_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T, [ E#association_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced );

split_events( _Changeset=[
				 E=#binary_association_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T,
				  [ E#binary_association_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced );

split_events( _Changeset=[
					 E=#disassociation_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T, [ E#disassociation_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced );

split_events( _Changeset=[ E=#connection_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T, [ E#connection_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced );

split_events( _Changeset=[
					 E=#disconnection_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T, [ E#disconnection_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced );

split_events( _Changeset=[ E=#update_event{ induced_events=Induced } | T ],
			  AccTop, AccInduced ) ->
	split_events( T, [ E#update_event{ induced_events=[] } | AccTop ],
				  Induced ++ AccInduced ).



% Predicate to tell whether there is no more already dispatched event in the
% specified table.
%
% (helper)
%
-spec no_more_dispatched_event( dispatch_table() ) -> boolean().
no_more_dispatched_event( DispatchTable ) ->

	% Gathers all event lists (associated to known object managers):
	EventLists = table:values( DispatchTable ),

	are_all_empty( EventLists ).


are_all_empty( [] ) ->
	true;

are_all_empty( [ _EventList=[] | T ] ) ->
	are_all_empty( T );

% At least one list is non-empty then:
are_all_empty( _ ) ->
	false.



% Called whenever all changesets have been fully processed by this world manager
% for this tick.
%
-spec on_all_changesets_processed( changeset(), wooper:state() ) ->
										 wooper:state().
on_all_changesets_processed( FullChangeset, State ) ->

	true = ?getAttr(all_changesets_injected),

	?trace_fmt( "Sending following overall, final, completed, "
				"flat changeset to the experiment manager: ~s",
				[ dataflow_support:changeset_to_string( FullChangeset,
														_IsVerbose=false ) ] ),

	SentState = class_Actor:send_actor_message(
				  ?getAttr(experiment_manager_pid),
				  { notifyFullyCompletedChangeset, [ FullChangeset ] },
				  State ),

	% Prepare for next tick (reset) :
	setAttributes( SentState, [ { completed_events, [] },
								{ all_changesets_injected, false },
								{ last_changeset_injection_tick,
								  undefined } ] ).



% Helper functions.


choose_manager_for_event( ExternalId, ListOfManagerPids ) ->
	% hash the ExternalId
	Hash = erlang:phash2( ExternalId ),
	% keep in our limits
	HashMod = Hash rem length( ListOfManagerPids ),
	% return the nth then
	lists:nth( HashMod+1, ListOfManagerPids ).


% Dispatches the specified world event, by sending it to the right object
% manager (based on its specified object type).
%
% Induced events will be tackled once their parent one will have completed.
%
% (helper)
%
-spec dispatch_event( world_event(), dispatch_table(), object_type_table(),
					  wooper:state() ) -> { dispatch_table(), wooper:state() }.
% We have again to perform roughly the same operation for all event types.
%
% Routing creations:
dispatch_event( Event=#creation_event{
							object_type=ObjectType,
							external_id=ExternalId },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			% fint he
			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),

			?void_fmt( "Creation event for object type '~s' (~s) dispatched "
						"to object manager ~w.", [ ObjectType,
							dataflow_support:world_event_to_string( Event ),
							ObjectManagerPid ] ),

			% No need to send the induced events as well, the whole event is
			% recorded next by this world manager:
			%
			StrippedEvent = Event#creation_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event, the entry already exists by design:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end;


% Routing destructions:
dispatch_event( Event=#destruction_event{
							object_type=ObjectType,
							external_id=ExternalId  },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),

			?void_fmt( "Destruction event for object type '~s' (~s) "
						"dispatched to object manager ~w.",
						[ ObjectType,
						  dataflow_support:world_event_to_string( Event ),
						  ObjectManagerPid ] ),

			% No need to send the induced events as well:
			StrippedEvent = Event#destruction_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end;


% Routing (non-binary) associations:
dispatch_event( Event=#association_event{
						object_type=ObjectType,
						external_id=ExternalId  },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),

			?void_fmt( "Association event for object type '~s' (~s) "
						"dispatched to object manager ~w.", [ ObjectType,
							dataflow_support:world_event_to_string( Event ),
							ObjectManagerPid ] ),

			% No need to send the induced events as well:
			StrippedEvent = Event#association_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end;


% Routing binary associations, based on the *source* event type:
dispatch_event( Event=#binary_association_event{
						source_object_type=ObjectType,
						source_external_id=ExternalId  },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),


			?void_fmt( "Binary association event for object type '~s' (~s) "
						"dispatched to object manager ~w.", [ ObjectType,
							dataflow_support:world_event_to_string( Event ),
							ObjectManagerPid ] ),

			% No need to send the induced events as well:
			StrippedEvent = Event#binary_association_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end;


% Routing disassociations:
dispatch_event( Event=#disassociation_event{
							object_type=ObjectType,
							external_id=ExternalId },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),

			?void_fmt( "Disassociation event for object type '~s' (~s) "
						"dispatched to object manager ~w.", [ ObjectType,
							dataflow_support:world_event_to_string( Event ),
							ObjectManagerPid ] ),

			% No need to send the induced events as well:
			StrippedEvent = Event#disassociation_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end;


% Routing connections (we dispatch them to the object manager in charge of the
% source object):
%
dispatch_event( Event=#connection_event{
							source_object_type=ObjectType,
							source_external_id=ExternalId },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),

			?void_fmt( "Connection event for object type '~s' (~s) dispatched "
						"to object manager ~w.", [ ObjectType,
							dataflow_support:world_event_to_string( Event ),
							ObjectManagerPid ] ),

			% No need to send the induced events as well:
			StrippedEvent = Event#connection_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end;


% Routing disconnections (we dispatch them to the object manager in charge of
% the source object):
%
dispatch_event( Event=#disconnection_event{
							source_object_type=ObjectType,
							source_external_id=ExternalId },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),

			?void_fmt( "Disconnection event for object type '~s' (~s) "
						"dispatched to object manager ~w.", [ ObjectType,
							dataflow_support:world_event_to_string( Event ),
							ObjectManagerPid ] ),

			% No need to send the induced events as well:
			StrippedEvent = Event#disconnection_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end;


% Routing updates:
dispatch_event( Event=#update_event{
							object_type=ObjectType,
							external_id=ExternalId },
				DispatchTable, ObjectTypeTable, State ) ->

	case table:lookupEntry( ObjectType, ObjectTypeTable ) of

		{ value, ListOfManagerPids } ->

			ObjectManagerPid = choose_manager_for_event(
											ExternalId, ListOfManagerPids ),

			?void_fmt( "Update event for object type '~s' (~s) dispatched "
						"to object manager ~w.", [ ObjectType,
							dataflow_support:world_event_to_string( Event ),
							ObjectManagerPid ] ),

			% No need to send the induced events as well:
			StrippedEvent = Event#update_event{ induced_events=[] },
			ManagerChangeset = [ StrippedEvent ],

			SentState = class_Actor:send_actor_message( ObjectManagerPid,
					{ applyChangeset, [ ManagerChangeset ] }, State ),

			% We record the full event:
			NewDispatchTable = table:appendToExistingEntry( ObjectManagerPid,
													   Event, DispatchTable ),

			{ NewDispatchTable, SentState };


		key_not_found ->
			throw( { unregistered_object_type, ObjectType } )


	end.



% Returns a textual description of this world manager.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	ExpString = case ?getAttr(experiment_manager_pid) of

		undefined ->
			"not associated to the experiment manager";

		ExpPid ->
			text_utils:format( "associated to the experiment manager ~w",
							   [ ExpPid ] )

	end,

	ObjectString = case ?getAttr(object_managers) of

		[] ->
			"not registering any object manager";

		ObjectManagers ->
			io_lib:format( "having registered ~B object managers: ~w",
						   [ length( ObjectManagers ), ObjectManagers ] )

	end,

	EntryString = case ?getAttr(entry_point_pid) of

		undefined ->
			"not referencing an experiment entry point";

		EntryPid ->
			text_utils:format( "referencing the experiment entry point ~w",
							   [ EntryPid ] )

	end,

	ExitString = case ?getAttr(exit_point_pid) of

		undefined ->
			"not referencing an experiment exit point";

		ExitPid ->
			text_utils:format( "referencing the experiment exit point ~w",
							   [ ExitPid ] )

	end,

	TypeString = object_type_to_string( ?getAttr(object_type_table) ),

	DispatchSubstrings = [

	  begin

		case DispatchedEvents of

			[] ->
				text_utils:format( "no event dispatched to "
								   "object manager ~w", [ ObjManPid ] );

			_ ->
				SubStrings = [ dataflow_support:world_event_to_string( E )
							   || E <- DispatchedEvents ],

				text_utils:format( "~B events dispatched to "
								   "object manager ~w:~s",
								   [ length( DispatchedEvents ), ObjManPid,
									 text_utils:strings_to_string( SubStrings,
												  _IdentLevel=1)
								   ] )

		end

	  end || { ObjManPid, DispatchedEvents }
				 <- table:enumerate( ?getAttr(dispatch_table) ) ],

	DispatchString = text_utils:strings_to_string( DispatchSubstrings ),


	EventString = case ?getAttr(completed_events) of

		[] ->
			"no world event is reported as completed";

		Events ->
			RevEvents = lists:reverse( Events ),
			EventStrings = [ dataflow_support:world_event_to_string( E )
							 || E <- RevEvents ],
			text_utils:format( "following ~B world events are reported as "
							   "completed:~s", [ length( Events ),
								text_utils:strings_to_string( EventStrings ) ] )

	end,

	text_utils:format( "World manager ~s, ~s, ~s, ~s, managing ~s~n"
					   "Regarding the dispatching of world events: ~s~n"
					   "Regarding the completion of world events: ~s",
					   [ ExpString, ObjectString, EntryString, ExitString,
						 TypeString, DispatchString, EventString ] ).



% Returns a textual description of the object type associations known of this
% world manager.
%
% (helper)
%
-spec object_type_to_string( object_type_table() ) -> string().
object_type_to_string( TypeTable ) ->

	Entries = table:enumerate( TypeTable ),

	case length( Entries ) of

		0 ->
			"no association of object type";

		L ->
			TypeStrings = [ text_utils:format(
							  "type '~s' managed by object manager ~p",
							  [ Type, ManagerPid ] )
							|| { Type, ManagerPid } <- Entries ],

			TypeString = text_utils:strings_to_string( TypeStrings ),

			text_utils:format( "~B associations of object types:~s",
							   [ L, TypeString ] )

	end.
