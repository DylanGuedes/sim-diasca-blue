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



% Dataflow class, in charge of representing a full, overall dataflow.
%
% Please refer to the 'Sim-Diasca Dataflow HOWTO' for further information.
%
-module(class_Dataflow).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, DataflowName,
		 ExperimentManagerPid ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export,
		 registerDataflowObject/3, unregisterDataflowObject/3,
		 registerDataflowUnit/3, unregisterDataflowUnit/3,
		 onFirstDiasca/2, declareSuspendedBlocks/3, resumeSuspendedBlocks/2 ).


% Static  method declarations:
%
-define( wooper_static_method_export, create_channel_value/1,
		 create_channel_value/4, create_direct_channel_value/4 ).




% Design notes:
%
% Dataflow instances shall be created before the simulation is started.


% Name of a dataflow:
-type dataflow_name() :: string().



% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Dataflow-instance" ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For experiment_manager_pid() and all:
-include("dataflow_defines.hrl").



% Shorthands (only in attribute definition):

%-type object_table() :: class_DataflowObjectManager:object_table().
%-type unit_table() :: class_DataflowUnitManager:unit_table().


-type tick_offset() :: class_TimeManager:tick_offset().



% Implementation notes:
%
% We currently demand that suspended blocks are resumed at the same tick as the
% one of their suspension, so that any pending block that is suspended yet not
% adequatly resumed can be detected (instead of being resumed by accident
% later).



% Dataflow-specific attributes are:
%
% - object_table :: object_table() keeps track of all the objects involved in
% that dataflow (which owns them), based on their actual object type
%
% - unit_table :: unit_table() keeps track of all the units involved in that
% dataflow (which owns them), based on their actual unit type
%
% - suspended_blocks :: set_utils:set( block_pid() ) is a set containing the
% blocks of that dataflow that are suspended (and thus that may have to be
% resumed in the future); this is a set and not a list, as a given block shall
% better be resumed (and suspended) only once
%
% - suspension_tick :: basic_utils:maybe( tick_offset() ) records at which tick
% the currently tracked blocks have been suspended; allows to catch suspensed
% blocks that are not resumed appropriately (on time)
%
% - experiment_manager_pid :: pid() is the PID of the experiment manager, which
% drives this dataflow




% Constructs a new dataflow, to account for an actual dataflow:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as assigned by the load balancer
%
% - DataflowName is a human-readable name for that dataflow (as a plain,
% non-empty string)
%
% - ExperimentManagerPid is the PID of the experiment manager
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 dataflow_name(), experiment_manager_pid() ) -> wooper:state().
construct( State, ActorSettings, DataflowName, ExperimentManagerPid ) ->

	% Ensuring a bidirectional connectivity (with a bit of interleaving):
	ExperimentManagerPid ! { registerDataflow, [], self() },

	% First the direct mother class:
	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( DataflowName ) ),

	% End of interleaving:
	receive

		{ wooper_result, dataflow_registered } ->
			ok

	end,

	EmptyTable = table:new(),

	% Then the class-specific actions:
	setAttributes( ActorState, [
		{ object_table, EmptyTable },
		{ unit_table, EmptyTable },
		{ suspended_blocks, set_utils:new() },
		{ suspension_tick, undefined },
		{ experiment_manager_pid, ExperimentManagerPid } ] ).



% Overidden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	Objects = table:values( ?getAttr(object_table) ),

	Units = table:values( ?getAttr(unit_table) ),

	case set_utils:to_list( ?getAttr(suspended_blocks) ) of

		[] ->
			ok;

		StillSuspended ->
			?warning_fmt( "At deletion, there are still ~B suspended blocks: "
						  "~w.", [ length( StillSuspended ), StillSuspended ] )

	end,

	?trace_fmt( "Being deleted, its ~B dataflow objects and ~B dataflow units "
				"will be automatically deleted as well.",
				[ length( Objects ), length( Units ) ] ),

	% No need to do that explicitly, as they are actors (hence will be removed
	% automatically at simulation end):
	%
	%[ ObjectPid ! delete || ObjectPid <- Objects ],
	%[ UnitPid ! delete || UnitPid <- Units ],

	State.




% Methods section.



% Callback executed on the first diasca of existence of this dataflow.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), sending_actor_pid() ) ->
						   actor_oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created ~s.", [ to_string( State ) ] ),

	?wooper_return_state_only( State ).



% Declares a list of suspended blocks, so that they can be resumed later.
%
% (actor oneway)
%
-spec declareSuspendedBlocks( wooper:state(), [ block_pid() ],
							  sending_actor_pid() ) -> actor_oneway_return().
declareSuspendedBlocks( State, SuspendedBlocks, _Sender ) ->

	% Yes, a list, not yet a set:
	?debug_fmt( "Recording the suspension of ~B blocks: ~w.",
				[ length( SuspendedBlocks ), SuspendedBlocks ] ),

	NewSuspensionTick = check_tick_consistency( State ),

	NewSuspendedBlocks = set_utils:add_element_list( SuspendedBlocks,
										 ?getAttr(suspended_blocks) ),

	SuspendedState = setAttributes( State, [
							{ suspended_blocks, NewSuspendedBlocks },
							{ suspension_tick, NewSuspensionTick } ] ),

	?wooper_return_state_only( SuspendedState ).



% Resumes the blocks of this dataflow that were suspended.
%
% (actor oneway)
%
-spec resumeSuspendedBlocks( wooper:state(), sending_actor_pid() ) ->
								   actor_oneway_return().
resumeSuspendedBlocks( State, _Sender ) ->

	% No need to update the suspension tick here (there will be no more
	% suspended blocks):
	%
	_NewSuspensionTick = check_tick_consistency( State ),

	FinalState = case set_utils:to_list( ?getAttr(suspended_blocks) ) of

		[] ->
			?debug( "No suspended block to resume." ),
			State;


		BlocksToResume ->

			% In this trace we reverse the order of this list to preserve the
			% principle of least surprise (even though block order has no impact
			% here):
			%
			?trace_fmt( "Resuming now ~B suspended blocks (~w).",
						[ length( BlocksToResume ),
						  lists:reverse( BlocksToResume ) ] ),

			SentState = class_Actor:send_actor_messages( BlocksToResume, resume,
														 State ),

			setAttribute( SentState, suspended_blocks, set_utils:new() )

	end,

	?wooper_return_state_only( FinalState ).



% Registers the specified dataflow object.
%
% A newly registered dataflow object starts implicitly in the suspended state.
%
% (actor oneway)
%
-spec registerDataflowObject( wooper:state(), wooper:class_name(),
							  object_pid() ) -> actor_oneway_return().
registerDataflowObject( State, Classname, RegisteredObjectPid ) ->

	?void_fmt( "Dataflow registering object ~p of class ~s.",
				[ RegisteredObjectPid, Classname ] ),

	NewSuspensionTick = check_tick_consistency( State ),

	% May create a new entry for this unit type:
	NewObjectTable = table:appendToEntry( Classname, RegisteredObjectPid,
										  ?getAttr(object_table) ),

	NewSuspendedBlocks = set_utils:add( RegisteredObjectPid,
										?getAttr(suspended_blocks) ),

	NewState = setAttributes( State, [
					{ object_table, NewObjectTable },
					{ suspended_blocks, NewSuspendedBlocks },
					{ suspension_tick, NewSuspensionTick } ] ),

	?wooper_return_state_only( NewState ).



% Unregisters the specified dataflow object.
%
% (actor oneway)
%
-spec unregisterDataflowObject( wooper:state(), wooper:class_name(),
								object_pid() ) -> actor_oneway_return().
unregisterDataflowObject( State, Classname, UnregisteredObjectPid ) ->

	?void_fmt( "Dataflow unregistering object ~p of class ~s.",
				[ UnregisteredObjectPid, Classname ] ),

	% May leave an empty entry:
	NewObjectTable = table:deleteFromEntry( Classname, UnregisteredObjectPid,
											?getAttr(object_table) ),

	% May or may not be there:
	NewSuspendedBlocks = set_utils:delete( UnregisteredObjectPid,
										   ?getAttr(suspended_blocks ) ),

	NewState = setAttributes( State, [
					{ object_table, NewObjectTable },
					{ suspended_blocks, NewSuspendedBlocks } ] ),

	?wooper_return_state_only( NewState ).



% Registers the specified dataflow processing unit.
%
% (actor oneway)
%
-spec registerDataflowUnit( wooper:state(), wooper:class_name(), unit_pid() ) ->
								  actor_oneway_return().
registerDataflowUnit( State, Classname, RegisteredUnitPid ) ->

	?void_fmt( "Dataflow registering unit ~p of class ~s.",
				[ RegisteredUnitPid, Classname ] ),

	_NewSuspensionTick = check_tick_consistency( State ),

	% May create a new entry for this unit type:
	NewUnitTable = table:appendToEntry( Classname, RegisteredUnitPid,
										?getAttr(unit_table) ),

	NewState = setAttribute( State, unit_table, NewUnitTable ),

	?wooper_return_state_only( NewState ).



% Unregisters the specified dataflow unit.
%
% (actor oneway)
%
-spec unregisterDataflowUnit( wooper:state(), wooper:class_name(),
							  unit_pid() ) -> actor_oneway_return().
unregisterDataflowUnit( State, Classname, UnregisteredUnitPid ) ->

	?void_fmt( "Dataflow unregistering unit ~p of class ~s.",
				[ UnregisteredUnitPid, Classname ] ),

	% May leave an empty entry:
	NewUnitTable = table:deleteFromEntry( Classname, UnregisteredUnitPid,
										  ?getAttr(unit_table) ),

	% May or may not be there:
	NewSuspendedBlocks = set_utils:delete( UnregisteredUnitPid,
										   ?getAttr(suspended_blocks ) ),

	NewState = setAttributes( State, [
					{ unit_table, NewUnitTable },
					{ suspended_blocks, NewSuspendedBlocks } ] ),

	?wooper_return_state_only( NewState ).



% Helper section.


% Ensures that operations make sense, time-wise.
%
% Returns the current, new suspension tick, once checked.
%
-spec check_tick_consistency( wooper:state() ) -> tick_offset().
check_tick_consistency( State ) ->

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	case ?getAttr(suspension_tick) of

		undefined ->
			CurrentTick;

		CurrentTick ->
			CurrentTick;

		Tick when Tick < CurrentTick ->

			SuspendedSet = ?getAttr(suspended_blocks),

			case set_utils:is_empty( SuspendedSet ) of

				true ->
					CurrentTick;

				false ->

					SuspendedList = set_utils:to_list( SuspendedSet ),

					?error_fmt( "Suspension-related operation triggered, "
								"whereas there were ~B non-resumed blocks "
								"(~w) from a past tick (~p).",
								[ length( SuspendedList ), SuspendedList,
								  Tick ] ),

					throw( { non_resumed_blocks, Tick, SuspendedList } )

			end

		% No tick in the future ever expected.

	end.



% Static section.


% Creates a dataflow value, based on the specified direct value. No metadata is
% specifically set (hence the returned channel value is incomplete).
%
% (static)
%
-spec create_channel_value( actual_value() ) -> channel_value().
create_channel_value( ActualValue ) ->
	#channel_value{ actual_value=ActualValue }.



% Creates a dataflow value that can be conveyed over a channel.
%
% The value unit may be either specified as a (binary) string or as an already
% interpreted pair.
%
% The type is a string description thereof.
%
% (static)
%
-spec create_channel_value( actual_value(), user_value_semantics(),
							unit_utils:unit_bin_string() | value_unit(),
							value_type_description() ) -> channel_value().
create_channel_value( ActualValue, UserSemantics,
					  Unit={ _UnitBinString, _CanonicalUnit },
					  TypeDescription ) when is_list( UserSemantics ) ->

	% We try to check that we create only legit values; currently we go only for
	% the lightest checking:
	%
	% (later we might define a cache that would allow to request servers -
	% semantics, type, etc. - only once thanks to memoization)

	Type = meta_utils:description_to_type( TypeDescription ),

	Semantics = class_SemanticServer:transform_as_internal( UserSemantics ),

	#channel_value{ actual_value=ActualValue, semantics=Semantics,
					unit=Unit, type=Type };

% Here the unit is specified only as a string:
create_channel_value( ActualValue, UserSemantics, UnitString, Type )
  when is_list( UnitString ) ->

	% Expanding here the user-specified unit, once for all:
	UnitBinString = text_utils:string_to_binary( UnitString ),
	CanonicalUnit = unit_utils:parse_unit( UnitString ),
	ValueUnit = { UnitBinString, CanonicalUnit },

	create_channel_value( ActualValue, UserSemantics, ValueUnit, Type ).



% Creates a dataflow value that can be conveyed over a channel, using a
% pre-processed semantics (set of binaries), unit (a pair) and type
% (type-as-term, rather than a type-as-a-string) for that (ex: the one already
% associated to a given port).
%
% Note: low-level direct assignment, minimum checking, for internal use only.
%
% (static)
%
-spec create_direct_channel_value( actual_value(), value_semantics(),
				  value_unit(), value_type() ) -> channel_value().
create_direct_channel_value( ActualValue, Semantics,
	  Unit={ _UnitBinString, _CanonicalUnit }, ActualType ) ->

	#channel_value{ actual_value=ActualValue, semantics=Semantics,
					unit=Unit, type=ActualType }.




% Returns a textual description of this dataflow.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	ObjectString = case table:enumerate( ?getAttr(object_table) ) of

		[] ->
			"not referencing any dataflow object";

		ObjectPairs ->
			ObjectListString = text_utils:strings_to_string(
				[ text_utils:format( "~B instances of object type '~s': ~w",
									 [ length( ObjList ), ObjType, ObjList ] )
				  || { ObjType, ObjList } <- ObjectPairs ] ),
			text_utils:format( "referencing ~B types of dataflow objects: ~s",
							   [ length( ObjectPairs ), ObjectListString ] )

	end,

	UnitString = case table:enumerate( ?getAttr(unit_table) ) of

		[] ->
			"not referencing any dataflow unit";

		UnitPairs ->
			UnitListString = text_utils:strings_to_string(
				[ text_utils:format( "~B instances of unit type '~s': ~w",
									 [ length( ObjList ), ObjType, ObjList ] )
				  || { ObjType, ObjList } <- UnitPairs ] ),
			text_utils:format( "referencing ~B types of dataflow units: ~s",
							   [ length( UnitPairs ), UnitListString ] )

	end,

	SuspendString = case set_utils:to_list( ?getAttr(suspended_blocks) ) of

		[] ->
			"not recording any block suspension";

		SuspendedBlocks ->
			text_utils:format( "recording ~B block suspensions (~w)",
							   [ length( SuspendedBlocks ), SuspendedBlocks ] )

	end,

	ExpString = text_utils:format( "linked to experiment manager ~w",
								   [ ?getAttr(experiment_manager_pid) ] ),

	text_utils:format( "Dataflow '~s', ~s, ~s, ~s, and ~s",
				   [ ?getAttr(name), ObjectString, UnitString, SuspendString,
					 ExpString ] ).
