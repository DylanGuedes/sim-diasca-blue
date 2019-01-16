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



% Dataflow mock-up unit class, corresponding to the implementation of the
% mock-up computational parts of a dataflow.
%
% The main purpose of this specialised dataflow processing unit is to emulate
% the inputs/outputs of a real unit, by applying "mock-up clauses" that are
% attached to every mock-up instance during its construction.
%
% Please refer to the 'Sim-Diasca Dataflow HOWTO' for further information.
%
-module(class_DataflowMockupUnit).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_DataflowProcessingUnit ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, MockupUnitName,
		 MockupUnitSpec, DataflowPid ).



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
-define( wooper_method_export, onFirstDiasca/2, activate/1, apply_clauses/1 ).



% Helpers:
-export([ read_mockup_unit_spec/1 ]).


% Design notes:
%
% A mock-up unit shares all the attributes of a classical processing unit, but
% is attached another one, which defines its behaviour everytime it gets
% activated. This new attribute is a list of "mock-up clauses". Each of these
% clauses defines which computations must be made in the activate/1 method (with
% results assigned to output ports) according to the context (the simulation
% time and the values on the input ports).
%
% The application rule is: these clauses are evaluated sequentially, as soon as
% the 'activation_policy' criterion is met (like for any processing unit), and
% the first one matching the context applies its changes. The idea is that a
% user defining a mock-up unit will want to emulate a model which is at least
% partially known. Thus, it is up to the user to define clauses that build a
% unit behaviour close enough to the emulated model.
%
% We introduce at some point the concept of 'variety'. In the current version,
% it is assumed legitimate to define mock-up units that emulate a same real
% model (hence a same class, in the computational sense) but follow different
% behavioral clauses. In other words, we do not consider the list of mock-up
% clauses as a class constant. These clauses are clearly not either
% instance-specific, since we expect that many instances will share a same
% behaviour, designed to be close to what the emulated model would do. Since
% this attribute is neither class-specific nor instance-specific, we say it is
% variety-specific, a mock-up variety being a set of instances sharing the same
% mock-up clauses.
%
% It is still an open question to know whether or not this concept of variety is
% of any use for mock-up units or not. Letting this freedom of specifying
% different clauses for a same unit type (a unit type being a child class of the
% present class) has no impact on the source code contained in this file. It is
% only a question of vocabulary for documentation purposes. The question is to
% know to what extent the association between a unit type and a real model, and
% between a real model and a set of clauses, is close or not.
%


% Current (last) version of the native DUMF file format:
-define( dumf_format_version, "0.3.1" ).


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.MockupUnit" ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For the notify_error_fmt/2 macro:
-include("traces.hrl").


% For mockup_unit_spec:
-include("dataflow_defines.hrl").



% Constructs a new dataflow mock-up unit:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - MockupUnitName is a human-readable name for that mock-up unit (as a plain,
% non-empty string)
%
% - ActivationPolicy is the policy driving the activations of the mock-up unit
%
% - InputPortSpecs is a list of the specifications of the input ports defined
% for this mock-up unit
%
% - OutputPortSpecs is a list of the specifications of the output ports defined
% for this mock-up unit
%
% - MockupClauses is a list of the specifications of the clauses ruling the
% behaviour of this mock-up unit (determining the state of its output ports from
% the simulation time and the state of its input ports)
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 class_DataflowProcessingUnit:unit_name(), mockup_unit_spec(),
				 dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, UnitName,
		   _MockupUnitSpec = #mockup_unit_spec{
								activation_policy=ActivationPolicy,
								input_port_specs=InputPortSpecs,
								output_port_specs=OutputPortSpecs,
								mockup_clauses=MockupClauses },
		   DataflowPid ) ->

	% First, sets the attributes of the direct mother class:
	ProcUnitState = class_DataflowProcessingUnit:construct( State,
						ActorSettings, ?trace_categorize( UnitName ),
						ActivationPolicy, InputPortSpecs, OutputPortSpecs,
						DataflowPid ),

	% Second, checks the validity of the mock-up clauses:
	check_clauses( MockupClauses ),

	% Then sets the class-specific attributes:
	setAttribute( ProcUnitState, mockup_clauses, MockupClauses ).


% Methods section.



% Callback executed on the first diasca of existence of this mock-up unit.
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created ~s.", [ to_string( State ) ] ),

	?wooper_return_state_only( State ).



% Callback executed automatically whenever the mock-up unit is activated.
%
% Meant to be overridden.
%
% (oneway)
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	?trace_fmt( "Evaluating now the ~s.", [ to_string( State ) ] ),

	SetState = apply_clauses( State ),

	?wooper_return_state_only( SetState ).



% Triggers the behaviour of the mock-up unit, based on the registered mock-up
% clauses.
%
% Note: this activation embeds several actions usually defined in the child unit
% classes (designed by a model implementor):
%
% - reading/getting the input port values
%
% - deducing the output values from input ones (this is what really "applying
% the clauses" means)
%
% - setting these output values to the corresponding outputs ports (including
% the creation of the channel values)
%
% Indeed, the genericity of the behaviour of mock-ups (following predefined
% rules) allows the automation of these first and third operations, and the
% execution of the second operation based on data (not code).
%
% (helper)
%
-spec apply_clauses( wooper:state() ) -> oneway_return().
apply_clauses( State ) ->

	MockupClauses = ?getAttr(mockup_clauses),

	% Applies the first matching clause by comparing the MockupInputSpecs with
	% the SimulationTime, the InputPortNames and the InputPortStatuses, and
	% then, if all matching tests succeeded, applies the corresponding
	% MockupOutputSpecs:
	%
	try_clause( MockupClauses, State ).



% Helper section.



% Checking the structure of a set of mock-up clauses.
%
% (helper)
%

% Checks that MockupClauses is indeed a list:
-spec check_clauses( [ mockup_clause() ] ) -> ok.
check_clauses( MockupClauses ) when is_list( MockupClauses ) ->
	check_each_clause( MockupClauses, 1 );

check_clauses( MockupClauses ) ->
	?notify_error_fmt( "The clauses defined for the mock-up unit do not form a "
					   "list:~n~p~n", [ MockupClauses ] ),
	throw( { clauses_are_not_a_list, MockupClauses } ).


% Checks that a particular mock-up clause is a tuple of 3 items:
-spec check_each_clause( [ mockup_clause() ], integer() ) -> ok.
check_each_clause( [], _ClauseCounter ) ->
	ok;

check_each_clause( [ MockupClause | MoreClauses ], ClauseCounter ) when
	  is_tuple( MockupClause ) andalso tuple_size( MockupClause ) =:= 3 ->

	{ TimeSpec, InputMatchSpecs, OutputMatchSpecs } = MockupClause,

	check_time_spec_format( TimeSpec, ClauseCounter ),
	check_input_match_spec_format( InputMatchSpecs, ClauseCounter ),
	check_output_match_spec_format( OutputMatchSpecs, ClauseCounter ),

	check_each_clause( MoreClauses, ClauseCounter+1 );

check_each_clause( [ MockupClause | _MoreClauses ], ClauseCounter ) ->
	?notify_error_fmt( "The ~B-th clause of the current mock-up unit is not "
					   "under the form of a tuple of 3 items:~n~p~n",
					   [ ClauseCounter, MockupClause ] ),
	throw( { clause_is_not_a_tuple, MockupClause } ).


% Checks that the time specification of a clause is of the good type:
-spec check_time_spec_format( clause_time_spec(), integer() ) -> ok.
check_time_spec_format( any_time, _ClauseCounter ) ->
	ok;

check_time_spec_format( TimeSpec, _ClauseCounter ) when
	  is_integer( TimeSpec ) ->
	ok;

check_time_spec_format( TimeSpec, ClauseCounter ) ->
	?notify_error_fmt( "The time specification of the ~B-th clause of the "
					   "current mock-up unit is neither integer nor 'any_time': "
					   "~p~n", [ ClauseCounter, TimeSpec ] ),
	throw( { bad_time_spec, TimeSpec } ).


% Checks that the input match specifications of a clause are valid choices:
-spec check_input_match_spec_format( [ clause_input_match_spec() ],
									 integer() ) -> ok.
check_input_match_spec_format( InputMatchSpecs, ClauseCounter ) when
	  not is_list( InputMatchSpecs ) ->
	?notify_error_fmt( "The input match specifications of the ~B-th clause of "
					   "the current mock-up unit do not form a list:~n~p~n",
					   [ ClauseCounter, InputMatchSpecs ] ),
	throw( { input_match_specs_are_not_a_list, InputMatchSpecs } );

check_input_match_spec_format( [], _ClauseCounter ) ->
	ok;

check_input_match_spec_format( [ _InputMatchSpec = { IPName, IPMatchSpec } |
								 MoreSpecs ], ClauseCounter )
  when is_list( IPName ) ->

	case IPMatchSpec of

		any_state ->
			ok;

		unset ->
			ok;

		set ->
			ok;

		{ set, _Value } ->
			ok;

		{ between, Value1, Value2 } when is_number( Value1 ) andalso
										 is_number( Value2 ) ->
			case Value1 =< Value2 of
				true ->
					ok;
				false ->
					?notify_error_fmt( "Impossible 'between' input match spec "
									   "in the ~B-th clause of the current "
									   "mock-up unit: ~p~n",
									   [ ClauseCounter, IPMatchSpec ] ),
					throw( { bad_input_match_spec, IPMatchSpec } )
			end;

		{ around, Value, Tolerance } when is_number( Value ) andalso
										  is_number( Tolerance ) ->
			ok;

		{ around, Value } when is_number( Value ) ->
			ok;

		{ among, ValuesList } when is_list( ValuesList ) ->
			ok;

		_Else ->
			?notify_error_fmt( "Unknown input match spec encountered in the "
							   "~B-th clause of the current mock-up unit: ~p~n",
							   [ ClauseCounter, IPMatchSpec ] ),
			throw( { bad_input_match_spec, IPMatchSpec } )

	end,

	check_input_match_spec_format( MoreSpecs, ClauseCounter );

check_input_match_spec_format( [ _InputMatchSpec = { IPName, _IPMatchSpec } |
								 _MoreSpecs ], ClauseCounter ) ->
	?notify_error_fmt( "The input port name of an input match specification of "
					   "the ~B-th clause of the current mock-up unit is not a "
					   "string: ~p~n", [ ClauseCounter, IPName ] ),
	throw( { input_port_name_not_a_string, IPName } );

check_input_match_spec_format( [ InputMatchSpec | _MoreSpecs ],
							   ClauseCounter ) ->
	?notify_error_fmt( "An input match specification of the ~B-th clause of "
					   "the current mock-up unit is not well defined: ~p~n",
					   [ ClauseCounter, InputMatchSpec ] ),
	throw( { bad_input_match_spec, InputMatchSpec } ).


% Checks that the output match specifications of a clause are valid choices:
-spec check_output_match_spec_format( [ clause_output_match_spec() ],
									  integer() ) -> ok.
check_output_match_spec_format( OutputMatchSpecs, ClauseCounter ) when
	  not is_list( OutputMatchSpecs ) ->
	?notify_error_fmt( "The output match specifications of the ~B-th clause of "
					   "the current mock-up unit do not form a list:~n~p~n",
					   [ ClauseCounter, OutputMatchSpecs ] ),
	throw( { output_match_specs_are_not_a_list, OutputMatchSpecs } );

check_output_match_spec_format( [], _ClauseCounter ) ->
	ok;

check_output_match_spec_format( [ _OutputMatchSpec = { OPName, OPMatchSpec } |
								  MoreSpecs ], ClauseCounter )
  when is_list( OPName ) ->

	case OPMatchSpec of

		reassign ->
			ok;

		unset ->
			ok;

		{ set, _Value } ->
			ok;

		{ state_of, OutputPortName } when is_list( OutputPortName ) ->
			ok ;

		{ state_of, _Anything } ->
			?notify_error_fmt( "Impossible 'state_of' output match spec in the "
							   "~B-th clause of the current mock-up unit: ~p~n",
							   [ ClauseCounter, OPMatchSpec ] ),
			throw( { bad_output_match_spec, OPMatchSpec } );

		_Else ->
			?notify_error_fmt( "Unknown output match spec encountered in the "
							   "~B-th clause of the current mock-up unit: ~p~n",
							   [ ClauseCounter, OPMatchSpec ] ),
			throw( { bad_output_match_spec, OPMatchSpec } )

	end,

	check_output_match_spec_format( MoreSpecs, ClauseCounter );

check_output_match_spec_format( [ _OutputMatchSpec = { OPName, _OPMatchSpec } |
								  _MoreSpecs ], ClauseCounter ) ->
	?notify_error_fmt( "The output port name of an output match specification "
					   "of the ~B-th clause of the current mock-up unit is not "
					   "a string: ~p~n", [ ClauseCounter, OPName ] ),
	throw( { output_port_name_not_a_string, OPName } );

check_output_match_spec_format( [ OutputMatchSpec | _MoreSpecs ],
								ClauseCounter ) ->
	?notify_error_fmt( "An output match specification of the ~B-th clause of "
					   "the current mock-up unit is not well defined: ~p~n",
					   [ ClauseCounter, OutputMatchSpec ] ),
	throw( { bad_output_match_spec, OutputMatchSpec } ).



% Clause per clause application.
%
% (helper)
%
-spec try_clause( [ mockup_clause() ], wooper:state() ) -> wooper:state().
try_clause( _Clauses=[], State ) ->
	State;

try_clause( [ _Clause={ any_time, InputSpecs, OutputSpecs } | T ], State ) ->
	% Checks the specifications regarding the states of input ports:
	%
	case check_input_specs( InputSpecs, State ) of

		% If they do match in turn, sets the states of output ports
		% according to the corresponding specifications:
		%
		true ->
			apply_output_specs( OutputSpecs, State );

		% If they do not, we try to match next clause (if any):
		false ->
			try_clause( T, State )

	end;

try_clause( [ _Clause={ TimeSpec, InputSpecs, OutputSpecs } | T ], State )
  when is_integer( TimeSpec ) ->

	% Gets the simulation time:
	SimulationStep = class_Actor:getSimulationTickOffset( State ),

	% Compares this time to the one specified in the current clause:
	case TimeSpec of

		% If the time specification is matched, checks the specifications
		% regarding the states of input ports:
		%
		SimulationStep ->

			case check_input_specs( InputSpecs, State ) of

				% If they do match in turn, sets the states of output ports
				% according to the corresponding specifications:
				%
				true ->
					apply_output_specs( OutputSpecs, State );

				% If they do not, we try to match next clause (if any):
				false ->
					try_clause( T, State )

			end;

		% ... otherwise the current clause is simply ignored, and we continue:
		_OtherTime ->
			try_clause( T, State )

	end;

try_clause( [ Clause | _T ], State ) ->

	?error_fmt( "A mock-up clause does not respect the expected structure, "
				"namely a tuple of 3 parameters, the first one (time "
				"specification) being either the atom 'any_time' or an "
				"integer, the latter two being { key, value } entries, with "
				"keys being strings referring a port names:~n~p~n"
				"Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
				"information.", [ Clause ] ),

	throw( { invalid_mockup_clause, Clause } ).



% Checks if the input specifications match the states of the input ports.
%
% This is done recursively over the list of port specifications. Any mismatch
% will break the recursion and will make the function return 'false' (spec not
% matched).
%
% (refer to the Dataflow-HOWTO documentation for more details about each kind of
%  specification)
%
% (helper)
%
-spec check_input_specs( [ clause_input_match_spec() ], wooper:state() ) ->
							   boolean().
check_input_specs( _InputMatchSpecs=[], _State ) ->
	true;

check_input_specs( _InputMatchSpecs=[
			 { InputPortNameSpec, InputPortStatusSpec } | T ], State ) ->

	% Finds an input port with name InputPortNameSpec and gets its status:
	InputPortStatus = class_DataflowBlock:get_input_port_status(
						InputPortNameSpec, State ),

	% Checks if the current input specification matches this status:
	case InputPortStatus of

		unset ->
			check_unset_input_port( InputPortStatusSpec, State )
				andalso check_input_specs( T, State );

		{ set, InputPortValue } ->
			check_set_input_port( InputPortStatusSpec, InputPortValue, State )
				andalso check_input_specs( T, State )

	end.



% Performs the match checking when the input port is unset.
%
% (helper)
%
check_unset_input_port( _InputPortStatusSpec=any_state, _State ) ->
	true ;

check_unset_input_port( _InputPortStatusSpec=unset, _State ) ->
	true;

check_unset_input_port( _InputPortStatusSpec=set, _State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ set, _Value }, _State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ between, _Value1, _Value2 },
						_State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ around, _Value, _Tolerance },
						_State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ around, _Value }, _State ) ->
	false;

check_unset_input_port( _InputPortStatusSpec={ among, _ValueList },
						_State ) ->
	false;

check_unset_input_port( InputPortStatusSpec, State ) ->
	?error_fmt( "Invalid input port specification '~p' encountered. "
				"Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
				"information.", [ InputPortStatusSpec ] ),
	throw( { invalid_mockup_input_spec, InputPortStatusSpec } ).



% Performs the match checking when the input port is set.
%
% (helper)
%
check_set_input_port( _InputPortStatusSpec=any_state, _InputPortValue,
					  _State ) ->
	true;

check_set_input_port( _InputPortStatusSpec=unset, _InputPortValue, _State ) ->
	false;

check_set_input_port( _InputPortStatusSpec=set, _InputPortValue, _State ) ->
	true;

check_set_input_port( _InputPortStatusSpec={ set, Value }, InputPortValue,
					  _State ) ->
	Value == InputPortValue;

check_set_input_port( _InputPortStatusSpec={ between, Value1, Value2 },
					  InputPortValue, _State ) ->
	Value1 =< InputPortValue andalso InputPortValue =< Value2;

check_set_input_port( _InputPortStatusSpec={ around, Value, Tolerance },
					  InputPortValue, _State ) ->
	math_utils:are_relatively_close( InputPortValue, Value, Tolerance );

check_set_input_port( _InputPortStatusSpec={ around, Value }, InputPortValue,
					  _State ) ->
	math_utils:are_relatively_close( InputPortValue, Value );

check_set_input_port( _InputPortStatusSpec={ among, ValueList }, InputPortValue,
					  _State ) ->
	lists:member( InputPortValue, ValueList );

check_set_input_port( InputPortStatusSpec, _InputPortValue, State ) ->
	?error_fmt( "Invalid input port specification ~p encountered. Please refer "
				"to the 'Sim-Diasca Dataflow HOWTO' for further information.",
				[ InputPortStatusSpec ] ),
	throw( { invalid_mockup_input_spec, InputPortStatusSpec } ).



% Sets the specified output values to the specified output ports, as would have
% to do any user-defined model unit inheriting from class_ProcessingUnit.
%
% When successful, this process automatically generates the channel values from
% the specifications used to build the output ports (during the construction of
% the mock-up unit).
%
% (refer to the Dataflow-HOWTO documentation for more details about each kind of
%  specification)
%
% (helper)
%
-spec apply_output_specs( [ clause_output_match_spec() ], wooper:state() ) ->
								wooper:state().
apply_output_specs( _OutputMatchSpecs=[], State ) ->
	State ;


% Sets again the same value (this is not a constant, do-nothing case, since the
% corresponding actor message has to be sent again):
%
apply_output_specs( [ _OutputMatchSpecs={ OutputPortNameSpec,
						   _OutputPortStatusSpec=reassign } | T ], State ) ->

	OutputPortStatus = class_DataflowBlock:get_output_port_status(
						 OutputPortNameSpec, State ),

	case OutputPortStatus of

		unset ->
			apply_output_specs( T, State );

		{ set, OutputPortValue } ->
			SetState = set_output_port_from_name( OutputPortNameSpec,
												  OutputPortValue, State ),
			apply_output_specs( T, SetState )

	end;


% Do-nothing case, leaves the output port in the default unset state:
apply_output_specs( [ _OutputMatchSpecs = { _OutputPortNameSpec,
						   _OutputPortStatusSpec=unset } | T ], State ) ->
	apply_output_specs( T, State );


% In this case, the value to be set is explicitly given:
apply_output_specs( [ _OutputMatchSpecs = { OutputPortNameSpec,
						   _OutputPortStatusSpec={ set, Value } } | T ],
					State ) ->
	SetState = set_output_port_from_name( OutputPortNameSpec, Value, State ),
	apply_output_specs( T, SetState );


% Copies the state of an input port identified by the specified name:
apply_output_specs( [ _OutputMatchSpecs = { OutputPortNameSpec,
						   _OutputPortStatusSpec={ state_of, InputPortName } }
					  | T ], State ) ->

	InputPortStatus = class_DataflowBlock:get_input_port_status(
						InputPortName, State ),

	case InputPortStatus of

		unset ->
			apply_output_specs( T, State );

		{ set, InputPortValue } ->
			SetState = set_output_port_from_name( OutputPortNameSpec,
												  InputPortValue, State ),
			apply_output_specs( T, SetState )

	end;


% Raises an error if the output port specification is not a supported one:
apply_output_specs( [ _OutputMatchSpecs = { OutputPortNameSpec,
						   OutputPortStatusSpec } | _T ], State ) ->

	?error_fmt( "Invalid output port specification ~p for output port ~p. "
				"Please refer to the 'Sim-Diasca Dataflow HOWTO' for further "
				"information.", [ OutputPortStatusSpec, OutputPortNameSpec ] ),

	throw( { invalid_mockup_output_spec, OutputPortStatusSpec } ).



% All-in-one function constructing a channel value from the user-defined port
% description and a value, then using it to set the state of the output port.
%
% (helper)
%
-spec set_output_port_from_name( output_port_string_name(), actual_value(),
								 wooper:state() ) -> wooper:state().
set_output_port_from_name( OutputPortName, Value, State ) ->

	% Looks for an output port with the specified name, and gets its SUTC
	% metadata:
	%
	{ ValueSemantics, ValueUnit, ValueType, _ValueConstraints } =
		class_DataflowBlock:get_output_port_metadata( OutputPortName, State ),

	% Creates the channel value:
	%
	ValueTypeDescription = meta_utils:type_to_description( ValueType ),

	ChannelValue = class_Dataflow:create_channel_value( Value, ValueSemantics,
										ValueUnit, ValueTypeDescription ),

	% Sets the output port with this channel value:
	class_DataflowBlock:set_output_port_value( OutputPortName, ChannelValue,
											   State ).




% Helper functions dealing with the parameters defining mock-up varieties.


% Reads a DUMF file and generates from it a mock-up variety that will be
% instantiated further by adding a name to its attributes.
%
% (helper)
%
-spec read_mockup_unit_spec( file_utils:file_name() ) -> mockup_unit_spec().
read_mockup_unit_spec( DUMFFilename ) ->

	% Checks if the file exists, and tries to read it as a table:
	%
	?notify_trace_fmt( "Reading the DUMF file ~s", [ DUMFFilename ] ),

	AbsolutePath = file_utils:ensure_path_is_absolute( DUMFFilename ),

	case file_utils:is_existing_file_or_link( AbsolutePath ) of

		true ->
			ok;

		false ->
			?notify_error_fmt( "Error: the mock-up specification file ~s could "
							   "not be found on node ~p.",
							   [ AbsolutePath, node() ] ),
			throw( { mockup_variety_file_not_found, AbsolutePath, node() } )

	end,

	MockupSpecTable = try table:new( file_utils:read_terms( AbsolutePath ) ) of
		Table ->
			Table
	catch
		_AnyKind:Exception ->
			?notify_error_fmt( "This DUMF file looks corrupted or incomplete. "
				"Please make sure that every value is in a line, or a block of "
				"lines, which is dot-terminated (one data = one dot). Each of "
				"these values is expected to be a pair of the form '{ key, "
				"Value }' where 'key' is an Erlang atom and 'Value' is any "
				"data type natively supported by Erlang (including basic "
				"containers such as lists and tuples).~n~n~p~n",
							   [ Exception ] ),
			exit( file_consult_failed )
	end,


	% From this table, gets all the necessary information for building the
	% mock-up unit and checks that the description is complete (with metadata)
	% and does not contain unexpected data not detected by the previous step:
	%
	AllExpectedKeys = [ dumf_version, unit_type, mockup_author,
						mockup_author_contact, mockup_version, mockup_date,
						activation_policy, input_port_specs, output_port_specs,
						mockup_clauses ],

	case lists:all( fun( ReadKey ) ->
							lists:member( ReadKey, AllExpectedKeys )
					end,
					table:keys( MockupSpecTable ) ) of
		true ->
			ok;
		false ->
			?notify_error_fmt( "This DUMF file contains a table with either "
							   "corrupted or extra keys. Here is their list: "
							   "~p", [ table:keys( MockupSpecTable ) ] ),
			exit( bad_keys_in_DUMF_table )
	end,

	[ ReadDUMFVersion, ReadUnitType, ReadAuthor, ReadAuthorContact, UnitVersion,
	  ReadDate, ReadActPolicy, ReadIPSpecs, ReadOPSpecs, ReadMockupClauses ] =
		[ parse_mockup_spec_table( Key, MockupSpecTable, AbsolutePath )
		  || Key <- AllExpectedKeys ],

	% Checks that the version of the DUMF format to which the file declares to
	% be conforming is the current (last) version:
	%
	case ReadDUMFVersion of

		?dumf_format_version ->
			ok;

		_AnyOtherVersion ->
			?notify_error_fmt( "This mock-up definition file conforms to an "
							   "unknown or outdated version of the DUMF format:"
							   " ~s instead of ~s",
							   [ ReadDUMFVersion, ?dumf_format_version ] ),
			exit( bad_DUMF_version )

	end,

	% Checks the class name (should begin with 'class_' for consistency):
	%
	StringUnitType = text_utils:atom_to_string( ReadUnitType ),

	UnitType = case text_utils:split_after_prefix( "class_", StringUnitType ) of

		no_prefix ->
			?notify_error_fmt( "Invalid unit type specification: ~p. "
							   "This must be an atom starting with 'class_' "
							   "and statically known from its expected unit "
							   "manager.", [ ReadUnitType ] ),
			throw( { invalid_mockup_unit_type, ReadUnitType } );

		_String ->
			ReadUnitType

	end,

	% Checks if the ActivationPolicy provided is supported:
	%
	ActivationPolicy = class_DataflowProcessingUnit:check_policy(
						 ReadActPolicy ),

	% Gets the specifications of the input and output ports of the targeted
	% mock-up variety.
	%
	% For this, convert both lists, ReadInputPortSpecs and ReadOutputPortSpecs,
	% from two lists of {key,value} lists to two lists of records defined
	% respectively by the input_port_spec() and output_port_spec() types:
	%
	InputPortSpecs = [ class_DataflowBlock:parse_raw_input_port_spec( IPS )
					   || IPS <- ReadIPSpecs ],
	OutputPortSpecs = [ class_DataflowBlock:parse_raw_output_port_spec( OPS )
						|| OPS <- ReadOPSpecs ],

	% Checks then copies, if valid, the mockup clauses from the file:
	%
	check_clauses( ReadMockupClauses ),
	MockupClauses = ReadMockupClauses,

	% Sends the metadata in a trace for information:
	%
	?notify_trace_fmt( "~s: The mock-up specification file ~s has been "
					   "succesfully read. ~s wrote this file on ~s (contact: "
					   "~s). The version of this mock-up variety is ~s and the "
					   "activation policy of the generated units will be: ~p.",
					   [ UnitType, AbsolutePath, ReadAuthor, ReadDate,
						 ReadAuthorContact, UnitVersion, ActivationPolicy ] ),

	% Returns the variety parameters in the form of a relevant record:
	%
	#mockup_unit_spec{
	   unit_type=UnitType,
	   activation_policy=ActivationPolicy,
	   input_port_specs=InputPortSpecs,
	   output_port_specs=OutputPortSpecs,
	   mockup_clauses=MockupClauses }.



% Searches for specified key in specified table.
%
-spec parse_mockup_spec_table( table:key(), [ { atom(), term() } ],
							   file_utils:file_name() ) -> [ table:value() ].
parse_mockup_spec_table( Key, MockupSpecTable, FileName ) ->

	case table:lookupEntry( Key, MockupSpecTable ) of

		{ value, Value } ->
			Value;

		key_not_found ->
			?notify_error_fmt( "Key '~p' not found in mock-up file '~s'.",
							   [ Key, FileName ] ),
			throw( { mockup_key_not_found, Key, FileName } )

	end.




% Helper functions dealing with formatted strings for displays.


% Returns a textual description of a mock-up clause.
%
-spec mockup_clauses_to_string( [ mockup_clause() ] ) -> string().
mockup_clauses_to_string( MockupClauses ) ->

	ClauseStrings = [ text_utils:format( "~p", [ MC ] )
					  || MC <- MockupClauses ],

	text_utils:strings_to_enumerated_string( ClauseStrings ).



% Returns a textual description of this mock-up unit.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } =
		class_DataflowBlock:io_to_string( State ),

	MockupClauses = ?getAttr(mockup_clauses),

	NumberOfClauses = length( MockupClauses ),

	ClauseStrings = mockup_clauses_to_string( MockupClauses ),

	text_utils:format( "Mock-up unit named '~s', applying the '~s' activation "
					   "policy, having ~s and ~s and following ~B clauses:~n~s",
					   [ ?getAttr(name), ?getAttr(activation_policy),
						 InputDetailed, OutputDetailed,
						 NumberOfClauses, ClauseStrings ] ).
