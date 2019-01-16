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



% Base class for all the Python-based processing units, i.e. units that rely on
% a Python implementation through our binding.
%
-module(class_DataflowPythonProcessingUnit).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_DataflowProcessingUnit ] ).


% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, UnitClassname,
		 PythonConstructionParameters, DataflowPid, PythonBindingManagerPid ).


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
-define( wooper_method_export, activate/1 ).

-define( wooper_static_method_export, get_port_specifications/1,
		 get_declared_semantics/1, get_declared_types/1 ).


% Designates the Python reference of a processing unit instance (relatively to
% its interpreter):
%
-type python_ref() :: basic_utils:count().


-export_type([ python_ref/0 ]).


% Helpers:
-export([ get_python_module_and_class_for/1, to_string/1 ]).



-type binary_value_semantics() :: binary().

-type binary_value_unit() :: binary().

-type binary_value_type() :: binary().

% A list of the changes to perform on output ports after the computations of the
% activate/1 method made in a Python interpreter:
-type activation_result() :: { output_port_name(),
							   { actual_value(), binary_value_semantics(),
								 binary_value_unit(), binary_value_type() } }.

-type activation_results() :: [ activation_result() ].



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.Bindings.PythonProcessingUnit" ).


% Allows to use macros for sending traces:
-include("class_TraceEmitter.hrl").

% Allows to use macros for sending standalone traces:
-include("traces.hrl").


% For types and shorthands related to dataflows:
-include("dataflow_defines.hrl").

% For types and shorthands related to bindings:
-include("bindings.hrl").


% For input_port and all:
-include("class_DataflowBlock_defines.hrl").



% Design notes:
%
% Each of these processing units uses the Python binding API in order to rely on
% a Python-based implementation: this type of processing unit is an Erlang
% lightweight, mostly empty shell that simply defers its actual processing to a
% Python interpreter, and reinjects back its results in the simulation by
% modifying its usual unit state accordingly.


% Implementation notes:
%
% Operations (such as construction or activation) are thus deferred to the
% related Python interpreter, and their result is then waited for by this Erlang
% unit counterpart.




% The attributes specific to Python a processing unit are:
%
% - python_interpreter_pid :: python_utils:interpreter_pid() is the PID of the
% Python interpreter in which the Python processing unit has been instantiated
%
% - python_module :: file_utils:bin_file_name() is the name of the Python
% module, as a filename including the '.py' extension, in which this processing
% unit is implemented
%
% - python_class :: text_utils:bin_string() is the name of the Python class
% providing the actual implementation of this processing unit (as a Python PEP8
% classname - yet not as an atom)
%
% - python_instance_ref :: python_ref() is the reference that identifies the
% Python processing unit in its interpreter




% Constructs a new dataflow Python processing unit:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - UnitClassname is the (WOOPER) classname of this unit (its Python counterpart
% must be available); ex: 'class_TransportationDemandUnit'
%
% - PythonConstructionParameters is the list of the construction parameters that
% will be used to instantiate the corresponding Python processing unit instance
% in its interpreter; the first argument must be the name of this unit, which is
% also stored in the (Erlang) actor state
%
% - DataflowPid is the PID identifying the dataflow to which this processing
% unit belongs
%
% - PythonBindingManagerPid is the PID of the Python runtime manager in charge
% of that actor
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 dataflow_unit_type(), construction_parameters(),
				 dataflow_pid(), python_binding_manager_pid() ) ->
					   wooper:state().
construct( State, ActorSettings, UnitClassname,
		   PythonConstructionParameters=[ UnitName | OtherParams ],
		   DataflowPid, PythonBindingManagerPid ) ->

	% Deduces from the specified (WOOPER) classname the name of the:
	%  - Python package (directory)
	%  - module (filename)
	%  - class of the instance being constructed, from the declared classname:

	{ PythonUnitModule, PythonUnitClassname } = get_python_module_and_class_for(
												  UnitClassname ),

	% Gets the Python interpreter in charge of that unit:
	InterpreterPid = class_PythonBindingManager:get_interpreter(
					   PythonBindingManagerPid ),

	% Instantiates the Python processing unit in its interpreter, and gets back
	% the initial data (attributes or not) needed in the Erlang world:
	%
	PythonInitialData = [ PythonUnitModule, PythonUnitClassname ]
		++ PythonConstructionParameters,

	TraceEmitterInfo = ?trace_categorize( UnitName ),

	ErlangInitialData = python_binding_utils:execute_request( InterpreterPid,
		instantiate_unit, PythonInitialData, TraceEmitterInfo ),

	% Separates and decodes the attributes received from the Python world, in
	% standard (Erlang) dataflow terms:
	%
	% (activation policy downcased in the encode/1 method of the
	% AutoHidingEnumprocess)
	%
	{ PythonInstanceRef, BinActivationPolicy, EncodedInputPortSpecs,
	  EncodedOutputPortSpecs } = ErlangInitialData,

	%trace_utils:debug_fmt( "Instance of the Python-based unit of class "
	%					   "'~s' (defined in module '~p') created by its "
	%					   "interpreter: its reference is ~p, "
	%					   "its activation policy is ~p.",
	%					   [ PythonUnitClassname, PythonUnitModule,
	%						 PythonInstanceRef, BinActivationPolicy ] ),

	ActivationPolicy = text_utils:binary_to_atom( BinActivationPolicy ),

	InputPortSpecs = [ begin
						   DecodedIPS = decode_input_port_specs( EIPS ),
						   class_DataflowBlock:parse_raw_input_port_spec(
							 DecodedIPS )
					   end || EIPS <- EncodedInputPortSpecs ],

	OutputPortSpecs = [ begin
							DecodedOPS = decode_output_port_specs( EOPS ),
							class_DataflowBlock:parse_raw_output_port_spec(
							  DecodedOPS )
						end || EOPS <- EncodedOutputPortSpecs ],

	% Constructs then the corresponding direct mother class:
	%
	UnitState = class_DataflowProcessingUnit:construct( State, ActorSettings,
					TraceEmitterInfo, ActivationPolicy,
					InputPortSpecs, OutputPortSpecs, DataflowPid ),

	?send_debug_fmt( UnitState, "Created a Python-based processing unit "
					 "instance named '~s' (extra construction parameters: ~p) "
					 "of type '~s' (corresponding in Python to classname '~s' "
					 "of module '~s', relying on the (Python) binding manager "
					 "~w), in the context of dataflow ~p.",
					 [ UnitName, OtherParams, UnitClassname,
					   PythonUnitClassname, PythonUnitModule,
					   PythonBindingManagerPid, DataflowPid ] ),

	% Sets the child-specific attributes:
	%
	setAttributes( UnitState, [
		{ python_interpreter_pid, InterpreterPid },
		{ python_module, PythonUnitModule },
		{ python_class, PythonUnitClassname },
		{ python_instance_ref, PythonInstanceRef } ] ).




% Methods section.



% Callback executed automatically whenever the processing unit is activated.
%
% Meant to be overridden.
%
% (oneway)
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	PythonClass = ?getAttr(python_class),

	?trace_fmt( "Activating and evaluating now the unit named '~s' "
				"of type '~s' (implemented as Python class '~s').",
				[ ?getAttr(name),
				  wooper_utils:pep8_class_to_wooper_class( PythonClass ),
				  PythonClass ] ),

	% Gathers the pieces of simulation context that could be of any use during
	% the computations involved in the Python activate/1 method:
	%
	{ _State, SimulationDate } = executeRequest( State, getSimulationDate ),

	SimulationData = { ?getAttr(current_tick_offset), SimulationDate },

	% Builds the exhaustive list of all port statuses (with their names):
	InputPortStatuses = get_encoded_input_ports_data( State ),

	% Builds the list of encoded data relative to input port iterations that
	% might be necessary for computations involved in the Python activate/1
	% method:
	%
	InputPortIterationPieces = get_encoded_input_port_iterations_data( State ),

	% builds the list of encoded data relative to output port iterations that
	% might be necessary for the unit to know how many output ports really
	% are instantiated
	OutputPortIterationPieces =
		get_encoded_output_port_iterations_data( State ),

	% Gathers all relevant input data, calls the activate/1 method of the
	% associated Python processing unit and gets back its computation results:

	InterpreterPid = ?getAttr(python_interpreter_pid),

	PythonActivationData = [ ?getAttr(python_instance_ref), SimulationData,
							 InputPortStatuses,
							 InputPortIterationPieces,
							 OutputPortIterationPieces ],

	ActivationResults = python_binding_utils:execute_request(
						  InterpreterPid, activate_unit, PythonActivationData,
						  State ),

	% Interprets then ActivationResults as a list of tasks to achieve on the
	% output ports, then performs them:
	%
	FinalState = apply_activation_results( ActivationResults, State ),

	?wooper_return_state_only( FinalState ).



% Helper functions.


% Builds the exhaustive list of all port statuses, with their names.
%
-spec get_encoded_input_ports_data( wooper:state() ) ->
									[ { input_port_name(), value_status() } ].
get_encoded_input_ports_data( State ) ->

	% Gets the complete table of input ports:
	InputPortTable = ?getAttr(input_ports),

	% Gets the names of all ports, in binary format (thus already encoded), as
	% the keys of this table:
	%
	InputPortBinNames = table:keys( InputPortTable ),

	% Zips these encoded port names with the associated port statuses:
	%
	[ begin

		  InputPortStatus = class_DataflowBlock:get_input_port_status(
												   InputPortBinName, State ),

		  { InputPortBinName, InputPortStatus }

	  end || InputPortBinName <- InputPortBinNames ].



% Builds the encoded list of all input data needed for activation and related to
% port iterations.
%
-spec get_encoded_input_port_iterations_data( wooper:state() ) ->
	[ { input_port_name(), iteration_multiplicity(), [ iterated_index() ] } ].
get_encoded_input_port_iterations_data( State ) ->

	% Builds the exhaustive list of all input port iterations (their current
	% states):
	InputPortIterationTable = ?getAttr(input_iterations),
	AllInputPortIterations = table:values( InputPortIterationTable ),

	% Returns, for each of these iterations, the data potentially relevant for
	% the computations made in the Python 'activate' method:
	%
	[ { InputPortIteration#input_port_iteration.base_name,
		InputPortIteration#input_port_iteration.multiplicity,
		InputPortIteration#input_port_iteration.port_indexes }

	  || InputPortIteration <- AllInputPortIterations ].


-spec get_encoded_output_port_iterations_data( wooper:state() ) ->
	[ { input_port_name(), iteration_multiplicity(), [ iterated_index() ] } ].
get_encoded_output_port_iterations_data( State ) ->

	% Builds the exhaustive list of all port iterations (their current states):
	OutputPortIterationTable = ?getAttr(output_iterations),
	AllOutputPortIterations = table:values( OutputPortIterationTable ),

	% Returns, from these iterations, the data potentially relevant for the
	% computations made in the Python 'activate' method:
	[ { OutputPortIteration#output_port_iteration.base_name,
		OutputPortIteration#output_port_iteration.multiplicity,
		OutputPortIteration#output_port_iteration.port_indexes }

	  || OutputPortIteration <- AllOutputPortIterations ].



% Interprets the specified activation results as a list of tasks to achieve on
% the output ports, then performs them.
%
-spec apply_activation_results( activation_results(), wooper:state() ) ->
									  wooper:state().
apply_activation_results( ActivationResults, State ) ->

	% Separates the names of ports from the data needed for creating the channel
	% values:
	%
	{ BinOutputPortNames, ChannelValueData } = lists:unzip( ActivationResults ),


	% Creates the channel values by decoding the fetched data:
	%
	ChannelValues = [ begin

		  StringSems = [ text_utils:binary_to_string( Sem ) || Sem <- BinSems ],

		  Unit = text_utils:binary_to_string( BinUnit ),
		  Type = text_utils:binary_to_string( BinType ),

		  class_Dataflow:create_channel_value( Value, StringSems, Unit, Type )

		  end || { Value, BinSems, BinUnit, BinType } <- ChannelValueData ],

	% Sets the channel values on the corresponding output ports:
	OutputPortPairs = lists:zip( BinOutputPortNames, ChannelValues ),

	class_DataflowBlock:set_output_port_values( OutputPortPairs, State ).



% Static section.


% Returns the specifications for the input and output ports of that dataflow
% processing unit.
%
% (static)
%
-spec get_port_specifications( dataflow_unit_type() ) ->
	{ [ input_port_spec() ], [ output_port_spec() ] }
		| 'no_port_specifications_declared'.
get_port_specifications( UnitType ) ->

	{ PythonModule, PythonClass } = get_python_module_and_class_for( UnitType ),

	% Requests the encoded port specifications from this Python module:
	%
	RequestResult = python_binding_utils:execute_request_locally(
					  get_port_specifications, [ PythonModule, PythonClass ],
					  ?trace_emitter_categorization ),

	trace_utils:debug_fmt( "get_port_specifications returned '~s' type (~s).",
				   [ meta_utils:get_type_of( RequestResult ), RequestResult ] ),

	% Returns the 'no_port_specifications_declared' atom if no static port
	% declaration has been found in Python, otherwise decodes the specifications
	% received:
	%
	case RequestResult of

		Atom=no_port_specifications_declared ->
			Atom;

		"no_port_specifications_declared" ->
			no_port_specifications_declared;

		<<"no_port_specifications_declared">> ->
			no_port_specifications_declared;


		{ EncodedInputPortSpecs, EncodedOutputPortSpecs } ->

			InputPortSpecs = [ begin
							   DecodedIPS = decode_input_port_specs( EIPS ),
							   class_DataflowBlock:parse_raw_input_port_spec(
								 DecodedIPS )
							   end || EIPS <- EncodedInputPortSpecs ],

			OutputPortSpecs = [ begin
								DecodedOPS = decode_output_port_specs( EOPS ),
								class_DataflowBlock:parse_raw_output_port_spec(
								  DecodedOPS )
								end || EOPS <- EncodedOutputPortSpecs ],

			{ InputPortSpecs, OutputPortSpecs }

	end.



% Returns the semantics statically declared by this unit.
%
% Defining this method allows to ensure that all the ports ever created by this
% unit will use semantics among this explicitly stated list.
%
% Otherwise the list would be deduced from the initial port specifications, with
% no specific control.
%
-spec get_declared_semantics( dataflow_unit_type() ) ->
		class_SemanticServer:vocabulary() | 'no_semantics_declared'.
get_declared_semantics( UnitType ) ->

	{ PythonModule, PythonClass } = get_python_module_and_class_for( UnitType ),

	% Requests the encoded semantics usable for dataflow ports and channels from
	% this Python module:
	%
	RequestResult = python_binding_utils:execute_request_locally(
					  get_declared_semantics, [ PythonModule, PythonClass ],
					  ?trace_emitter_categorization ),

	trace_utils:debug_fmt( "get_declared_semantics returned '~s' type (~p).",
			   [ meta_utils:get_type_of( RequestResult ), RequestResult ] ),

	% Returns the 'no_semantics_declared' atom if no static declaration of
	% semantics has been found in Python, or decodes the received one otherwise:
	%
	case RequestResult of

		Atom=no_semantics_declared ->
			Atom;

		% Mimics the behaviour of the 'get_and_trigger' methods in
		% class_DataflowBlock.erl, for which empty declarations are equivalent
		% to no declaration at all:
		[] ->
			no_semantics_declared;


		"no_semantics_declared" ->
			no_semantics_declared;

		<<"no_semantics_declared">> ->
			no_semantics_declared;

		EncodedSemantics when is_list( EncodedSemantics ) ->
			[ text_utils:binary_to_atom( BinSem )
			  || BinSem <- EncodedSemantics ]

	end.



% Returns the types statically declared by this unit.
%
-spec get_declared_types( dataflow_unit_type() ) -> 'no_types_declared' |
												class_TypeServer:type_entries().
get_declared_types( UnitType ) ->

	{ PythonModule, PythonClass } = get_python_module_and_class_for( UnitType ),

	% Requests the encoded strings describing the types accepted by each port,
	% from this Python module:
	%
	RequestResult = python_binding_utils:execute_request_locally(
					  get_declared_types, [ PythonModule, PythonClass ],
					  ?trace_emitter_categorization ),

	% Returns the 'no_types_declared' atom if no static declaration of value
	% types has been found in Python, or decodes the received one otherwise:
	%
	case RequestResult of

		Atom=no_types_declared ->
			Atom;

		% Mimics the behaviour of the 'get_and_trigger' methods in
		% class_DataflowBlock.erl, for which empty declarations are equivalent
		% to no declaration at all:
		[] ->
			no_types_declared;

		"no_types_declared" ->
			no_types_declared;

		<<"no_types_declared">> ->
			no_types_declared;

		EncodedTypes when is_list( EncodedTypes ) ->
			[ { text_utils:binary_to_atom( BinTypeName ),
				text_utils:binary_to_string( BinExplicitType ) }
			  || { BinTypeName, BinExplicitType } <- EncodedTypes ]

	end.




% Helpers section.



% Returns the Python module and class that correspond to the specified (Erlang)
% unit type, i.e. a WOOPER classname, like
% 'class_BigPackage__MyPackage__MyExample', resulting in:
% { 'big_package.my_package.my_example', 'MyExample' }.
%
-spec get_python_module_and_class_for( dataflow_unit_type() ) ->
		{ python_utils:pep8_class_module(), python_utils:pep8_class_name() }.
get_python_module_and_class_for( UnitType ) ->

	% For instance, let's suppose UnitType = "class_Package__Module".

	% Then PythonPackageAndClass = 'Package__Module':
	PythonPackageAndClass = wooper_utils:wooper_class_to_pep8_class( UnitType ),

	StrPythonPackageAndClass = text_utils:atom_to_string(
								 PythonPackageAndClass ),

	% [ "Package", "Module" ]:
	SplitElems = string:split( StrPythonPackageAndClass, _Pattern="__",
							   _Where=all ),

	% Then PythonClass = 'Module':
	PythonClass = text_utils:string_to_atom( lists:last( SplitElems ) ),

	% Then PythonModule = "package_.module"
	PythonModule = python_utils:pep8_class_to_pep8_module(
					 PythonPackageAndClass ),

	{ PythonModule, PythonClass }.



% Turns all the binaries in the encoded input port specs (keys and values as
% well) into the types expected by the 'parse_raw_input_port_spec' function.
%
-spec decode_input_port_specs( [ { binary(), term() } ] ) ->
									 [ { atom(), term() } ].
decode_input_port_specs( EncodedInputPortSpecs ) ->

	% Grabs the values and converts them one by one, according to their meaning:
	decode_input_port_specs_values( EncodedInputPortSpecs, _Acc=[] ).



% (helper)
decode_input_port_specs_values( _Proplist=[], Acc ) ->
	lists:reverse( Acc );

decode_input_port_specs_values( [ { <<"name">>, BinName } | T ], Acc )
  when is_binary( BinName ) ->

	NewAcc = [ { input_port_name, text_utils:binary_to_string( BinName ) }
			   | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ { <<"comment">>, BinComment } | T ], Acc )
  when is_binary( BinComment ) ->

	NewAcc = [ { comment, text_utils:binary_to_string( BinComment ) } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ _IterSpec={ <<"is_iteration">>, IterSpecVal }
								  | T ], Acc ) ->

	% An iteration specification from Python can only be a boolean, or an int,
	% or a tuple, or imbricated tuples containing integers.
	%
	% All of these possible types of specification are transmitted as they are
	% in Erlang, and do not need any conversion:
	%
	decode_input_port_specs_values( T, [ { is_iteration, IterSpecVal }
										 | Acc ] );

decode_input_port_specs_values( [ { <<"value_semantics">>, BinSemList } | T ],
								Acc ) when is_list( BinSemList ) ->

	StringSems = [ text_utils:binary_to_string( BinSem ) ||
					 BinSem <- BinSemList ],

	NewAcc = [ { value_semantics, StringSems } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ { <<"value_unit">>, BinUnit } | T ], Acc )
  when is_binary( BinUnit ) ->

	NewAcc = [ { value_unit, text_utils:binary_to_string( BinUnit ) } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ { <<"value_type_description">>, BinTypeDesc }
								  | T ], Acc )
  when is_binary( BinTypeDesc ) ->

	NewAcc = [ { value_type_description,
				 text_utils:binary_to_string( BinTypeDesc ) } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


% Processes the specific case of the "in" constraint, which takes a list as
% parameters:
%
decode_input_port_specs_values( [
	  { <<"value_constraints">>, [ { <<"in">>, BinConstraints } ] } | T ], Acc )
  when is_list( BinConstraints ) ->

	% Checking:
	case lists:all( fun( C ) -> is_binary( C ) end, BinConstraints ) of

		true ->
			ok;

		false ->
			?notify_error_fmt( "Invalid constrainsts: the 'in' constraint came "
							   "with a list of constrainst values that were "
							   "not all binaries: '~p'", [ BinConstraints ] ),
			throw( { non_binary_constraint_in_list, BinConstraints } )

	end,

	DecodedConstraints = [ text_utils:binary_to_string( C )
						   || C <- BinConstraints ],

	NewAcc = [ { value_constraints, [ { in, DecodedConstraints } ]  } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


% Processes the other constrainsts, where the constrainsts should all be binary:
%
decode_input_port_specs_values( [ { <<"value_constraints">>, BinConstraints }
								  | T ], Acc ) when is_list( BinConstraints ) ->

	% Checking:
	case lists:all( fun( C ) -> is_binary( C ) end, BinConstraints ) of

		true ->
			ok;

		false ->
			?notify_error_fmt( "Invalid constraints: some of the constraints "
							   "were not binary in: '~p'", [ BinConstraints ] ),
			throw( { non_binary_constraint_in_list, BinConstraints } )
	end,

	DecodedConstraints = [ text_utils:binary_to_atom( C )
						   || C <- BinConstraints ],

	NewAcc = [ { value_constraints, DecodedConstraints } | Acc ],

	decode_input_port_specs_values( T, NewAcc );


decode_input_port_specs_values( [ OtherSpec | _T ], _Acc ) ->

	?notify_error_fmt( "Invalid input port specification received from the "
					   "static declarations of the Python processing unit: ~p",
					   [ OtherSpec ] ),

	throw( { invalid_input_port_spec, OtherSpec } ).



% Turns all the binaries in the encoded output port specs (keys and values as
% well) into the types expected by the 'parse_raw_output_port_spec' function:
%
-spec decode_output_port_specs( [ { binary(), term() } ] ) ->
									 [ { atom(), term() } ].
decode_output_port_specs( EncodedOutputPortSpecs ) ->

	% Grabs the values and converts them one by one, according to their meaning:
	decode_output_port_specs_values( EncodedOutputPortSpecs, _Acc=[] ).


% (helper)
decode_output_port_specs_values( _Proplist=[], Acc ) ->
	lists:reverse( Acc );

decode_output_port_specs_values( [ { <<"name">>, BinName } | T ], Acc )
  when is_binary( BinName ) ->

	NewAcc = [ { output_port_name, text_utils:binary_to_string( BinName ) }
			   | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ { <<"comment">>, BinComment } | T ], Acc )
  when is_binary( BinComment ) ->

	NewAcc = [ { comment, text_utils:binary_to_string( BinComment ) } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ _IterSpec={ <<"is_iteration">>, IterSpecVal }
								   | T ], Acc ) ->

	% An iteration specification from Python can only be a boolean, or an int,
	% or a tuple, or imbricated tuples containing integers.
	%
	% All of these possible types of specification are transmitted as are in
	% Erlang, and do not need any conversion:
	%
	decode_output_port_specs_values( T, [ { is_iteration, IterSpecVal }
										  | Acc ] );


decode_output_port_specs_values( [ _ResultSpec={ <<"produces_result">>,
												 SpecVal } | T ], Acc ) ->

	% A 'produces_result' specification from Python can only be a boolean value.
	%
	% Since booleans are converted automatically by ErlPort, there is no need
	% for any extra conversion:
	%
	decode_output_port_specs_values( T, [ { produces_result, SpecVal }
										  | Acc ] );


decode_output_port_specs_values( [ { <<"value_semantics">>, BinSemList } | T ],
								 Acc ) when is_list( BinSemList ) ->

	StringSems = [ text_utils:binary_to_string( BinSem ) ||
					 BinSem <- BinSemList ],

	NewAcc = [ { value_semantics, StringSems } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ { <<"value_unit">>, BinUnit } | T ], Acc )
  when is_binary( BinUnit ) ->

	NewAcc = [ { value_unit, text_utils:binary_to_string( BinUnit ) } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ { <<"value_type_description">>, BinTypeDesc }
								   | T ], Acc ) when is_binary( BinTypeDesc ) ->

	NewAcc = [ { value_type_description,
				 text_utils:binary_to_string( BinTypeDesc ) } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ { <<"value_constraints">>, BinConstraints }
								   | T ], Acc )
  when is_list( BinConstraints ) ->

	true = lists:all( fun( C ) -> is_binary( C ) end, BinConstraints ),

	DecodedConstraints = [ text_utils:binary_to_atom( C )
						   || C <- BinConstraints ],

	NewAcc = [ { value_constraints, DecodedConstraints } | Acc ],

	decode_output_port_specs_values( T, NewAcc );


decode_output_port_specs_values( [ OtherSpec | _T ], _Acc ) ->

	?notify_error_fmt( "Invalid output port specification received from the "
					   "static declarations of the Python processing unit: ~p",
					   [ OtherSpec ] ),

	throw( { invalid_output_port_spec, OtherSpec } ).




% Textual helpers section.


% Returns a textual description of this processing unit.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } = class_DataflowBlock:io_to_string(
										  State ),

	UnitClassname = wooper_utils:pep8_class_to_wooper_class(
						?getAttr(python_class) ),

	text_utils:format( "Python processing unit named '~s' of type ~p, "
			"implemented in ~p and ruled by the activation policy '~p'.~n "
			"Inputs: ~s~nOutputs:~s~n",
			[ ?getAttr(name), UnitClassname, ?getAttr(python_module),
			  ?getAttr(activation_policy), InputDetailed, OutputDetailed ] ).
