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



% Dataflow processing unit class, corresponding to the implementation of the
% computational parts of a dataflow.
%
% This is a specialization of the generic dataflow block actor, meant to provide
% the mother class from which the actual processing units can directly inherit.
%
% This class should provide most of the basics needed to properly describe most
% processing units, including various built-in activation policies. It may be
% subclassed if needed to introduce variants.
%
% Please refer to the 'Sim-Diasca Dataflow HOWTO' for further information.
%
-module(class_DataflowProcessingUnit).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_DataflowBlock ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ActorSettings, ProcessingUnitName,
		 ActivationPolicy, InputPortSpecs, OutputPortSpecs, DataflowPid ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/6, new_link/6,
		 synchronous_new/6, synchronous_new_link/6,
		 synchronous_timed_new/6, synchronous_timed_new_link/6,
		 remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		 remote_synchronous_new_link/7, remote_synchronisable_new_link/7,
		 remote_synchronous_timed_new/7, remote_synchronous_timed_new_link/7,
		 construct/7 ).



% Member method declarations:
%
-define( wooper_method_export, onFirstDiasca/2,
		 setInputPortValue/4, notifyNewInput/4, triggerActivation/2,
		 %getOutputPortValue/3,
		 activate/1, triggerDestruction/3 ).


% Each processing unit class ought to define following static methods:
%
% - either get_port_specifications/0 (preferable, if possible - i.e. if only
% initial ports are used)
%
% - otherwise dedicated, seperate get_declared_semantics/0 and
% get_declared_types/0
%
% If none of these static methods is defined and exported, no blocking error
% will be raised, however no static information will be reported, and at the
% very least runtime checkings will be hindered.


% So, prefer (if only initial ports are used):

%-define( wooper_static_method_export, get_port_specifications/0 ).
%
% -spec get_port_specifications() ->
%				 { [ input_port_spec() ], [ output_port_spec() ] }.

% otherwise:

%-define( wooper_static_method_export, get_declared_semantics/0,
%		  get_declared_types/0 ).
%


% Returns the semantics statically declared by this processing unit.
%
% Defining this method allows to ensure that all the ports ever created by this
% processing unit will rely on user-level semantics among this explicitly stated
% list.
%
% Otherwise the list would be deduced from the initial port specifications, with
% no specific control.
%
% (static)
%
%-spec get_declared_semantics() -> user_vocabulary().
% get_declared_semantics() ->
%	[ "an example" ].


% Returns the types statically declared by this processing unit.
%
% (static)
%
%-spec get_declared_types() -> class_TypeServer:type_entries().
%get_declared_types() ->
%	[ { 'my_type', "'something'|'other thing'" } ].


% For the input_port record and all:
-include("class_DataflowBlock_defines.hrl").


% Design notes:
%


% A processing unit activation corresponds to the execution of its (probably
% overridden) activate/1 oneway.


% Name of an instance of processing unit:
%
-type unit_name() :: string().




% About processing unit activation policies:
%
% - activate_on_new_set: the processing unit will be activated at most once per
% diasca, at the one immediately following the diasca at which at least one of
% its input ports was triggered; it is up to the processing unit to reset the
% input ports (i.e. to set them back to the unset state) when deemed appropriate
%
% - activate_when_all_set: the processing unit is activated if and only if all
% of its input ports are set; after an activation, all input ports are
% automatically reset to the 'unset' state
%
% - custom_activation: the activation of the processing unit is managed by the
% unit itself (very uncommon case)
%
-type activation_policy() :: 'activate_on_new_set'
						   | 'activate_when_all_set'
						   | 'custom_activation'.


-export_type([ unit_name/0, activation_policy/0 ]).


% Processing units may define following static method:
%
% -spec get_declared_types() -> [ class_TypeServer:type_entry() ].
%
% in order that their manager(s) are able to obtain from them the specific types
% that they may define and use.
%
%-define( wooper_static_method_export, get_declared_types/0 ).


% Helpers:
-export([ check_policy/1, to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.ProcessingUnit" ).

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").

% Allows to use macros for trace sending while no state is still available:
-include("traces_app_header.hrl").


% For types and shorthands:
-include("dataflow_defines.hrl").



% Implementation notes:
%



% Processing unit-specific attributes are:
%
% - activation_policy :: activation_policy() is the policy applied to decide
% when this processing unit should be activated
%
% - activation_requested :: boolean() tells whether an activation has already
% been requested this diasca (allows to activate an unit ruled by the
% activate_on_new_set policy only once, even if multiple of its input ports have
% been set at the same diasca)



% Constructs a new dataflow processing unit:
%
% - ActorSettings describes the actor abstract identifier (AAI) and seed of this
% actor, as automatically assigned by the load balancer
%
% - ProcessingUnitName is a human-readable name for that processing unit (as a
% plain, non-empty string)
%
% - ActivationPolicy is the policy driving the activations of this processing
% unit
%
% - InputPortSpecs is a list of the specifications of the input ports defined
% for this processing unit
%
% - OutputPortSpecs is a list of the specifications of the output ports defined
% for this processing unit
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
				 unit_name(), activation_policy(), [ input_port_spec() ],
				 [ output_port_spec() ], dataflow_pid() ) -> wooper:state().
construct( State, ActorSettings, ProcessingUnitName, ActivationPolicy,
		   InputPortSpecs, OutputPortSpecs, DataflowPid ) ->

	% First the direct mother class:
	BlockState = class_DataflowBlock:construct( State, ActorSettings,
			?trace_categorize( ProcessingUnitName ),
			InputPortSpecs, OutputPortSpecs, DataflowPid ),

	ActualPolicy = check_policy( ActivationPolicy ),

	% Then the class-specific actions:
	setAttributes( BlockState, [ { activation_policy, ActualPolicy },
								 { activation_requested, false } ] ).



% Methods section.



% Callback executed on the first diasca of existence of this processing unit.
%
% Note: should this method be overridden in a child class, this version should
% be called from there as well (as must be called in all cases).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _CallerPid ) ->

	?debug_fmt( "Created a ~s", [ to_string( State ) ] ),

	DataflowPid = ?getAttr(dataflow_pid),

	% As this class may have been specialised:
	ActualClassname = wooper:get_class_name( State ),

	SentState = class_Actor:send_actor_message( DataflowPid,
		{ registerDataflowUnit, [ ActualClassname ] }, State ),

	?wooper_return_state_only( SentState ).



% Sets explicitly the specified input port to the specified fully-specified
% channel value.
%
% Note: calling this method bypasses the (channel-based) dataflow system; it is
% mostly useful in order to feed source units from outside of the dataflow
% (typically from the experiment entry point). Such an explicit setting will
% perform activations exactly like a standard setting.
%
% (actor oneway)
%
-spec setInputPortValue( wooper:state(),
			input_port_name() | input_port_string_name(), channel_value(),
			class_Actor:actor_pid() ) -> oneway_return().
% Sending binaries shall be preferred (more efficient):
setInputPortValue( State, InputPortName, ChannelValue, SenderPid )
  when is_binary( InputPortName )
	   andalso is_record( ChannelValue, channel_value ) ->

	?debug_fmt( "Explicit setting of input port '~s' to the value '~s' "
				"for this processing unit, as requested by process ~w.",
				[ InputPortName,
				  class_DataflowBlock:value_to_string( ChannelValue ),
				  SenderPid ] ),

	% Values conveyed by channels obey by design some rules - here we have to
	% validate them from scratch as they are introduced with no control:
	%
	% (only lightweight checking done currently)

	InputPortTable = ?getAttr(input_ports),

	InputPort = class_DataflowBlock:get_input_port( InputPortName,
													InputPortTable, State ),

	class_DataflowBlock:validate_value_for_input_port( ChannelValue, InputPort,
													   InputPortName, State ),

	NewInputPort = class_DataflowBlock:assign_input_value( ChannelValue,
											 InputPort, InputPortName, State ),

	NewInputPortTable = table:updateEntry( InputPortName, NewInputPort,
										   InputPortTable ),

	SetState = setAttribute( State, input_ports, NewInputPortTable ),

	ScheduledState = consider_activation_after_input( SetState ),

	?wooper_return_state_only( ScheduledState );


setInputPortValue( State, InputPortName, ChannelValue, SenderPid )
  when is_list( InputPortName ) ->
	setInputPortValue( State, text_utils:string_to_binary( InputPortName ),
					   ChannelValue, SenderPid );


setInputPortValue( _State, InputPortName, _ChannelValue, _SenderPid ) ->
	% Probably an atom here:
	throw( { invalid_type_for_port_name, InputPortName } ).




% Notifies this dataflow unit that, for the specified input port, one of its
% upstream blocks just emitted a new (channel) value, possibly in an activation
% being triggered.
%
% Note: an immediate value (with no specific metadata) could have sufficed, as
% they have been checked at channel creation.
%
% (actor oneway)
%
-spec notifyNewInput( wooper:state(), input_port_name(), channel_value(),
					  block_pid() ) -> oneway_return().
notifyNewInput( State, InputPortName, ChannelValue, UpstreamBlockPid )
  when is_binary( InputPortName )
	   andalso is_record( ChannelValue, channel_value ) ->

	?debug_fmt( "Dataflow-based setting of input port '~s' to the value '~s' "
				"for this unit, as requested by upstream block ~w.",
				[ InputPortName,
				  class_DataflowBlock:value_to_string( ChannelValue ),
				  UpstreamBlockPid ] ),

	% Values conveyed by channels obey by design some rules - here we validate
	% them for an increased safety:
	%
	% (only lightweight checking done currently)

	InputPortTable = ?getAttr(input_ports),

	InputPort = class_DataflowBlock:get_input_port( InputPortName,
													InputPortTable, State ),

	class_DataflowBlock:validate_value_for_input_port( ChannelValue, InputPort,
													   InputPortName, State ),

	NewInputPort = class_DataflowBlock:assign_input_value( ChannelValue,
										   InputPort, InputPortName, State ),

	NewInputPortTable = table:updateEntry( InputPortName, NewInputPort,
										   InputPortTable ),

	SetState = setAttribute( State, input_ports, NewInputPortTable ),

	ScheduledState = consider_activation_after_input( SetState ),

	?wooper_return_state_only( ScheduledState );


notifyNewInput( _State, InputPortName, _ChannelValue, _UpstreamBlockPid )
  when is_list( InputPortName ) ->
	throw( { non_binary_port_name, InputPortName } );


notifyNewInput( _State, InputPortName, _ChannelValue, _UpstreamBlockPid ) ->
	% Probably an atom here:
	throw( { invalid_type_for_port_name, InputPortName } ).



% Considers whether the activation criteria are met for this processing unit,
% knowing that an input port has just been set.
%
% (helper)
%
-spec consider_activation_after_input( wooper:state() ) -> wooper:state().
consider_activation_after_input( State ) ->

	ActivationPolicy = ?getAttr(activation_policy),

	IsActivated = case ActivationPolicy of

		activate_on_new_set ->
			true;

		activate_when_all_set ->
			are_all_input_ports_set( State );

		custom_activation ->
			%is_custom_activation_triggered( State )
			throw( not_implemented_yet )

	end,

	case IsActivated of

		true ->
			case ?getAttr(activation_requested) of

				true ->
					?trace_fmt( "An input port was set that would have led to "
								"an activation of this processing unit (ruled "
								"by the ~w policy), should it be not already "
								"planned to be activated.",
								[ ActivationPolicy ] ),
					State;

				false ->
					?trace_fmt( "An input port was set, and this led to an "
								"activation of this processing unit "
								"(ruled by the ~w policy).",
								[ ActivationPolicy ] ),

					ActivatedState = class_Actor:send_actor_message( self(),
													triggerActivation, State ),

					setAttribute( ActivatedState, activation_requested, true )

			end;

		false ->
			?debug_fmt( "An input port was set, yet this did not lead to an "
						"activation of this processing unit "
						"(ruled by the ~w policy):~s", 
						[ ActivationPolicy,
						  list_unset_input_ports( State ) ] ),
			State

	end.




% Tells whether all input ports are set.
%
% Note: should there be no input port, this property (actually telling whether
% none is unset) is thus considered true.
%
% (helper)
%
-spec are_all_input_ports_set( wooper:state() ) -> boolean().
are_all_input_ports_set( State ) ->
	InputPorts = table:values( ?getAttr(input_ports) ),
	are_all_set( InputPorts ).

% Lists the ports which are not yet set, for information.
%
% (helper)
%
-spec list_unset_input_ports( wooper:state() ) -> string().
list_unset_input_ports( State ) -> 

	% in all the (standard) input ports,
	% filter the ports which are not set
	% and keep their names
	InputTable = ?getAttr(input_ports),
	UnsetPortNames = [
		text_utils:ensure_string(PortName)
		|| 
		{ 
			PortName, 
			#input_port{ 
				value_status=ValueStatus 
				}
		} <- table:enumerate( InputTable ),
		ValueStatus == unset 
	],

	text_utils:format( "The following ~B ports over ~B are not set:~n~s",
					   [ length(UnsetPortNames), table:size(InputTable),
						 text_utils:format( string:join( lists:sort( UnsetPortNames ), ",~n" ), [] )
					   ]
					   ).

% Tells whether all specified input ports are set.
%
% (helper)
%
-spec are_all_set( [ input_port() ] ) -> boolean().
are_all_set( _InputPorts=[] ) ->
	true;

are_all_set( _InputPorts=[ #input_port{ value_status=unset } | _T ] ) ->
	false;

% A bit of extraneous checking:
are_all_set( _InputPorts=[ #input_port{ value_status={set,_V} } | T ] ) ->
	are_all_set( T ).



% Unsets all the input ports of this processing unit.
%
-spec unset_all_input_ports( wooper:state() ) -> wooper:state().
unset_all_input_ports( State ) ->

	InputPorts = table:enumerate( ?getAttr(input_ports) ),

	ResetInputPorts = reset_ports( InputPorts, _Acc=[] ),

	NewInputPortTable = table:new( ResetInputPorts ),

	setAttribute( State, input_ports, NewInputPortTable ).



% Returns a reset version of the specified list of pairs.
%
reset_ports( _InputPorts=[], Acc ) ->
	Acc;

reset_ports( _InputPorts=[ { PortName, Port } | T ], Acc ) ->

	NewPort = Port#input_port{ value_status=unset },

	reset_ports( T, [ { PortName, NewPort } | Acc ] ).



% Delayed activation, so that by design all input ports that may have been set
% during the previous diasca have already been recorded: then this unit is
% activated only once, regardless of the number of the previous input port
% assignments.
%
% (self-triggered actor oneway)
%
-spec triggerActivation( wooper:state(), sending_actor_pid() ) ->
							   actor_oneway_return().
triggerActivation( State, _SelfSendingActorPid ) ->

	ActivatedState = executeOneway( State, activate ),

	PortResetState = case ?getAttr(activation_policy) of

		activate_when_all_set ->
			unset_all_input_ports( ActivatedState );

		_ ->
			ActivatedState

	end,

	ActResetState = setAttribute( PortResetState, activation_requested, false ),

	?wooper_return_state_only( ActResetState ).



% Callback executed automatically whenever the processing unit is activated.
%
% Meant to be overridden.
%
% (oneway)
%
-spec activate( wooper:state() ) -> oneway_return().
activate( State ) ->

	?warning_fmt( "Default, do-nothing activation triggered for ~s.",
				  to_string( State ) ),

	?wooper_return_state_only( State ).



% Triggers the destruction of this processing unit.
%
% Typically called from its unit manager when having to destruct a unit after
% being notified that an associated dataflow object has been destructed.
%
% (actor oneway)
%
-spec triggerDestruction( wooper:state(), class_DataflowUnitManager:action_id(),
						  sending_actor_pid() ) -> actor_oneway_return().
triggerDestruction( State, ActionId, SenderPid ) ->

	?debug_fmt( "Destruction triggered by ~w, in the context of action #~B",
				[ SenderPid, ActionId ] ),

	% Regardless of upstream or downstream:
	ConnectedBlocks = set_utils:to_list(
			class_DataflowBlock:get_directly_connected_blocks( State ) ),

	ConnectState = class_Actor:send_actor_messages( ConnectedBlocks,
									_Oneway=disconnectFromBlock, State ),

	ActualClassname = wooper:get_class_name( ConnectState ),

	UnregisterState = class_Actor:send_actor_message( ?getAttr(dataflow_pid),
		{ unregisterDataflowUnit, [ ActualClassname ] }, ConnectState ),

	DestructState = class_Actor:send_actor_message( SenderPid,
		{ onUnitDestructed, [ ActionId, ActualClassname ] }, UnregisterState ),

	DeclaredState = executeOneway( DestructState, declareTermination ),

	EmptyPortTable = table:new(),

	FinalState = setAttributes( DeclaredState, [
					{ input_ports, EmptyPortTable },
					{ output_ports, EmptyPortTable },
					{ run_status, terminating } ] ),

	?wooper_return_state_only( FinalState ).





% Helper functions.


% Checks the processing unit activation policy provided by the user.
%
-spec check_policy( basic_utils:user_data() ) ->
						  activation_policy().
check_policy( P=activate_on_new_set ) ->
	P;

check_policy( P=activate_when_all_set ) ->
	P;

check_policy( P=custom_activation ) ->
	P;

check_policy( P ) when is_atom( P ) ->
	?app_error_fmt( "Unknown activation policy encountered: ~p (unsupported).",
				[ P ] ),
	throw( { unsupported_activation_policy, P } );

check_policy( P ) ->
	?app_error_fmt( "Invalid type of activation policy definition: ~p.",
					[ P ] ),
	throw( { invalid_activation_policy_type, P } ).



% Returns a textual description of this processing unit.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	{ InputDetailed, OutputDetailed } = class_DataflowBlock:io_to_string(
										  State ),

	text_utils:format( "processing unit named '~s', being ~s, "
					   "applying the ~s activation policy, having ~s and ~s",
					   [ ?getAttr(name), ?getAttr(run_status),
						 ?getAttr(activation_policy),
						 InputDetailed, OutputDetailed ] ).
