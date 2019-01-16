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



% Class defining the manager(s) of Java Virtual Machines (actually Jinterface
% OtpNodes hosting one or several OtpMailbox(es), each OtpMailbox being roughly
% equivalent to an Erlang PID).
%
% Since a single OtpNode is thought not to be enough for most general needs, we
% need to start several ones and to store them in a list. A manager instantiated
% from this class is an object intended to manipulate such a list.
%
-module(class_JavaBindingManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_LanguageBindingManager ] ).


% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, ComputingNodes ).


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
-define( wooper_method_export, getAssociatedJavaMailbox/1 ).


-type manager_pid() :: class_LanguageBindingManager:manager_pid().


-export_type([ manager_pid/0 ]).


% Helpers:
-export([ get_registered_manager/0, %get_virtual_machine/0,
		  %get_virtual_machine/1, to_string/1 ]).
		  to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Deployment.JavaBindingManager" ).


% For registration:
-define( java_binding_manager_name, sim_diasca_java_binding_manager ).


% Allows to use macros for trace sending:
-include("traces.hrl").



% Implementation notes:
%
% For the communication between Erlang and Java, Jinterface is relied upon
% (http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html). Of course
% each OtpMailbox will have its own state.



% Constructs a new manager of Java resources (resource manager):
%
-spec construct( wooper:state(), [ net_utils:atom_node_name() ] ) ->
					   wooper:state().
construct( State, ComputingNodes ) ->

	% First the direct mother class:
	LangState = class_LanguageBindingManager:construct( State,
						   ?trace_categorize( "JavaBindingManager" ) ),


	% Any language-specific binding manager might be registered that way:
	% (enforces uniqueness, and provides global access)
	%
	naming_utils:register_as( ?java_binding_manager_name, global_only ),

	?send_info_fmt( LangState, "Creating the binding manager of ~B Java "
							   "OtpNodes, running on the following computing "
							   "nodes:~s",
					[ length( ComputingNodes ),
					  text_utils:atoms_to_string( ComputingNodes ) ] ),

	% Rushing now the parallel, longer interpreter creations:
	NodeTable = table:new(),%initialise_java_nodes( ComputingNodes, LangState ),

	setAttribute( LangState, node_table, NodeTable ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	State. % TODO



% Methods section.



% Returns the Java Virtual Machine associated to specified sender.
%
% In practice the returned binding container is the Java VM running on the same
% node as the sender, to lighten the load induced by their exchanges.
%
% (const request)
%
-spec getAssociatedJavaMailbox( wooper:state() ) ->
			request_return( language_utils:java_vm_container_pid() ).
getAssociatedJavaMailbox( State ) ->

	SenderPid = ?getSender(),

	% This inherited method is just fine:
	{ State, InterpreterPid } = executeRequest( State,
							getAssociatedRuntimeContainer, [ SenderPid ] ),

	?wooper_return_state_result( State, InterpreterPid ).



% Static section.



% Returns the atom corresponding to the name the Java binding manager should be
% registered as.
%
% Note: executed on the caller node.
%
% (static)
%
%-spec get_registration_name() -> naming_utils:registration_name().
%get_registration_name() ->
%	% Ex: 'sim_diasca_java_binding_manager':
%	?java_binding_manager_name.



% Returns the PID of the (unique) Java binding manager.
%
% (static method, to be used by clients of the Java binding manager)
%
-spec get_registered_manager() -> 'none' | manager_pid().
get_registered_manager() ->

	case naming_utils:is_registered( ?java_binding_manager_name, global ) of

		not_registered ->
			none;

		Pid ->
			Pid

	end.



% Helpers section.



% Returns a textual description of this actor.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( _State ) ->

	text_utils:format( "Java binding manager federating ~B virtual machines:~s",
	   [ 0, " []" ] ).
