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


% Class in charge of managing a set of Python interpreters, to be used as
% binding containers for actors that happen to rely on Python-based code.
%
% Several interpreters may be used in order to dispatch processing load and
% memory consumption, in a distributed way.
%
% This class is meant to be a simulation-level singleton, a service running on
% the user node that creates one Python interpreter per computing node and
% federates them all.
%
-module(class_PythonBindingManager).


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
-define( wooper_method_export, getAssociatedPythonInterpreter/1 ).


-type manager_pid() :: class_LanguageBindingManager:manager_pid().


-export_type([ manager_pid/0 ]).


% Helpers:
-export([ get_registration_name/0, get_registered_manager/0, get_interpreter/0,
		  get_interpreter/1, to_string/1 ]).




% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Deployment.PythonBindingManager" ).


% For registration:
-define( python_binding_manager_name, sim_diasca_python_binding_manager ).


% Allows to use macros for trace sending:
-include("traces.hrl").



% Implementation notes:
%
% For the communication between Erlang and Python, ErlPort (http://erlport.org/;
% source in https://github.com/hdima/erlport) is relied upon. Of course each
% interpreter will have its own state.



% Constructs a new manager of a set of Python interpreters.
%
% - ComputingNodes is a list of the computing nodes on each of which a Python
% interpreter is to be created
%
-spec construct( wooper:state(), [ net_utils:atom_node_name() ] ) ->
					   wooper:state().
construct( State, ComputingNodes ) ->

	% First the direct mother class:
	LangState = class_LanguageBindingManager:construct( State,
						   ?trace_categorize( "PythonBindingManager" ) ),


	% Any language-specific binding manager might be registered that way:
	% (enforces uniqueness, and provides global access)
	%
	naming_utils:register_as( ?python_binding_manager_name, global_only ),

	?send_info_fmt( LangState, "Creating the binding manager of ~B "
							   "Python interpreters, running on the following "
							   "computing nodes:~s",
					[ length( ComputingNodes ),
					  text_utils:atoms_to_string( ComputingNodes ) ] ),

	% Rushing now the parallel, longer interpreter creations:
	NodeTable = initialise_interpreters( ComputingNodes, LangState ),

	setAttribute( LangState, node_table, NodeTable ).




% Launches and initializes Python interpreters in each of the specified nodes.
%
% (helper)
%
-spec initialise_interpreters( [ net_utils:atom_node_name() ],
		   wooper:state() ) -> class_LanguageBindingManager:node_table().
initialise_interpreters( TargetNodes, State ) ->

	% Options customizing the Python interpreters and the way ErlPort
	% communicates with them:
	%  - compression level (for optimisation tests)
	%  - which 'python' executable (because Python 2 might be the default)
	%  - path(s) to append to Python's code path (in sys.path)

	RootDir = class_DeploymentManager:determine_root_directory(),

	PythonAPIPath = file_utils:join( [ RootDir, "sim-diasca", "src", "core",
							"src", "dataflow", "bindings", "python", "api" ] ),

	WorkingDir = file_utils:get_current_directory(),

	ErlportStartOptions = [ { compressed, 0 },
							% Could be: /bin/env/python, python3, etc.:
							{ python, "python-sdec" },
							{ python_path, [ WorkingDir, PythonAPIPath ] } ],


	% Starts one interpreter per specified node, and populates the inherited
	% node table with them; launching an interpreter is long, it is thus done in
	% parallel:
	%
	% (for an unknown reason, python:start_link/1 shall not be used - at least
	% no with a rpc call, as this leads to a freeze and an interpreter that is
	% crashed or not even launched)
	%
	{ Res, FailedNodes } = rpc:multicall( TargetNodes, python, start,
										  [ ErlportStartOptions ] ),

	case FailedNodes of

		[] ->
			ok;

		_ ->
			?error_fmt( "Following ~B nodes failed during interpreter "
						"initializations:~s",
						[ length( FailedNodes ),
						  text_utils:strings_to_string( FailedNodes ) ] ),
			throw( { failed_nodes, python_initialization, FailedNodes } )

	end,

	% Res is a (supposedly ordered) list of per-node results, being the ones
	% of python:start_link/1:

	% We take advantage of this pass to link this manager to each interpreter
	% (as the start_link above must have been done towards a temporary, now
	% terminated process):
	%
	InterpreterPids = case lists:foldl(
						fun
						  ( { ok, IntPid }, _Acc={ AccPid, AccError } ) ->
							  erlang:link( IntPid ),
							  { [ IntPid | AccPid ], AccError };

						  ( { error, Error }, _Acc={ AccPid, AccError } ) ->
								{ AccPid, [ Error | AccError ] };

						  ( Unexpected, _Acc ) ->
								throw( { unexpected_launch_outcome,
										 Unexpected } )

						end,
						_Acc0={ [], [] },
						_List=Res ) of

		{ PidList, _Errors=[] } ->
			PidList;

		{ _PidList, Errors } ->

			% When an error occurs, not sure we can relate it to a given node,
			% as it is unclear whether the rpc:multicall put outcomes in the
			% order of the specified target nodes (probably yes, though)

			ErrorStrings = [ text_utils:format( "~p", [ E ] ) || E <- Errors ],

			?error_fmt( "Following ~B errors occurred during interpreter "
						"initializations:~s", [ length( Errors ),
						text_utils:strings_to_string( ErrorStrings ) ] ),

			throw( { failed_interpreter_initializations, Errors } )

	end,

	NodePairs = lists:zip( TargetNodes, InterpreterPids ),

	% Filling and checking the node table:
	FilledNodeTable = lists:foldl(
		fun( { Node, InterpreterPid }, AccTable ) ->
			% Checking:
			%Node = node( InterpreterPid ),
			table:addNewEntry( Node, InterpreterPid, AccTable )
		end,
		_FillAcc=table:new(),
		_FillList=NodePairs ),

	?debug_fmt( "~B interpreters (~w) successfully spawned.",
				[ length( InterpreterPids ), InterpreterPids ] ),

	% Retrieves and displays the version of Python used by each interpreter:
	PythonVersions = [ python:call( Pid, 'platform', python_version, [ ] )
					   || Pid <- InterpreterPids ],

	?debug_fmt( "~B Python interpreters successfully initialised, "
				"running versions ~p.",
				[ length( InterpreterPids ), PythonVersions ] ),

	% Initializes (sequentially) the binding-induced states of all interpreters,
	% using our binding_input.py module for that:
	%
	[ python:call( Pid, 'common.erlang_binding_entry', init_binding,
				   [ self() ] ) || Pid <- InterpreterPids ],


	% Check returned value; how exceptions are to be handled?

	FilledNodeTable.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Stopping all interpreters:
	case ?getAttr(node_table) of

		undefined ->
			State;

		NodeTable ->

			case table:values( NodeTable ) of

				[] ->
					?trace( "Manager destructed (no Python interpreter "
							"was registered)." ),
					State;

				InterpreterPids ->

					?trace_fmt( "Stopping ~w Python interpreters "
								"(corresponding to following runtime "
								"containers: ~w).",
								[ length( InterpreterPids ),
								  InterpreterPids ] ),

					% python:stop/1 returns 'ok' in all cases:
					[ python:stop( IntPid ) ||  IntPid <- InterpreterPids ],

					?trace( "Manager destructed." ),

					setAttribute( State, node_table, undefined )

			end

	end.




% Methods section.


% Returns the Python interpreter associated to specified sender.
%
% In practice the returned binding container is the Python interpreter running
% on the same node as the sender, to lighten the load induced by their
% exchanges.
%
% (const request)
%
-spec getAssociatedPythonInterpreter( wooper:state() ) ->
			request_return( language_utils:python_interpreter_container_pid() ).
getAssociatedPythonInterpreter( State ) ->

	SenderPid = ?getSender(),

	% This inherited method is just fine:
	{ State, InterpreterPid } = executeRequest( State,
							getAssociatedRuntimeContainer, [ SenderPid ] ),

	?wooper_return_state_result( State, InterpreterPid ).





% Static section.



% Returns the atom corresponding to the name the Python binding manager should
% be registered as.
%
% Note: executed on the caller node.
%
% (static)
%
-spec get_registration_name() -> naming_utils:registration_name().
get_registration_name() ->
	?python_binding_manager_name.



% Returns the PID of the (unique) Python binding manager.
%
% (static method, to be used by clients of the Python binding manager)
%
-spec get_registered_manager() -> manager_pid().
get_registered_manager() ->

	ManagerName = get_registration_name(),

	% No waiting performed, as expected to have been synchronously created:
	% try naming_utils:wait_for_global_registration_of( ManagerName )...

	case naming_utils:is_registered( ManagerName, _Scope=global ) of

		not_registered ->
			?notify_error( "No Python binding manager registered, whereas its "
						   "availability has been requested; maybe it has not "
						   "been listed in the 'enabled_language_bindings' "
						   "field of the deployment settings?" ),

			throw( python_binding_manager_not_registered );

		ManagerPid ->
			ManagerPid

	end.




% Returns the PID of the (local) Python interpreter.
%
% (static method, to be used by clients of this Python binding manager)
%
-spec get_interpreter() -> python_utils:interpreter_pid().
get_interpreter() ->
	get_interpreter( get_registered_manager() ).



% Returns the PID of the (local) Python interpreter, based on the specified
% Python binding manager.
%
% (static method, to be used by clients of this Python binding manager)
%
-spec get_interpreter( manager_pid() ) -> python_utils:interpreter_pid().
get_interpreter( PythonBindingManagerPid ) ->

	PythonBindingManagerPid ! { getAssociatedPythonInterpreter, [], self() },

	receive

		{ wooper_result, InterpreterPid } when is_pid( InterpreterPid ) ->
			InterpreterPid

	end.



% Helpers section.



% Returns a textual description of this manager.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	NodePairs = lists:enumerate( ?getAttr(node_table) ),

	NodeStrings = [ text_utils:format( "interpreter ~w running on node '~s'",
		   [ ContainerPid, Node ] ) || { Node, ContainerPid } <- NodePairs ],

	text_utils:format( "Python binding manager federating ~B interpreters:~s",
	   [ length( NodeStrings ), text_utils:strings_to_string( NodeStrings ) ] ).
