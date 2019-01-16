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



% Abstract class defining a binding manager for a given programming language,
% i.e. facilities in order to drive a set of distributed runtime containers for
% that language (ex: virtual machines, interpreters, etc.), each running on a
% distinct computing node.
%
% This class defines the mother class of all (singleton) per-language manager
% (each driving its set of runtime containers).
%
-module(class_LanguageBindingManager).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_EngineBaseObject ] ).


% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, BindingManagerName ).


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
-define( wooper_method_export, getAssociatedRuntimeContainer/2 ).


-type manager_pid() :: agent_pid().


-type node_table() :: table:table( net_utils:atom_node_name(),
								   language_utils:binding_container_pid() ).


% For child classes:
-export_type([ node_table/0, manager_pid/0 ]).


% For agent_pid():
-include("common_defines.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define(trace_emitter_categorization,"Core.Deployment.LanguageBindingManager").


% Allows to use macros for trace sending:
-include("traces.hrl").



% Implementation notes:
%
% Each actual manager for a language Foo is expected to register itself
% globally, by defining:
%
% -define( foobar_binding_manager_name, sim_diasca_foobar_binding_manager ).
%
% The process is to create all (Erlang) actors regardless of their relying or
% not on a language binding, knowing that some of them may internally rely on
% foreign code (ex: Python-based) that is to be evaluated by a binding container
% (ex: a Python interpreter) that will be chosen so that it runs on the same
% computing node as the created actor making use of it.
%
% The binding-specific code shall remain as much as possible separated from the
% rest of the code.



% The class-specific attributes are:
%
% - node_table :: basic_utils:maybe( note_table() ), a table associating to each
% computing node the PID of the binding container running on that node



% Constructs a new, abstract, named, language binding manager.
%
-spec construct( wooper:state(), class_TraceEmitter:emitter_init() ) ->
					   wooper:state().
construct( State, BindingManagerName ) ->

	% First the direct mother class:
	BaseState = class_EngineBaseObject:construct( State,
			?trace_categorize( BindingManagerName ) ),

	setAttribute( BaseState, node_table, undefined ).



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	case ?getAttr(node_table) of

		undefined ->
			State;

		NodeTable ->
			case table:isEmpty( NodeTable ) of

				true ->
					?trace( "Language manager destructed "
							"(with no live runtime container)." ),
					State;

				false ->
					?warning_fmt( "Language manager destructed, while still "
								  "having runtime containers registered: ~s",
								  [ to_string( State ) ] ),

					% Container destruction is language-specific:
					setAttribute( State, node_table, undefined )

			end

	end.



% Methods section.


% Returns the runtime container of the binding associated to specified sender.
%
% In practice the returned runtime container is the one running on the same node
% as the sender, to lighten the load induced by their exchanges.
%
% (const request)
%
-spec getAssociatedRuntimeContainer( wooper:state(), sending_actor_pid() ) ->
				request_return( language_utils:runtime_container_pid() ).
getAssociatedRuntimeContainer( State, SenderPid ) ->

	SenderNode = node( SenderPid ),

	NodeTable = ?getAttr(node_table),

	case table:lookupEntry( _K=SenderNode, NodeTable ) of

		key_not_found ->
			throw( { no_runtime_container_on_node, SenderNode } );

		{ value, ContainerPid } ->
			?wooper_return_state_result( State, ContainerPid )

	end.



% Helpers section.



% Returns a textual description of this language manager.
%
% (helper)
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	NodePairs = lists:enumerate( ?getAttr(node_table) ),

	NodeStrings = [ text_utils:format( "container ~w running on node '~s'",
		   [ ContainerPid, Node ] ) || { Node, ContainerPid } <- NodePairs ],

	text_utils:format( "binding manager federating ~B runtime containers:~s",
	   [ length( NodeStrings ), text_utils:strings_to_string( NodeStrings ) ] ).
