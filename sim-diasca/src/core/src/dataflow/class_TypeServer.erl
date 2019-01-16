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



% Class in charge of managing typing information, notably to establish the
% definition of a given type and to tell whether a given term is of a given
% type.
%
% Generally instantiated as a singleton.
%
-module(class_TypeServer).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_EngineBaseObject ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
%-define( wooper_construct_parameters,).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/0, new_link/0,
		 synchronous_new/0, synchronous_new_link/0,
		 synchronous_timed_new/0, synchronous_timed_new_link/0,
		 remote_new/1, remote_new_link/1, remote_synchronous_new/1,
		 remote_synchronous_new_link/1, remote_synchronisable_new_link/1,
		 remote_synchronous_timed_new/1, remote_synchronous_timed_new_link/1,
		 construct/1, destruct/1 ).



% Member method declarations:
%
-define( wooper_method_export, declareType/3, declareTypes/2,
		 validateType/3, validateTypes/2, getType/2, getStatus/1 ).


% Static method declarations:
%
-define( wooper_static_method_export, start/0, stop/0, stop/1, get_server/0,
		 get_names_of_builtin_types/0, resolve_type/3 ).


% Design notes:
%
% Currently the types are only checked for equality.


% Shorthands:

-type type_name() :: meta_utils:type_name().

% We rely here only on fully-expanded, pure, explicit type definitions:
-type type_definition() :: meta_utils:explicit_type().

-type type_entry() :: { type_name(), type_definition() }.

-type type_entries() :: [ type_entry() ].


% PID of the type server:
-type type_server_pid() :: pid().


% Inner table, may be reused by the type clients:
-type type_table() :: table:table( type_name(), type_definition() ).

-export_type([ type_name/0, type_definition/0, type_entry/0, type_entries/0,
			   type_server_pid/0, type_table/0 ]).




% Possible outcomes of a type validation (possibly involving multiple types):
-type validation_outcome() :: 'type_accepted'
							| { 'type_rejected', basic_utils:error_reason() }.


% Helpers:
-export([ to_string/1, type_table_to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.Dataflow.Types" ).


% For registration:
-define( type_server_name, sim_diasca_type_server ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").




% Implementation notes:
%



% The specific attributes of a type server are:
%
% - type_table :: type_table() is a table associating the name of a type to its
% actual (canonical) definition; we do not try to expand types here
% (i.e. sub-types are not replaced by their actual definition)




% Constructs a new type server.
%
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother class:
	TraceState = class_EngineBaseObject:construct( State,
									  ?trace_categorize( "TypeServer" ) ),

	naming_utils:register_as( self(), ?type_server_name, global_only ),

	?send_info( TraceState, "Type server started." ),

	% Then the class-specific actions:
	setAttribute( TraceState, type_table, table:new() ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	TypeTable = ?getAttr(type_table),

	TypeNames = lists:sort(
				  [ Name || { Name, _Def } <- table:enumerate( TypeTable ) ] ),

	?info_fmt( "Stopping type server; it knew following ~B types:~s",
			   [ length( TypeNames ),
				 text_utils:atoms_to_string( TypeNames ) ] ),

	State.




% Methods section.



% Type declarations are oneways here (that may throw), while type validations
% are requests (that may return errors).


% Declares a (possibly new) type to this server.
%
% (oneway)
%
-spec declareType( wooper:state(), type_name(), type_definition() ) ->
						 wooper:oneway_result().
declareType( State, TypeName, TypeDefinition ) ->

	TypeTable = ?getAttr(type_table),

	case record_type( TypeName, TypeDefinition, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			?wooper_return_state_only( NewState );

		{ error, Error } ->
			throw( { invalid_type_declared, Error, TypeName, TypeDefinition } )

	end.



% Declares a set of (possibly new) types to this server.
%
% (oneway)
%
-spec declareTypes( wooper:state(), [ type_entry()] ) ->
						  wooper:oneway_result().
declareTypes( State, TypeEntries ) ->

	TypeTable = ?getAttr(type_table),

	case record_types( TypeEntries, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			?wooper_return_state_only( NewState );

		{ error, Error } ->
			throw( { invalid_type_declared, Error, TypeEntries } )

	end.



% Requests this type server to validate specified type entry (the corresponding
% type may or may not be new).
%
% (request)
%
-spec validateType( wooper:state(), type_name(), type_definition() ) ->
		wooper:request_result( validation_outcome() ).
validateType( State, TypeName, TypeDefinition ) ->

	?debug_fmt( "Validation of type '~p' defined as '~p' requested.",
				[ TypeName, TypeDefinition ] ),

	TypeTable = ?getAttr(type_table),

	case record_type( TypeName, TypeDefinition, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			?wooper_return_state_result( NewState, type_accepted );

		{ error, Error } ->
			?wooper_return_state_result( State, { type_rejected, Error } )

	end.



% Requests this type server to validate specified type entries (the
% corresponding types may or may not be new).
%
% (request)
%
-spec validateTypes( wooper:state(), [ type_entry() ] ) ->
		wooper:request_result( validation_outcome() ).
validateTypes( State, TypeEntries ) ->

	?debug_fmt( "Validation of types '~p' requested.", [ TypeEntries ] ),

	TypeTable = ?getAttr(type_table),

	case record_types( TypeEntries, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			NewState = setAttribute( State, type_table, NewTypeTable ),
			?wooper_return_state_result( NewState, type_accepted );

		{ error, Error } ->
			?wooper_return_state_result( State, { type_rejected, Error } )

	end.



% Checks and records the specified type.
%
% (helper)
%
-spec record_type( type_name(), type_definition(), type_table(),
		wooper:state() ) -> { 'ok', type_table() } | basic_utils:error_term().
record_type( TypeName, TypeDefinition, TypeTable, State )
  when is_atom( TypeName ) ->

	% Received type definition may or may not be already known, and, if yes, may
	% or may not correspond to the previous registered one:

	% We need a canonical form in all cases:
	case check_type( TypeDefinition, State ) of

		{ accepted, CanonicalTypeDefinition } ->

		   case table:lookupEntry( TypeName, TypeTable ) of

			   { value, CanonicalTypeDefinition } ->
				   % Matching an already known type, nothing changed:
				   { ok, TypeTable };

			   { value, OtherTypeDefinition } ->
				   % A more precise checking comparison shall be conducted:
				   ?error_fmt( "Received a non-matching definition for "
							   "type '~s': '~p', translated as '~p', "
							   " instead of the known one '~p'.",
							   [ TypeName, TypeDefinition,
								 CanonicalTypeDefinition,
								 OtherTypeDefinition ] ),
				   { error, { unmatching_type_definition_for, TypeName,
							  TypeDefinition, OtherTypeDefinition } };

			   key_not_found ->
				   ?trace_fmt( "Type '~s', defined as '~s', recorded as '~s'.",
							   [ TypeName, TypeDefinition,
								 CanonicalTypeDefinition ] ),
				   { ok, table:addEntry( TypeName, TypeDefinition, TypeTable ) }

		   end;

		{ rejected, Reason } ->
			?error_fmt( "Type definition '~p' rejected, reason: ~p",
						[ TypeDefinition, Reason ] ),
			{ error, { type_rejected, TypeName, TypeDefinition, Reason } }

   end;

record_type( TypeName, TypeDefinition, _TypeTable, State ) ->
	?error_fmt( "Type name '~p' rejected (not an atom).", [ TypeName ] ),
	throw( { error, { invalid_type_name, TypeName, TypeDefinition } } ).



% Checks and records the specified types.
%
% (helper)
%
-spec record_types( [ type_entries() ], type_table(), wooper:state() ) ->
						  { 'ok', type_table() } | basic_utils:error_term().
record_types( _TypeEntries=[], TypeTable, _State ) ->
	{ ok, TypeTable };

record_types( _TypeEntries=[ { TypeName, TypeDef } | T ], TypeTable, State ) ->

	case record_type( TypeName, TypeDef, TypeTable, State ) of

		{ ok, NewTypeTable } ->
			record_types( T, NewTypeTable, State );

		Error -> % Error={ error, Reason }
			% Stop recursing:
			Error

	end;

record_types( _TypeEntries=[ InvalidTypeEntry | _T ], _TypeTable, State ) ->
	?error_fmt( "Invalid type entry received: '~p', whereas a "
				"{TypeName,TypeDefinition} pair was expected.",
				[ InvalidTypeEntry ] ),
	throw( { invalid_type_entry, InvalidTypeEntry } ).



% Returns the definition of specified type, expected to be already known.
%
% (const request)
%
-spec getType( wooper:state(), type_name() ) ->
				 wooper:request_return( type_definition() | 'unknown_type' ).
getType( State, TypeName ) ->

	TypeTable = ?getAttr(type_table),

	case table:lookupEntry( TypeName, TypeTable ) of

		{ value, TypeDefinition } ->
			?wooper_return_state_result( State, TypeDefinition );

		key_not_found ->
			?wooper_return_state_result( State, unknown_type )

	end.



% Returns a low-level (broken into elementary constructs), context-free
% definition of specified type.
%
% (const request)
%
%-spec resolveType( wooper:state(), type_definition() ) -> type_definition().
%resolveType( State, TypeDefinition ) ->
%
%	ResolvedType = TypeDefinition,
%
%	?wooper_return_state_result( State, ResolvedType ).



% Checks specified type definition.
%
% (helper)
%
-spec check_type( basic_utils:unchecked_data(), wooper:state() ) ->
						{ validation_outcome(), wooper:state() }.
check_type( TypeDefinition, _State ) ->

	% Expected: type_definition().

	% Not implemented yet:
	{ accepted, TypeDefinition }.



% Returns a textual description of the state of this type server.
%
% (const request)
%
-spec getStatus( wooper:state() ) -> request_return( string() ).
getStatus( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).



% Helper section.



% Static section.


% Launches the type server, with default settings.
%
% (static)
%
-spec start() -> type_server_pid().
start() ->
	new_link().



% Stops the type server.
%
% (static)
%
-spec stop() -> basic_utils:void().
stop() ->
	TypeServerPid = get_server(),
	stop( TypeServerPid ).


% Stops specified type server.
%
% (static)
%
-spec stop( type_server_pid() ) -> basic_utils:void().
stop( TypeServerPid ) ->
	TypeServerPid ! delete.



% Returns the PID of the type server (if any).
%
% (static)
%
-spec get_server() -> type_server_pid().
get_server() ->
	naming_utils:get_registered_pid_for( ?type_server_name, global ).



% Returns the names of the built-in types, i.e. the names of the types exposed
% to the user and that cannot be further decomposed.
%
% (static)
%
-spec get_names_of_builtin_types() -> [ type_name() ].
get_names_of_builtin_types() ->
	%meta_utils:get_elementary_types().
	[ boolean, integer, float, string, any ].



% Resolves specified type.

-spec resolve_type( type_name(), type_table(), type_server_pid() ) ->
						  meta_utils:explicit_type().
resolve_type( _TypeName, _TypeTable, _TypeServerPid ) ->
	% Still to be implemented:
	any.



% Helper section:


% Returns a textual description of the state of this type server.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	TypeString = type_table_to_string( ?getAttr(type_table) ),

	text_utils:format( "Type server with ~s", [ TypeString ] ).



% Returns a textual description of the specified type table.
%
-spec type_table_to_string( type_table() ) -> string().
type_table_to_string( TypeTable ) ->

	case table:enumerate( TypeTable ) of

		[] ->
			"no type known";

		Types ->

			TypeStrings = [ text_utils:format( "type '~s' is: ~p",
					[ Tn, Td ] ) || { Tn, Td } <- lists:sort( Types ) ],

			BulletString = text_utils:strings_to_string( TypeStrings ),
			text_utils:format( "~B type(s) known, namely, "
							   "in alphabetical order:~s",
							   [ length( Types ), BulletString ] )

	end.
