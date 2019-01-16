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



% Class in charge of maintaining a two-way relationship between an external
% identifier (ex: set by a more global platform) and an internal one (i.e. the
% PID of a dataflow block).
%
% External identifiers of type string shall better be transmitted as binaries.
%
% Generally instantiated as a singleton.
%
% Also referred to as the "ID server".
%
-module(class_IdentificationServer).


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
-define( wooper_method_export,
		 declareIdentifierAssociation/3, declareIdentifierAssociations/2,
		 removeIdentifierAssociation/2,
		 getExternalIdentifier/2, getAnyExternalIdentifier/2,
		 getExternalIdentifiers/2, getAnyExternalIdentifiers/2,
		 getBlockPID/2, getAnyBlockPID/2,
		 getBlockPIDs/2, getAnyBlockPIDs/2,
		 getStatus/1 ).



% Static method declarations:
%
-define( wooper_static_method_export, start/0, forge_external_identifier/1,
		 stop/0, get_server/0, get_any_server/0 ).



% Design notes:
%
% This server is not an actor, and other technical components are expected to
% interact with it mostly thanks to (synchronous) requests.
%
% One should ensure that no unsynchronised concurrent access to this server is
% performed in the course of the simulation.


% The identifier bijection is maintained thanks to two synchronised tables:

% Inner table to convert external identifiers into (internal) block PIDs:
-type inbound_table() :: table:table( external_id(), block_pid() ).


% Inner table to convert (internal) block PIDs into external identifiers:
-type outbound_table() :: table:table( block_pid(), external_id() ).


% Should no external identifier be available:
-type maybe_external_id() :: basic_utils:maybe( external_id() ).


% Should no block PID be available:
-type maybe_block_pid() :: basic_utils:maybe( block_pid() ).


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% Must be included before class_EngineBaseObject header:
-define( trace_emitter_categorization,
		 "Core.Dataflow.World.Identification" ).


% For registration:
-define( id_server_name, sim_diasca_identification_server ).


% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For identification_server_pid() and all:
-include("dataflow_defines.hrl").



% Implementation notes:
%
% We expect look-ups of PIDs to be faster than look-ups of external IDs
% (typically binaries), hence we prioritize the accesses to the outbound table
% over the ones to the inbound table.



% The specific attributes of an identification server are:
%
% - inbound_table :: inbound_table() is a table able to convert external
% identifiers into (internal) block PIDs
%
% - outbound_table :: outbound_table() is a table able to convert (internal)
% block PIDs into external identifiers



% Constructs a new identification server.
%
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% First the direct mother class:
	TraceState = class_EngineBaseObject:construct( State,
							?trace_categorize( "IdentificationServer" ) ),

	naming_utils:register_as( self(), ?id_server_name, global_only ),

	?send_info( TraceState, "Identification server started." ),

	EmptyTable = table:new(),

	% Then the class-specific actions:
	setAttributes( TraceState, [
		{ inbound_table, EmptyTable },
		{ outbound_table, EmptyTable } ] ).



-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Expected to match:
	InCount = table:size( ?getAttr(inbound_table) ),
	OutCount = table:size( ?getAttr(outbound_table) ),

	?info_fmt( "Stopping identification server; it knew ~B inbound "
			   "associations and ~B outbound ones.", [ InCount, OutCount ] ),

	State.




% Methods section.



% Declares the specified identifier association.
%
% Multiple declarations for the same identifier pair are allowed, provided that
% they match.
%
% (request, for synchronisation)
%
-spec declareIdentifierAssociation( wooper:state(), block_pid(),
  external_id() ) -> wooper:request_result( 'identifier_association_declared' ).
declareIdentifierAssociation( State, BlockPid, ExternalId ) ->

	InboundTable = ?getAttr(inbound_table),
	OutboundTable = ?getAttr(outbound_table),

	{ NewInboundTable, NewOutboundTable } = declare_association( BlockPid,
							 ExternalId, InboundTable, OutboundTable, State ),

	NewState = setAttributes( State, [ { inbound_table, NewInboundTable },
									   { outbound_table, NewOutboundTable } ] ),

	?wooper_return_state_result( NewState, identifier_association_declared ).



% Declares specified association.
%
% (helper)
%
% State is const, only used for traces.
%
-spec declare_association( block_pid(), external_id(), inbound_table(),
  outbound_table(), wooper:state() ) -> { inbound_table(), outbound_table() }.
declare_association( BlockPid, ExternalId, InboundTable, OutboundTable,
					 State ) ->

	?void_fmt( "Identifier association of block PID '~w' to external "
				"identifier '~p'.~n", [ BlockPid, ExternalId ] ),

	% Tables expected to be consistent, and look-up in outbound probably faster:
	%
	case table:lookupEntry( BlockPid, OutboundTable ) of

		key_not_found ->

			% Not addEntry for inbound, as we are paranoid:
			NewIn = table:addNewEntry( ExternalId, BlockPid, InboundTable ),

			NewOut = table:addEntry( BlockPid, ExternalId, OutboundTable ),

			{ NewIn, NewOut };


		{ value, ExternalId } ->
			% OK, consistent, no more checking:
			{ InboundTable, OutboundTable };


		{ value, OtherExternalId } ->
			?error_fmt( "Error, received an association declaration for block "
						"PID ~w and external identifier '~s', whereas the "
						"external identifier '~s' was already registered "
						"for this block.",
						[ BlockPid, ExternalId, OtherExternalId ] ),
			throw( { inconsistent_association_declaration, BlockPid,
					 { ExternalId, OtherExternalId } } )

	end.



% Declares the specified identifier associations.
%
% Multiple declarations for the same identifier pair are allowed, provided that
% they match.
%
% (request, for synchronisation)
%
-spec declareIdentifierAssociations( wooper:state(),
						   [ { block_pid(), external_id() } ]  ) ->
				 wooper:request_result( 'identifier_associations_declared' ).
declareIdentifierAssociations( State, IdPairs ) ->

	InboundTable = ?getAttr(inbound_table),
	OutboundTable = ?getAttr(outbound_table),

	{ NewInboundTable, NewOutboundTable } = declare_associations( IdPairs,
								   InboundTable, OutboundTable, State ),

	NewState = setAttributes( State, [ { inbound_table, NewInboundTable },
									   { outbound_table, NewOutboundTable } ] ),

	?wooper_return_state_result( NewState, identifier_associations_declared ).



% Declares specified identifier associations.
%
% (helper)
%
% State is const, only used for traces.
%
-spec declare_associations( [ { block_pid(), external_id() } ],
				inbound_table(), outbound_table(), wooper:state() ) ->
								  { inbound_table(), outbound_table() }.
declare_associations( _IdPairs=[], InboundTable, OutboundTable, _State ) ->
	{ InboundTable, OutboundTable };

declare_associations( _IdPairs=[ { BlockPid, ExternalId } | T ], InboundTable,
					  OutboundTable, State ) ->

   { NewInboundTable, NewOutboundTable } = declare_association( BlockPid,
						   ExternalId, InboundTable, OutboundTable, State ),

	declare_associations( T, NewInboundTable, NewOutboundTable, State ).



% Removes the known identifier association for specified block PID.
%
% Throws if the specified block PID is not known.
%
% (request, for synchronicity)
%
-spec removeIdentifierAssociation( wooper:state(), block_pid() ) ->
			   wooper:request_result( 'identifier_associations_removed' ).
removeIdentifierAssociation( State, BlockPid ) ->

	OutboundTable = ?getAttr(outbound_table),

	% Better control than extractEntry/2:
	case table:lookupEntry( BlockPid, OutboundTable ) of

		{ value, ExtId } ->

			%?debug_fmt( "The association between the block PID ~w and the "
			%			"external identifier '~p' has been removed.",
			%			[ BlockPid, ExtId ] ),

			NewOutboundTable = table:removeEntry( BlockPid, OutboundTable ),

			NewInboundTable = table:removeEntry( ExtId,
												 ?getAttr(inbound_table) ),

			RemovedState = setAttributes( State, [
							{ outbound_table, NewOutboundTable },
							{ inbound_table, NewInboundTable } ] ),

			?wooper_return_state_result( RemovedState,
										 identifier_associations_removed );


		key_not_found ->
			throw( { unknown_block_pid, BlockPid } )

	end.



% Returns the external identifier corresponding to the specified block PID.
%
% Throws if the specified block PID is not known.
%
% (const request)
%
-spec getExternalIdentifier( wooper:state(), block_pid() ) ->
								   wooper:request_result( external_id() ).
getExternalIdentifier( State, BlockPid ) ->

	case table:lookupEntry( BlockPid, ?getAttr(outbound_table) ) of

		{ value, ExternalId } ->
			?wooper_return_state_result( State, ExternalId );


		key_not_found ->
			throw( { unknown_block_pid, BlockPid } )

	end.



% Returns the external identifier corresponding to the specified block
% PID.
%
% Returns 'undefined' if the specified block PID is not known.
%
% (const request)
%
-spec getAnyExternalIdentifier( wooper:state(), block_pid() ) ->
		   wooper:request_result( maybe_external_id() ).
getAnyExternalIdentifier( State, BlockPid ) ->

	case table:lookupEntry( BlockPid, ?getAttr(outbound_table) ) of

		{ value, ExternalId } ->
			?wooper_return_state_result( State, ExternalId );


		key_not_found ->
			?wooper_return_state_result( State, undefined )

	end.



% Returns the external identifiers corresponding to the specified block PIDs.
%
% The returned list is in the same order as the input one, i.e. the external
% identifier of a block PID is at the same rank as it was in the list of block
% PIDs.
%
% Throws if a specified block PID is not known.
%
% (const request)
%
-spec getExternalIdentifiers( wooper:state(), [ block_pid() ] ) ->
								   wooper:request_result( [ external_id() ] ).
getExternalIdentifiers( State, BlockPids ) ->

	OutboundTable = ?getAttr(outbound_table),

	ExtIds = get_external_ids( BlockPids, OutboundTable, State, _Acc=[] ),

	?wooper_return_state_result( State, ExtIds ).



% (helper)
%
get_external_ids( _BlockPids=[], _OutboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_external_ids( _BlockPids=[ BlockPid | T ], OutboundTable, State, Acc ) ->

	ExtId = case table:lookupEntry( BlockPid, OutboundTable ) of

		{ value, ExternalId } ->
			ExternalId;

		key_not_found ->
			throw( { unknown_block_pid, BlockPid } )

	end,

	get_external_ids( T, OutboundTable, State, [ ExtId | Acc ] ).



% Returns the external identifiers corresponding to the specified block PIDs.
%
% The returned list is in the same order as the input one, i.e. the external
% identifier of a block PID is at the same rank as it was in the list of block
% PIDs.
%
% Returns 'undefined' for any specified block PID that is not known.
%
% (const request)
%
-spec getAnyExternalIdentifiers( wooper:state(), [ block_pid() ] ) ->
		   wooper:request_result( [ external_id() ] ).
getAnyExternalIdentifiers( State, BlockPids ) ->

	OutboundTable = ?getAttr(outbound_table),

	ExtIds = get_any_external_ids( BlockPids, OutboundTable, State, _Acc=[] ),

	?wooper_return_state_result( State, ExtIds ).



% (helper)
%
get_any_external_ids( _BlockPids=[], _OutboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_any_external_ids( _BlockPids=[ BlockPid | T ], OutboundTable, State,
					  Acc ) ->

	ExtId = case table:lookupEntry( BlockPid, OutboundTable ) of

		{ value, ExternalId } ->
			ExternalId;

		key_not_found ->
			undefined

	end,

	get_any_external_ids( T, OutboundTable, State, [ ExtId | Acc ] ).




% Returns the block PID corresponding to the specified external identifier.
%
% Throws if the specified external identifier is not known.
%
% (const request)
%
-spec getBlockPID( wooper:state(), external_id() ) ->
								   wooper:request_result( block_pid() ).
getBlockPID( State, ExternalIdentifier ) ->

	InboundTable = ?getAttr(inbound_table),

	case table:lookupEntry( ExternalIdentifier, InboundTable ) of

		{ value, BlockPid } ->
			?wooper_return_state_result( State, BlockPid );


		key_not_found ->
			throw_on_external_id_not_found( ExternalIdentifier, State )

	end.



% Returns the block PID corresponding to the specified external identifier.
%
% Returns 'undefined' if the specified external identifier is not known.
%
% (const request)
%
-spec getAnyBlockPID( wooper:state(), external_id() ) ->
								   wooper:request_result( maybe_block_pid() ).
getAnyBlockPID( State, ExternalIdentifier ) ->

	InboundTable = ?getAttr(inbound_table),

	case table:lookupEntry( ExternalIdentifier, InboundTable ) of

		{ value, BlockPid } ->
			?wooper_return_state_result( State, BlockPid );


		key_not_found ->
			?wooper_return_state_result( State, undefined )

	end.



% Returns the block PIDs corresponding to the specified external identifiers.
%
% The returned list is in the same order as the input one, i.e. the block PID of
% an external identifier is at the same rank as it was in the list of external
% identifiers.
%
% Throws if a specified block PID is not known.
%
% (const request)
%
-spec getBlockPIDs( wooper:state(), [ external_id() ] ) ->
								   wooper:request_result( [ block_pid() ] ).
getBlockPIDs( State, ExternalIdentifiers ) ->

	InboundTable = ?getAttr(inbound_table),

	BlockPids = get_block_pids( ExternalIdentifiers, InboundTable, State,
								_Acc=[] ),

	?wooper_return_state_result( State, BlockPids ).



% (helper)
%
get_block_pids( _ExtIDs=[], _InboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_block_pids( _ExtIDs=[ ExtID | T ], InboundTable, State, Acc ) ->

	 BlockPid = case table:lookupEntry( ExtID, InboundTable ) of

		{ value, Pid } ->
			Pid;

		key_not_found ->
			throw_on_external_id_not_found( ExtID, State )

	end,

	get_block_pids( T, InboundTable, State, [ BlockPid | Acc ] ).



% Returns the block PIDs corresponding to the specified external identifiers.
%
% The returned list is in the same order as the input one, i.e. the block PID of
% an external identifier is at the same rank as it was in the list of external
% identifiers.
%
% Returns 'undefined' for any specified external identifier that is not known.
%
% (const request)
%
-spec getAnyBlockPIDs( wooper:state(), [ external_id() ] ) ->
								   wooper:request_result( [ block_pid() ] ).
getAnyBlockPIDs( State, ExternalIdentifiers ) ->

	InboundTable = ?getAttr(inbound_table),

	BlockPids = get_any_block_pids( ExternalIdentifiers, InboundTable, State,
								_Acc=[] ),

	?wooper_return_state_result( State, BlockPids ).



% (helper)
%
get_any_block_pids( _ExtIDs=[], _InboundTable, _State, Acc ) ->
	% Preserve the right order:
	lists:reverse( Acc );


get_any_block_pids( _ExtIDs=[ ExtID | T ], InboundTable, State, Acc ) ->

	 BlockPid = case table:lookupEntry( ExtID, InboundTable ) of

		{ value, Pid } ->
			Pid;

		key_not_found ->
			undefined

	end,

	get_any_block_pids( T, InboundTable, State, [ BlockPid | Acc ] ).




% Returns the current status of this identification server.
%
% (const request)
%
-spec getStatus( wooper:state() ) ->
					   wooper:request_result( text_utils:bin_string() ).
getStatus( State ) ->

	Status = text_utils:string_to_binary( to_string( State ) ),

	?wooper_return_state_result( State, Status ).



% Static section.


% Launches the identification server, with default settings.
%
% (static)
%
-spec start() -> identification_server_pid().
start() ->
	new_link().



% Stops the identification server.
%
% (static)
%
-spec stop() -> basic_utils:void().
stop() ->
	IdentificationServerPid = get_server(),
	stop( IdentificationServerPid ).



% Stops specified identification server.
%
% (static)
%
-spec stop( identification_server_pid() ) -> basic_utils:void().
stop( IdentificationServerPid ) ->
	IdentificationServerPid ! delete.



% Returns the PID of the identification server.
%
% (static)
%
-spec get_server() -> identification_server_pid().
get_server() ->
	naming_utils:get_registered_pid_for( ?id_server_name, global ).



% Returns the PID of the identification server, if any.
%
% (static)
%
-spec get_any_server() -> basic_utils:maybe( identification_server_pid() ).
get_any_server() ->

	case naming_utils:is_registered( ?id_server_name, global ) of

		not_registered ->
			undefined;

		Pid ->
			Pid

	end.



% Creates an external identifier in the form of a binary string, from specified
% block PID.
%
% Returns for example <<"sim-diasca-0.57.0">>.
%
% (static)
%
-spec forge_external_identifier( block_pid() ) -> text_utils:bin_string().
forge_external_identifier( Pid ) when is_pid( Pid ) ->

	ExtIdString = text_utils:format( "sim-diasca-~s",
									 [ text_utils:pid_to_string( Pid ) ] ),

	text_utils:string_to_binary( ExtIdString ).




% Helper section:


% Throws an exception because specified external identifier could not be
% resolved.
%
% (helper)
%
-spec throw_on_external_id_not_found( external_id(), wooper:state() ) ->
											no_return().
throw_on_external_id_not_found( ExternalIdentifier, State ) ->

	% Should they be not textual already:
	ExtIdStrings = lists:sort( [ text_utils:format( "~p", [ K ] )
			   || K <- table:keys( ?getAttr(inbound_table) ) ] ),

	?error_fmt( "External identifier '~p' not found among the ~B known ones:~s",
				[ ExternalIdentifier, length( ExtIdStrings ),
				  text_utils:strings_to_string( ExtIdStrings ) ] ),

	throw( { unknown_external_identifier, ExternalIdentifier } ).



% Returns a textual description of the state of this identification server.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	IndentationLevel = 0,

	InString = inbound_to_string( IndentationLevel, ?getAttr(inbound_table) ),

	OutString = outbound_to_string( IndentationLevel,
									?getAttr(outbound_table) ),

	text_utils:format( "Identification server with an ~s and with an ~s",
					   [ InString, OutString ] ).



% Returns a textual description of the specified inbound table.
%
-spec inbound_to_string( text_utils:indentation_level(), inbound_table() ) ->
							   string().
inbound_to_string( IndentationLevel, InboundTable ) ->

	InEntries = table:enumerate( InboundTable ),

	case InEntries of

		[] ->
			"empty inbound table (not registering any external identifier)";

		_ ->

			InStrings = lists:sort( [ text_utils:format(
										"external identifier '~s' translated "
										"to block PID ~w", [ Ext, Pid ] )
									  || { Ext, Pid } <- InEntries ] ),

			ListString = text_utils:strings_to_string( InStrings,
													   IndentationLevel ),

			text_utils:format( "inbound table able to convert ~B external "
							   "identifiers into as many block PIDs:~s",
							   [ length( InEntries ), ListString ] )

	end.



% Returns a textual description of the specified outbound table.
%
-spec outbound_to_string( text_utils:indentation_level(), outbound_table() ) ->
								string().
outbound_to_string( IndentationLevel, OutboundTable ) ->

	OutEntries = table:enumerate( OutboundTable ),

	case OutEntries of

		[] ->
			"empty outbound table (not registering any block PID)";

		_ ->

			OutStrings = lists:sort( [ text_utils:format(
										 "block PID ~w translated to external"
										 " identifier '~s'", [ Pid, Ext ] )
									   || { Pid, Ext } <- OutEntries ] ),

			ListString = text_utils:strings_to_string( OutStrings,
													   IndentationLevel ),

			text_utils:format( "outbound table able to convert ~B block PIDs "
							   "into as many external identifiers:~s",
							   [ length( OutEntries ), ListString ] )

	end.
