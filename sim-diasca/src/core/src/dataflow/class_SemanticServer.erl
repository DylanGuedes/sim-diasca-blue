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



% Class in charge of managing semantic information.
%
% Generally instantiated as a singleton.
%
-module(class_SemanticServer).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_EngineBaseObject ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, MinDistance ).



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
-define( wooper_method_export, declareVocabulary/2, declareSemantics/2,
		 validateSemantics/2, getStatus/1 ).


% Static method declarations:
%
-define( wooper_static_method_export, start/0, start/1,
		 declare_vocabulary/2, are_semantics_compliant/2,
		 transform_as_internal/1, stop/0, stop/1, get_server/0 ).


% Design notes:
%
% Currently the semantics are only checked for equality and to ensure that their
% texts are not too close one from each other (useful to detect typos, case
% mismatches, etc.).


% A user-level element of a semantics is a symbol (a plain string):
-type user_semantics() :: rdf_utils:string_iri().

% A user-level vocabulary is a list of semantic elements (plain strings):
-type user_vocabulary() :: rdf_utils:user_vocabulary().



% An (internal) element of a semantics is a symbol (a binary string):
-type semantics() :: rdf_utils:subject().

% An (internal) vocabulary is a set of semantic elements (binary strings):
-type vocabulary() :: rdf_utils:vocabulary().


% PID of the semantic server:
-type semantic_server_pid() :: pid().


-export_type([ user_semantics/0, user_vocabulary/0, semantics/0, vocabulary/0,
			   semantic_server_pid/0 ]).


% String version of a vocabulary:
-type string_vocabulary() :: [ rdf_utils:string_iri() ].


% Possible outcomes of a semantic validation (possibly involving multiple
% semantics):
%
-type validation_outcome() :: 'semantics_accepted'
				| { 'semantics_rejected', basic_utils:error_reason() }.


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% To have the trace messages adequatly sorted:
-define( trace_emitter_categorization, "Core.Dataflow.Semantics" ).


% For registration:
-define( semantic_server_name, sim_diasca_semantic_server ).


% Default minimum lexicographic distance allowed between two semantic elements
% (Levenshtein distance):
%
-define( default_min_distance, 1 ).


% Allows to use macros for trace sending:

-include("class_TraceEmitter.hrl").




% Implementation notes:
%
% Internal IRIs could have been atoms (rather than binary strings).


% The specific attributes of a semantic server are:
%
% - vocabulary :: vocabulary() is the set of the known semantic elements, for
% fast inclusing look-up
%
% - string_vocabulary :: string_vocabulary() is the corresponding string-based
% version of vocabulary, for faster distance computations (a list with the same
% elements of the 'vocabulary' attribute, in unspecified order); allows to avoid
% countless conversions from binaries to strings
%
% - min_distance :: text_utils:distance() is the minimum lexicographic distance
% allowed between two semantic elements (Levenshtein distance)



% Constructs a new semantic server.
%
% - MinDistance is the minimum lexicographic distance allowed between two
% semantic elements (Levenshtein distance)
%
-spec construct( wooper:state(), text_utils:distance() ) -> wooper:state().
construct( State, MinDistance ) ->

	% First the direct mother class:
	TraceState = class_EngineBaseObject:construct( State,
							   ?trace_categorize( "SemanticServer" ) ),

	naming_utils:register_as( self(), ?semantic_server_name, global_only ),

	?send_info_fmt( TraceState, "Semantic server started, relying on a minimum "
								"lexicographic Levenshtein distance of ~B.",
					[ MinDistance ] ),

	% Then the class-specific actions:
	setAttributes( TraceState, [
		{ vocabulary, set_utils:new() },
		{ string_vocabulary, [] },
		{ min_distance, MinDistance } ] ).



-spec destruct( wooper:state() ) -> wooper:state().

% To avoid the Vocabulary variable being reported as unused because of the
% two-liner:

-ifdef(tracing_activated).


destruct( State ) ->

	Vocabulary = ?getAttr(string_vocabulary),

	?info_fmt( "Stopping semantic server, vocabulary had following "
			   "~B elements:~s",
			   [ length( Vocabulary ),
				 text_utils:strings_to_string( Vocabulary ) ] ),

	State.


-else. % tracing_activated


destruct( State ) ->
	State.


-endif. % tracing_activated





% Methods section.


% Declares specified vocabulary (expected to be a list of binary strings) to
% this server.
%
% (oneway)
%
-spec declareVocabulary( wooper:state(), basic_utils:unchecked_data() ) ->
								 wooper:oneway_result().
declareVocabulary( State, Vocabulary ) when is_list( Vocabulary ) ->

	DeclaredState = declare_semantics( Vocabulary, State ),

	?wooper_return_state_only( DeclaredState ).



% Declares either a single semantic element (expected to be a binary string) or
% a list thereof to this server.
%
% (oneway)
%
-spec declareSemantics( wooper:state(), basic_utils:unchecked_data() ) ->
								 wooper:oneway_result().
% Single element clause:
declareSemantics( State, SemanticElement ) when is_binary( SemanticElement ) ->

	case add_semantic_element( SemanticElement, State ) of

		{ semantics_accepted, DeclaredState } ->
			?debug_fmt( "Declaration of semantic element '~s' accepted.",
						[ SemanticElement ] ),
			?wooper_return_state_only( DeclaredState );

		{ { semantics_rejected, Reason }, _NewState } ->
			?error_fmt( "Declaration of semantic element '~s' rejected, "
						"reason: ~p", [ SemanticElement, Reason ] ),
			throw( { semantics_rejected, SemanticElement, Reason } )

	end;


% Multiple elements here (to centralise the trace outcomes in the correct
% cases):
declareSemantics( State, SemanticElements ) when is_list( SemanticElements ) ->

	% Throws an exception, should an element be rejected:
	DeclaredState = declare_semantics( SemanticElements, State ),

	?debug_fmt( "Declaration of semantic elements '~p' accepted.",
				[ SemanticElements ] ),

	?wooper_return_state_only( DeclaredState ).




% Tells whether specified semantic element (specified as a binary string) or a
% list thereof, are valid.
%
% (request, for result and synchronisation)
%
-spec validateSemantics( wooper:state(), basic_utils:unchecked_data() ) ->
							   wooper:request_result( validation_outcome() ).
validateSemantics( State, SemanticElement ) when is_binary( SemanticElement ) ->

	%?debug_fmt( "Checking semantic element '~s'.", [ SemanticElement ] ),
	{ Outcome, NewState } = add_semantic_element( SemanticElement, State ),

	%trace_utils:debug_fmt( "Single-element outcome: ~p.", [ Outcome ] ),
	?wooper_return_state_result( NewState, Outcome );


validateSemantics( State, SemanticElements ) when is_list( SemanticElements ) ->

	%?debug_fmt( "Checking semantic elements '~p'.", [ SemanticElements ] ),
	{ Outcome, NewState } = add_semantic_elements( SemanticElements, State ),

	%trace_utils:debug_fmt( "Multiple-element outcome: ~p.", [ Outcome ] ),
	?wooper_return_state_result( NewState, Outcome ).



% Returns a textual description of the state of this semantic server.
%
% (const request)
%
-spec getStatus( wooper:state() ) -> request_return( string() ).
getStatus( State ) ->
	?wooper_return_state_result( State, to_string( State ) ).





% Static section.


% Launches the semantic server, with default settings.
%
% (static)
%
-spec start() -> semantic_server_pid().
start() ->
	start( ?default_min_distance ).



% Launches the semantic server, with specified minimum lexicographic distance
% allowed between two semantic elements (Levenshtein distance).
%
% (static)
%
-spec start( text_utils:distance() ) -> semantic_server_pid().
start( MinDistance ) ->
	new_link( MinDistance ).




% Declares specified user-level vocabulary.
%
% (static)
%
-spec declare_vocabulary( user_vocabulary(), semantic_server_pid() ) ->
							   basic_utils:void().
declare_vocabulary( Vocabulary, SemanticServerPid )
  when is_list( Vocabulary ) ->

	% To reduce messaging payload, switching from user_semantics() to
	% semantics():
	BinVocabulary = [ text_utils:string_to_binary( S ) || S <- Vocabulary ],

	SemanticServerPid ! { declareVocabulary, [ BinVocabulary ] }.



% Tells whether specified semantics are compliant.
%
% Currently we consider that this is the case iff the emitter semantics are
% a subset of the receiver ones.
%
% (static)
%
-spec are_semantics_compliant( semantics(), semantics() ) -> boolean().
are_semantics_compliant( EmitterSemantics, ReceiverSemantics ) ->
	set_utils:is_subset( EmitterSemantics, ReceiverSemantics ).



% Transforms a user-level vocabulary into one in internal form.
%
% (static)
%
-spec transform_as_internal( user_vocabulary() ) -> vocabulary().
transform_as_internal( UserVocabulary ) when is_list( UserVocabulary ) ->
	BinSemantics = text_utils:strings_to_binaries( UserVocabulary ),
	set_utils:from_list( BinSemantics ).



% Stops (asynchronously) the semantic server.
%
% (static)
%
-spec stop() -> basic_utils:void().
stop() ->
	SemServerPid = get_server(),
	stop( SemServerPid ).



% Stops (asynchronously) the specified semantic server.
%
% (static)
%
-spec stop( semantic_server_pid() ) -> basic_utils:void().
stop( SemServerPid ) ->
	SemServerPid ! delete.



% Returns the PID of the semantic server (if any).
%
-spec get_server() -> semantic_server_pid().
get_server() ->
	naming_utils:get_registered_pid_for( ?semantic_server_name, global ).




% Helper section.


% Declares a list of semantics (that are binary strings).
%
% (helper)
%
-spec declare_semantics( [ semantics() ], wooper:state() ) -> wooper:state().
declare_semantics( _SemanticElements=[], State ) ->
	State;

declare_semantics( _SemanticElements=[ E | T ], State ) when is_binary( E ) ->

	case add_semantic_element( E, State ) of

		{ semantics_accepted, NewState } ->
			declare_semantics( T, NewState );

		{ { semantics_rejected, Reason }, _NewState } ->
			?error_fmt( "Declaration of semantic element '~s' rejected, "
						"reason: ~p", [ E, Reason ] ),
			throw( { semantics_rejected, E, Reason } )

	end;

declare_semantics( _SemanticElements=[ E | _T ], _State ) ->
	throw( { non_binary_semantics, E } ).



% Adds specified semantic element, if accepted.
%
% Note: the caller shall ensure that this element is a binary string indeed.
%
% (helper)
%
-spec add_semantic_element( semantics(), wooper:state() ) ->
								  { validation_outcome(), wooper:state() }.
add_semantic_element( SemanticElement, State ) ->

	Vocabulary = ?getAttr(vocabulary),

	case set_utils:member( SemanticElement, Vocabulary ) of

		true ->
			% Already registered, hence accepted:
			{ semantics_accepted, State };

		false ->

			% New element, hence more (string-based) checking:

			% Both structures needed in both cases:
			StringSemanticElement = text_utils:binary_to_string(
									  SemanticElement ),

			StringVocabulary = ?getAttr(string_vocabulary),

			case ?getAttr(min_distance) of

				0 ->
					% No restriction here, hence directly included (shortcut):

					%?trace_fmt( "Semantic element '~s' directly accepted.",
					%			[ SemanticElement ] ),

					IncState = include_element( SemanticElement,
						 StringSemanticElement, Vocabulary, StringVocabulary,
						 State ),

					{ semantics_accepted, IncState };


				MinDistance ->

					% We will now check the distance between this element and
					% all the ones in the internal vocabulary:

					case is_valid_semantics( StringSemanticElement,
											 StringVocabulary, MinDistance ) of

						true ->
							%?trace_fmt( "Semantic element '~s' accepted.",
							%			[ SemanticElement ] ),

							IncState = include_element( SemanticElement,
										   StringSemanticElement, Vocabulary,
										   StringVocabulary, State ),

							{ semantics_accepted, IncState };

						{ false, Reason } ->
							%?trace_fmt( "Semantic element '~s' rejected, "
							% "reason: ~p.", [ SemanticElement, Reason ] ),

							{ { semantics_rejected, Reason }, State }

					end

			end

	end.



% Adds specified semantic elements, if accepted.
%
% Returns an accepted outcome iff all elements are valid.
%
% (helper)
%
-spec add_semantic_elements( [ semantics() ], wooper:state() ) ->
								   { validation_outcome(), wooper:state() }.
add_semantic_elements( _Elements=[], State ) ->
	{ semantics_accepted, State };

add_semantic_elements( _Elements=[ E | T ], State ) when is_binary( E ) ->
	case add_semantic_element( E, State ) of

		{ semantics_accepted, AcceptedState } ->
			add_semantic_elements( T, AcceptedState );

		Rejection -> % = { { semantics_rejected, Reason }, RejectedState } ->
			% Stop the recursion on first rejection:
			Rejection

	end.



% Tells whether specified semantics is valid, knowing it is a string here, and
% that it does not belong to the specified vocabulary.
%
% Returns either true or { false, Reason }.
%
-spec is_valid_semantics( string(), string_vocabulary(),
				  text_utils:distance() ) -> 'true' | { 'false', term() }.
is_valid_semantics( SemanticElement, VocabularyStrings, MinDistance ) ->

	case get_minimal_distance( SemanticElement, VocabularyStrings ) of

		none ->
			% Empty vocabulary here:
			true;

		% Insufficient lexicographic distance:
		{ D, Closest } when D < MinDistance ->
			{ false, { semantics_too_close, { SemanticElement, Closest } } };

		% Sufficient lexicographic distance:
		_ ->
			true

	end.



% (helper)
%
-spec include_element( semantics(), rdf_utils:string_iri(), vocabulary(),
					   string_vocabulary(), wooper:state() ) -> wooper:state().
include_element( SemanticElement, StringSemanticElement, Vocabulary,
				 StringVocabulary, State ) ->

	NewVocabulary = set_utils:add( SemanticElement, Vocabulary ),
	NewStringVocabulary = [ StringSemanticElement | StringVocabulary ],

	setAttributes( State, [ { vocabulary, NewVocabulary },
							{ string_vocabulary, NewStringVocabulary } ] ).



% Returns the minimal lexicographic distance between the specified semantic
% element and vocabulary.
%
% Returns either 'none' if the vocabulary is empty, or { MinimalDistance,
% CorrespondingVocabularyElement }.
%
-spec get_minimal_distance( rdf_utils:string_iri(), string_vocabulary() ) ->
							  'none' | { text_utils:distance(), semantics() }.
get_minimal_distance( SemanticElement, Vocabulary ) ->
	get_minimal_distance( SemanticElement, Vocabulary, _Min=none ).


get_minimal_distance( _SemanticElement, _Vocabulary=[], Min ) ->
	Min;

% Having a first element allows to have an actual minimum:
get_minimal_distance( SemanticElement, _Vocabulary=[ E | T ], _Min=none ) ->

	Dist = text_utils:get_lexicographic_distance( SemanticElement, E ),

	NewMin = { Dist, E },

	get_minimal_distance( SemanticElement, T, NewMin );


get_minimal_distance( SemanticElement, _Vocabulary=[ E | T ],
					  Min={ MinDist, _MinElem } ) ->

	case text_utils:get_lexicographic_distance( SemanticElement, E ) of

		D when D < MinDist ->
			NewMin = { D, E },
			get_minimal_distance( SemanticElement, T, NewMin );

		_ ->
			get_minimal_distance( SemanticElement, T, Min )

	end.



% Returns a textual description of the state of this semantic server.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->

	VocString = case ?getAttr(string_vocabulary) of

		[] ->
			"no vocabulary";

		Vocabulary ->
			text_utils:format( "a vocabulary of ~B semantic elements, namely, "
							   "in alphabetical order:~s",
							   [ length( Vocabulary ),
								 text_utils:strings_to_string(
								   lists:sort( Vocabulary ) ) ] )

	end,

	text_utils:format( "Semantic server relying on a minimum Levenshtein "
					   "distance of ~B and having ~s",
					   [ ?getAttr(min_distance), VocString ] ).
