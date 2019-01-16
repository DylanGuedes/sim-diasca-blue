% Copyright (C) 2008-2017 EDF R&D

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


% Locatable class, base of all instances having in-world 3D coordinates.
%
-module(class_Locatable).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [] ).


% Parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, Location ).


% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, two replacements performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2 ).


% Member method declarations.
-define( wooper_method_export, getLocation/1, setLocation/2,
		 getAbscissa/1, setAbscissa/2,
		 getOrdinate/1, setOrdinate/2,
		 getAltitude/1, setAltitude/2 ).


% Helper functions.
-export([ describe_location/1 ]).


% Describes a location in the 3D context:
-type location() :: linear_3D:point().



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
%-define(trace_emitter_categorization,"Locatable").


% Allows to use macros for trace sending:
%-include("class_TraceEmitter.hrl").



% Constructs a new locatable instance, based on a record of an in-world
% location.
%
-spec construct( wooper:state(), location() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% First the direct mother classes, then this class-specific actions:
	%TraceState = class_TraceEmitter:construct( State, "Locatable" ),

	%?send_info_fmt( TraceState,
	%	"Creating a new locatable whose location is ~s.",
	%	[ space:location_to_string( Location ) ] ),

	setAttribute( State, location, Location ).




% Methods section.



% Returns the in-world location of this locatable.
%
% (const request)
%
-spec getLocation( wooper:state() ) -> request_return( location() ).
getLocation( State ) ->
	?wooper_return_state_result( State, ?getAttr(location) ).



% Sets the in-world location of this locatable.
%
% (oneway)
%
-spec setLocation( wooper:state(), location() ) -> oneway_return().
setLocation( State, NewLocation ) ->
	?wooper_return_state_only( setAttribute( State, location, NewLocation) ).



% Returns the in-world abscissa of this locatable.
%
% (const request)
%
-spec getAbscissa( wooper:state() ) -> request_return( linear:coordinate() ).
getAbscissa( State ) ->

	{ X, _Y, _Z } = ?getAttr(location),

	?wooper_return_state_result( State, X ).



% Sets the in-world abscissa of this locatable.
%
% (oneway)
%
-spec setAbscissa( wooper:state(), linear:coordinate() ) -> oneway_return().
setAbscissa( State, NewX ) ->

	{ _X, Y, Z } = ?getAttr(location),

	?wooper_return_state_only(
	   setAttribute( State, location, { NewX, Y, Z } ) ).



% Returns the in-world ordinate of this locatable.
%
% (const request)
%
-spec getOrdinate( wooper:state() ) -> request_return( linear:coordinate() ).
getOrdinate( State ) ->

	{ _X, Y, _Z } = ?getAttr(location),

	?wooper_return_state_result( State, Y ).



% Sets the in-world ordinate of this locatable.
%
% (oneway)
%
-spec setOrdinate( wooper:state(), linear:coordinate() ) -> oneway_return().
setOrdinate( State, NewY ) ->

	{ X, _Y, Z } = ?getAttr(location),

	?wooper_return_state_only(
	   setAttribute( State, location, { X, NewY, Z } ) ).



% Returns the in-world altitude of this locatable.
%
% (const request)
%
-spec getAltitude( wooper:state() ) -> request_return( linear:coordinate() ).
getAltitude( State ) ->

	{ _X, _Y, Z } = ?getAttr(location),

	?wooper_return_state_result( State, Z ).



% Sets the in-world altitude of this locatable.
%
% (oneway)
%
-spec setAltitude( wooper:state(), linear:coordinate() ) -> oneway_return().
setAltitude( State, NewZ ) ->

	{ X, Y, _Z } = ?getAttr(location),

	?wooper_return_state_only(
	   setAttribute( State, location, { X, Y, NewZ } ) ).




% Section for helper functions (not methods).

% Returns the location of this Locatable.
%
% Note: is never and cannot be overloaded.
%
-spec describe_location( wooper:state() ) -> string().
describe_location( State ) ->
	io_lib:format( "~p", [ ?getAttr(location) ] ).
