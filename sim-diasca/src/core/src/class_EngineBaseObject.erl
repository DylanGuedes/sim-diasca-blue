% Copyright (C) 2017-2017 EDF R&D

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



% Abstract base class common to all Sim-Diasca instances, whence most actual
% classes are to inherit, directly or not.
%
% Useful to be able to introduce uniformly common, transverse, possibly new
% behaviours such as trace sending, serialization, etc. in the engine.
%
-module(class_EngineBaseObject).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_TraceEmitter ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, InstanceName ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (template pasted, only two replacements were performed to update arities)
-define( wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2 ).



% Member method declarations:
%
-define( wooper_method_export, ).


% Static  method declarations:
%
-define( wooper_static_method_export, ).


% The PID of an engine base object:
-type object_pid() :: class_TraceEmitter:emitter_pid().

-export_type([ object_pid/0 ]).


% Helpers:
-export([ init/1, to_string/1 ]).


% To have the trace messages adequatly sorted:
-define( trace_emitter_categorization, "Core" ).


% Root of the Sim-Diasca class hierarchy.



% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").



% For the trace_categorize/1 macro:
-include("class_TraceEmitter.hrl").


% Implementation notes:
%
% No trace_emitter_categorization defined, as meant to be an abstract base class
% (necessarily overridden).

% No class-specific attribute defined.




% Constructs a new, named Sim-Diasca base object.
%
-spec construct( wooper:state(), class_TraceEmitter:emitter_init() ) ->
					   wooper:state().
construct( State, InstanceName ) ->

	% Only direct mother class, updated state returned directly:
	class_TraceEmitter:construct( State, ?trace_categorize( InstanceName ) ).





% Helper section.



% Initializes some context-specific information.
%
% (helper)
%
-spec init( wooper:state() ) -> wooper:state().
init( State ) ->
	class_TraceEmitter:init( State ).



% Returns a textual description of this instance.
%
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->
	text_utils:format( "Sim-Diasca base object named '~s'",
					   [ ?getAttr(name) ] ).
