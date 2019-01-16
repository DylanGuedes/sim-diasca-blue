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



% Load balancer.


% Its role is to manage all creation requests of simulation actors.
%
% It will ensure that the corresponding processes are dispatched according to
% specified placement policy and spawned in a reproducible order.
%
% The actors will be assigned unique and reproducible abstract actor identifiers
% (AAI), as opposed to their PID, with is a technical non-reproducible
% identifier.
%
% The actor identifier is simply an incrementing counter managed by the load
% balancer, knowing that this agent is expected to be a singleton (only one
% instance of the load balancer should run at any time for a given simulation).
%
% Moreover the load balancer is able to perform conversions between actor
% identifiers and PID, in both directions, in order to answer to look-up
% requests.
%
% The load balancer is itself an actor, as it needs to perform a reproducible
% reordering of the creation requests it receives.
%
% A shorthand for 'Abstract Actor Identifier' is AAI.
%
% The load balancer is usually created by the deployment manager.
%
% See also class_LoadBalancer_test.erl.
%
-module(class_LoadBalancer).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_BroadcastingActor ] ).



% Parameters taken by the constructor ('construct').
% These are class-specific data needing to be set in the constructor:
-define( wooper_construct_parameters, PlacementPolicy, Nodes,
		 NodeAvailabilityTolerance, SimulationMode, TroubleshootingMode,
		 InitialisationFiles ).



% Declaring all variations of WOOPER standard life-cycle operations:
% (just a matter of a copy/paste followed by the replacement of arities)
-define( wooper_construct_export, new/6, new_link/6,
		 synchronous_new/6, synchronous_new_link/6,
		 synchronous_timed_new/6, synchronous_timed_new_link/6,
		 remote_new/7, remote_new_link/7, remote_synchronous_new/7,
		 remote_synchronous_new_link/7, remote_synchronisable_new_link/7,
		 remote_synchronous_timed_new/7, remote_synchronous_timed_new_link/7,
		 construct/7, destruct/1 ).


% Member method declarations.
-define( wooper_method_export, simulationEnded/1, timeManagerShutdown/1,
		 getComputingNodes/1,
		 createInitialInstancesFromFiles/3, onInstancesLoaded/1,
		 createInitialActor/4, createInitialPlacedActor/5,
		 createInitialActors/3,
		 createRuntimeActor/4, createRuntimeActor/5,
		 createRuntimePlacedActor/5, createRuntimePlacedActor/6,
		 spawn_successful/2,
		 getActorCreationInformation/4, getActorCreationInformationFromHint/4,
		 getNodeForUserIdentifier/2, registerInitialActors/2,
		 actSpontaneous/1, notifyDeletion/4, getInstanceCounts/1,
		 getOverallInstanceCount/2, traceState/2 ).


% Static methods declarations.
-define( wooper_static_method_export, settings_to_string/1,
		 get_registration_name/0, get_balancer/0 ).


% Hint for actor placement (co-allocation):
%
% Used based on its hash value.
%
-type placement_hint() :: any().


% The policies for actor placement.
%
-type placement_policy() :: 'round_robin' | 'select_least_loaded_first'.


% How non-responding, unavailable computing nodes shall be managed:
%
-type node_availability_tolerance() :: 'fail_on_unavailable_node'
									 | 'allow_unavailable_nodes'.


-export_type([ placement_hint/0, placement_policy/0,
			   node_availability_tolerance/0 ]).



% We need serialisation hooks to take care of internal helper processes:
-define( wooper_serialisation_hooks, ).


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.LoadBalancing" ).


% For notify_debug_fmt:
-include("traces.hrl").

% Allows to use macros for trace sending:
-include("class_TraceEmitter.hrl").


% For load_balancer_name:
-include("class_LoadBalancer.hrl").


% For evaluation_mode(), evaluation_requested_properties():
-include("class_TimeManager.hrl").


% Where the load balancer should be registered.
%
% Could be 'global_only' as well:
%
-define( registration_type, local_and_global ).



% Defines the default constant seed used in reproducible mode.
%
% Note that {0,0,0} does not seem to be a proper seed in pre-R15B versions for
% the 'random' module (not a problem with the latter default modes of operation
% we use).
%
-define( default_reproducible_seed, {1,7,11} ).




% Implementation notes.



% Actor identifier section.

% An actor identifier is a reproducible abstract (non technically-dependent)
% identifier of a simulation actor, a.k.a. AAI, for 'Abstract Actor Identifier'.
%
% It is a strictly positive integer, starting at 1 and incremented by the load
% balancer each time it creates an actor.
%
% During a simulation, there is a bijection between actor identifiers and the
% PID of their corresponding Erlang processes.
%
% From a simulation to another, actor PIDs will most probably change (ex: if the
% number of computing nodes varies, or if the placement policy in use takes
% dynamically into account the load of the computers), but a given actor should
% always have the same actor identifier assigned to it.


% The load balancer has no real life of its own, it is mainly triggered whenever
% an actor is to be created, so it can remain mostly passive.


% Previously the load balancer had to send information about created actor
% instances to the relevant instance tracker and to maintain a node-based
% tracker look-up table to target the right one.
%
% The load balancer does not communicate anymore with an instance tracker, as it
% could not manage at least one useful actor-level information: the name of that
% actor. Thus each actor had, one way or another, to update anyway a tracker
% with its name (preferentially when subscribing to its time manager).
%
% Having to send its name, the actor can go a little further and provide all the
% information needed at once (ex: classname), and the load balancer does not
% need anymore to be involved there, simplifying the overall applicative
% protocol.


% Previously the seeding of each actor was done by its local time manager. Now,
% as anyway the load balancer is able to send information to each actor when
% creating it (ex: its AAI), its seed is specified as well.
%
% This design is simpler, and easily allows for a seeding which does not depend
% on the number of available computing nodes, as it is centralised by the
% load-balancer (instead of being distributed among time managers, which would
% make the outcome of the simulations depend on the number of involved computing
% hosts).
%
% Hence to any actor / AAI a unique, reproducible seed must correspond. This is
% an issue when loading instances from file, as they are presented to the load
% balancer in any order. As a result, the loader balancer must maintain a table
% of pre-generated random seeds, indexed by the corresponding AAI. As this table
% may grow and shrink (depending on the order of the actor presentations), a
% table is more convenient (see seed_table).


% The load balancer is both a simulation agent and an actor, which requires, in
% some cases, extra care (ex: when deserialising a simulation).



% Regarding initialisation:
%
% Creating instances before the simulation starts can be done either
% programmatically (typically directly from the simulation case, from the method
% of a scenario, etc.) or based on data (typically from a set of initialisation
% strams or files).
%
% The 'base_actor_identifier' attribute is set depending on these modes,
% respectively to 'undefined' or to the value at which the
% 'next_actor_identifier' attribute was set when switching from code-based (the
% default) to file-based: the AAI assigned to instances read from file then
% increment from base_actor_identifier onward, based on the line number of their
% definition in the read file. As not all lines of these files correspond to
% creation requests (ex: blank line or comment), some AAI may never be assigned
% (that is not a problem).


% Regarding instance creation:
%
% It is now managed asynchronously internally (yet still synchronously in terms
% of API), to allow for nested creations (actors that create actors from their
% constructor). So now the load balancer should be interacted with thanks to
% oneway messages only, and it has to track the spawn initiators so that, when a
% created instance reports it is ready, the load balancer can notify its
% initiator.
%
% As single or multiple creations can be requested, the load-balancer stores two
% tables:
%
% - spawn_table, which can convert the PID of a spawned actor (its keys) into
% the PID of the corresponding initiator (the associated value), to find the
% relevant initiator (typically the simulation case itself for initial
% creations, and an actor for runtime creations) when a spawn_successful is
% received
%
% - initiator_requests, which can convert a PID of an initiator into a list of:
%
%  * either single PID, the spawned one, to know it was a single-spawn request,
%  and thus that the initiator can be directly notified
%
%  * or pairs of two lists of PIDs; the first is the original, full, ordered
%  list of the created PIDs immediately returned at spawn time; the second
%  begins as an exact copy thereof, yet each time a spawn success is reported,
%  the corresponding PID is removed from it; when this list becomes empty, then
%  the previous one can be returned to the initiator, as it contains by design
%  the right PIDs in the right order
%
% For runtime creations, a per-creation tag can be specified; it allows avoiding
% the sending back of the construction parameters. As initial creations are
% usually synchronously managed and anyway are in-order, no specific tag
% applies.
%

% Creating actors (ex: A2, A3) from constructors (ex: from the one of A1) cannot
% be done, as A1 is by design not synchronised yet to the simulation (hence it
% cannot send an actor message to the load balancer for that).
%
% If, in the future, this was allowed, then the 3 following paragraphs would
% apply:

% Creating actors (say, A2 and A3) from any actor oneway (including
% onFirstDiasca/2) of an actor (say, A1) means that, when the first actor, A1,
% is requested to be created by A0 thanks to class_Actor:create_actor/3 (A0
% sending a createRuntime actor message to the load balancer),
% createRuntime/{4,5} *must not return* until A1 and thus any nested actors it
% created from its constructurs (A2, A3, etc.) are successfully spawned.
%
% Indeed, if createRuntime/{4,5} was not blocking, then the load balancer would
% notify immediately after it triggered the spawn of A1 that its diasca ended,
% and from then the simulation would progress arbitrarily until the
% corresponding spawn_successful message(s) are received, thus too late.
%
% So the createRuntime variations (with or without tag, placed or not) override
% the main loop of the load balancer, to serve creation requests and spawn
% notifications, blocking until A1 finally reached the end of its constructor
% and thus reported that its spawn succeeded.




% Describes a computing node for the load-balancing:
%
-record( compute_node, {

	% The name of this computing node, as an atom:
	name :: net_utils:atom_host_name()

} ).


-type compute_node() :: #compute_node{}.



% All state attributes of the load balancer are explained here:
%
% - placement_policy :: placement_policy() describes what is the current
% placement policy
%
% - node_availability_tolerance :: node_availability_tolerance() tells how we
% should react, should a computing node be unavailable
%
% - next_actor_identifier :: class_Actor:aai() corresponds to the AAI that will
% be assigned to the *next* created actor (if any); it starts at 2, as the load
% balancer itself is an actor, having the first AAI (1); as a consequence, the
% total number of actual created actors, as long as no initialisation file is
% loaded and no instance is deleted, is 'next_actor_identifier - 2'
%
% - spawn_table :: table( class_Actor:actor_pid(), pid() ), which can convert
% the PID of a spawned actor (its keys) into the PID of the corresponding
% initiator (the associated value), to find the relevant initiator when a
% spawn_successful is received
%
% - initiator_requests :: table( pid(), class_Actor:actor_pid() | {
% class_Actor:actor_pid(), class_Actor:tag() } | { [ class_Actor:actor_pid() ],
% [ class_Actor:actor_pid() ] } ), which can convert a PID of an initiator into:
%
%  * a single PID (corresponding to the spawned initial actor), to know it was
%  an (initial) single-spawn request, and thus that the initiator can be
%  directly notified
%
%  * or a pair aggregating a single PID (corresponding to the spawned runtime
%  actor) and the associated tag, in order the spawn initiator to be able to
%  discriminate among pending creation requests
%
%  * or a pair of two lists of PIDs, for batch initial creations; the first is
%  the original, full, ordered list of the created PIDs immediately returned at
%  spawn time; the second begins as an exact copy thereof, yet each time a spawn
%  success is reported, the corresponding PID is removed from it; when this list
%  becomes empty, then the previous one can be returned to the initiator, as it
%  contains by design the right PIDs in the right order
%
% - base_actor_identifier :: class_Actor:aai() is always set to 'undefined'
% unless initialisation files are read, in which case it is set to the then
% current next_actor_identifier minus 1; then created instances will have for
% AAI the addition of this base AAI and their line number in the creation file;
% as line are numbered from 1 onward, the first instance will have thus, for
% AAI, this 'next_actor_identifier', the next one will have
% 'next_actor_identifier + 1', etc.
%
% - current_actor_count ::basic_utils:count() keeps track of the current number
% of living actors in the simulation (contrary to next_actor_identifier, it can
% decrease, due to actor deletions)
%
% - instances_per_class :: table:table( class_name(), { basic_utils:count(),
% basic_utils:count() } ) is a table whose keys are class names (as atoms) and
% whose associated values are pairs where CreationCounter keeps track of the
% overall instance creation count for that class (regardless of deletions), and
% DeletionCounter keeps track of the overall instance deletion count for that
% class
%
% - instances_per_node :: table:table( net_utils:atom_node_name(),
% basic_utils:count() ) is a table whose keys are names of computing nodes and
% whose associated values are the number of current actors on this computing
% node (allowing thus to determine the number of existing instances for each
% class)
%
% - initial_actors :: [ pid() ] is a list of the PIDs of all initial actors; it
% is used so that, on simulation start, this load balancer can notify all of
% them that their first diasca is happening (onFirstDiasca actor oneway)
%
% - initialisation_files :: [ file_utils:path() ] is a list of absolute paths
% (as the current directory has to change over time) to initialisation files,
% from which initial instances will be created
%
% - deployment_manager_pid :: maybe( pid() ) allows telling the deployment
% manager that the initialisation data for instances has been processed
%
% - compute_nodes :: [ compute_node() ] is a list of compute_node records,
% describing the available Erlang nodes on which actors should be created
%
% - placement_policy_data is:
%
%  - with the round-robin policy: ring_utils:ring( net_utils:atom_node_name() ),
%  i.e. a ring allowing to iterate continuously on the computing nodes
%
% - seed :: random_utils::seed() corresponds to the root seed this load-balancer
% begins with, depending on the simulation settings
%
% - seed_table :: table:table( class_Actor::aai(), random_utils:seed() ) is a
% table whose keys are AAIs of instances being loaded and whose values are the
% seed corresponding to each of these AAIs
%
% - troubleshooting_mode :: boolean() tells whether the troubleshooting mode is
% enabled





% Constructs a new load balancer, from following parameters:
%
% - PlacementPolicy describes which heuristic should be used to dispatch created
% actors onto computing nodes; following placement policies are specified:
%
%  - round_robin: one of the simplest scheduling algorithms, which assigns
%  actors to computing nodes in equal portions and in order, handling all
%  creation requests without priority. Round-robin scheduling is both simple and
%  easy to implement, and starvation-free. It relies on the hypothesis that all
%  actors consume on average a similar amount of resource and that all computing
%  nodes provide on average a similar amount of resource; see also:
%  http://en.wikipedia.org/wiki/Round-robin_scheduling
%
%  - select_least_loaded_first: the load balancer will evaluate the current load
%  of the computing nodes, and then will choose create any new actor on the
%  least loaded node (not implemented yet)
%
% - Nodes :: [ net_utils:atom_node_name() ] is a list of Erlang nodes (as atoms)
% that are to take part to the simulation, i.e. that are eligible as running
% environments for actors
%
% - NodeAvailabilityTolerance can be:
%
%  - fail_on_unavailable_node: the construction of the load balancer will fail
%  if at least one of the specified nodes is not available
%
%  - allow_unavailable_nodes: all nodes found not available will be rejected,
%  and the simulation will rely only on the remaining ones
%
% - EvaluationMode :: evaluation_requested_properties() provides the load
% balancer with all information to properly seed each actor
%
% - TroubleshootingMode :: boolean() tells whether the troubleshooting mode is
% activated
%
% - InitialisationFiles :: [ file_utils:path() ] is a list of initialisation
% files, from which initial instances will be created
%
% A node might be unavailable because its host is unavailable, or because the
% node cannot be run on its available host.
%
% A load balancer is also an actor (to create reproducibly other actors), and
% thus is linked to a time manager. As the load balancer is created on the same
% node as the root time manager, its time manager is the root one.
%
-spec construct( wooper:state(), placement_policy(),
				 [ net_utils:atom_node_name() ], node_availability_tolerance(),
				 evaluation_mode(), boolean(), [ file_utils:path() ] ) ->
					   wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	% The load-balancer is a potential bottleneck of the architecture, insofar
	% as, for example, it will have to interact with the initial actors, which
	% may be *very* numerous (onFirstDiasca):
	%
	erlang:process_flag( priority, _Level=high ),

	{ Seed, SeedInfoString, OrderingMode } = manage_seeding( SimulationMode ),

	% The load balancer is the only actor that is created in an ad hoc way:
	BalancerSeed = random_utils:get_random_seed(),

	BalancerSettings = #actor_settings{
					 aai=1,
					 seed=BalancerSeed,
					 message_ordering_mode=OrderingMode
				   },

	% By convention the load balancer assigns to itself the first AAI, 1:
	InitialState = class_BroadcastingActor:construct( State, BalancerSettings,
										?trace_categorize( "Load Balancer" ) ),

	% Then the class-specific actions:

	% We must create empty slots for nodes, so that the performance tracker does
	% not have to wait for an instance to be created on a node to see this node
	% listed:
	InitialInstancesPerNode = table:addEntries(
		 [ { N, _InstanceCount=0 } || N <- Nodes ], table:new() ),


	% Checking, as not all random generators can:
	true = random_utils:can_be_seeded(),

	TraceState = setAttributes( InitialState, [

		{ placement_policy, PlacementPolicy },
		{ node_availability_tolerance, NodeAvailabilityTolerance },
		{ seed, Seed },
		{ seed_table, undefined },
		{ message_ordering_mode, OrderingMode },
		{ next_actor_identifier, 2 },
		{ base_actor_identifier, undefined },
		{ spawn_table, table:new() },
		{ initiator_requests, table:new() },
		{ current_actor_count, 1 },
		{ instances_per_class, table:new() },
		{ instances_per_node, InitialInstancesPerNode },
		{ initial_actors, [] },
		{ initialisation_files, InitialisationFiles },
		{ deployment_manager_pid, undefined },
		{ troubleshooting_mode, TroubleshootingMode },

		% For bootstrapping purposes, the load balancer is the only actor that
		% starts with a non-empty agenda:
		{ current_agenda, [ 0 ] }

											   ] ),

	?send_info( TraceState, SeedInfoString ),

	SelectedComputingNodeRecords = inspect_computing_nodes( Nodes,
		NodeAvailabilityTolerance, TraceState ),

	% Anticipated checking (otherwise a function clause is raised):
	PlacementPolicyData = case PlacementPolicy of

		round_robin ->
			% Returns the ring of computing nodes:
			NodeList = get_node_list_from( SelectedComputingNodeRecords ),
			ring_utils:from_list( NodeList )

	end,

	SelectedCount = length( SelectedComputingNodeRecords ),

	?send_info_fmt( TraceState,
		"Creating a new load balancer whose placement policy is ~p, "
		"whose node tolerance is ~p, "
		"whose ~B validated computing nodes are:~n~s",
		[ PlacementPolicy, NodeAvailabilityTolerance, SelectedCount,
			 compute_nodes_to_string( SelectedComputingNodeRecords ) ] ),

	% Commented out, as this information is already given by the deployment
	% manager:

	%	case SelectedCount of

	%		1 ->
	%			io:format( "The single validated computing node is ~s.~n",
	%		 [ compute_node_to_string( hd( SelectedComputingNodeRecords ) ) ] );

	%		_More ->
	%			io:format( "The ~B validated computing nodes are:~n~s~n",
	%			  [ SelectedCount, compute_nodes_to_string(
	%                                  SelectedComputingNodeRecords ) ] )

	%	end,


	StartingState = setAttributes( TraceState, [

		{ compute_nodes, SelectedComputingNodeRecords },
		{ placement_policy_data, PlacementPolicyData }

	] ),

	% Ensures also it is a singleton indeed:
	naming_utils:register_as( ?load_balancer_name, ?registration_type ),

	class_InstanceTracker:register_agent( State ),

	StartingState.



% Overridden destructor.
%
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	%io:format( "Deleting load balancer ~w.~n", [ self() ] ),

	% Not counting this balancer itself:
	%
	?trace_fmt( "Deleting load balancer, while actual actor count was ~B "
				"(total created: ~B).",
				[ ?getAttr(current_actor_count) - 1,
				  ?getAttr(next_actor_identifier) - 2 ] ),

	class_InstanceTracker:unregister_agent(),

	naming_utils:unregister( ?load_balancer_name, ?registration_type ),

	?debug( "Load balancer deleted." ),

	% Then allow chaining:
	State.





% Methods section.



% Notifies this load balancer that the simulation ended.
%
% For the vast majority of actors (but unlike the load balancer), this means
% deletion (overridden for the load balancer, which has a different life cycle).
%
% (oneway)
%
-spec simulationEnded( wooper:state() ) -> oneway_return().
simulationEnded( State ) ->

	% Do not trigger a delete here.

	?wooper_return_state_only( State ).



% Reacts to a notification of time manager shutdown.
%
% Overridden from class_Actor, not wanting for this very particular actor to be
% deleted then (deletion to be managed by the deployment manager).
%
% (oneway)
%
-spec timeManagerShutdown( wooper:state() ) -> oneway_return().
timeManagerShutdown( State ) ->

	% Do not trigger a delete here.

	?wooper_return_state_only( State ).



% Returns the list of the records corresponding to the actual selected computing
% nodes.
%
% (const request)
%
-spec getComputingNodes( wooper:state() ) -> request_return( compute_node() ).
getComputingNodes( State ) ->
	?wooper_return_state_result( State, ?getAttr(compute_nodes) ).





% Section about actor creations, initial or not.


% Requests this load balancer to trigger the actual creation of the initial
% instances that were specified in the file(s) listed in the simulation
% settings.
%
% In all cases, the caller expects a 'instances_created_from_files' message to
% be ultimately returned.
%
% (oneway, to remain responsive to placement requests)
%
-spec createInitialInstancesFromFiles( wooper:state(), file_utils:path(),
									   pid() ) -> oneway_return().
createInitialInstancesFromFiles( State, DeploymentManagerPid, EngineRootDir ) ->

	?debug( "Creation of initial instances from files." ),

	% Checkings:
	undefined = ?getAttr(base_actor_identifier),
	undefined = ?getAttr(seed_table),

	DeployState = setAttribute( State, deployment_manager_pid,
								DeploymentManagerPid ),

	LastState = case ?getAttr(initialisation_files) of


		[] ->
			% No need to create a useless instance loading process then:
			?debug( "No initialisation files specified." ),

			% The asynchronous loading notification is immediate in this case:
			self() ! onInstancesLoaded,

			DeployState;


		InitialisationFiles ->

			% Previously we were accessing the initialisation files from the
			% deployed archive. This allowed to create the load balancer on any
			% node, yet the compressing, sending and decompressing of these data
			% could become way too long; so now we assume again that the load
			% balancer is always created on the user host, hence that it can
			% access the initialisation files readily, directly from the disk:
			%
			%RootDir = class_Actor:get_deployed_root_directory( State ),
			RootDir = EngineRootDir,

			ActualInitPaths = [ file_utils:join( RootDir, F )
								|| F <- InitialisationFiles ],

			%io:format( "Creating initial instances from files:~s",
			%		   [ text_utils:strings_to_string( ActualInitPaths ) ] ),

			% To trigger back the onInstancesLoaded oneway:
			spawn_link( _M=instance_loading,
						_F=manage_initialisation,
						_A=[ ActualInitPaths,
							 _NodeCount=length( ?getAttr(compute_nodes) ),
							 self() ] ),

			NextAAI = ?getAttr(next_actor_identifier),

			BaseAAI = NextAAI - 1,

			%io:format( "Setting base actor id to ~B (next AAI: ~B).~n",
			%		   [ BaseAAI, NextAAI ] ),

			setAttributes( DeployState, [

				{ base_actor_identifier, BaseAAI },
				{ seed_table, table:new() }

										 ] )

	end,

	?wooper_return_state_only( LastState ).



% Called (either by itself or by the spawned instance loader) whenever all
% instances have been loaded from files (if any).
%
% (oneway, allowing the load balancer not to be blocked in
% createInitialInstancesFromFiles/2)
%
-spec onInstancesLoaded( wooper:state() ) -> oneway_return().
onInstancesLoaded( State ) ->

	?debug( "All instances loaded from files (if any)." ),

	% Unblocks in turn the deployment manager:
	?getAttr(deployment_manager_pid) ! instances_created_from_files,

	% Switch back to normal AAI mode if necessary:
	%
	% (next_actor_identifier needs no update; current random seed already in
	% final, correct state)
	%
	% We do not check seed_table for emptyness as some {AAI,Seed} entries may be
	% still there (corresponding to empty lines or comments in the
	% initialisation files).
	%
	FinalState = setAttributes( State, [

					{ base_actor_identifier, undefined },
					{ seed_table, undefined }

									   ] ),

	?wooper_return_state_only( FinalState ).



% Creates specified actor on an automatically selected computing node, while the
% simulation is not running (i.e. not to be called by actors wanting to create
% other actors while the simulation is running, see createRuntimeActor/{4,5}
% instead).
%
% Mostly meant to be called directly from simulation scenarios, test cases,
% etc. to recreate the initial situation before the simulation is started;
% needed for simulation bootstrap.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Will trigger back a onInitialActorCreated/2 oneway message so that the caller
% is notified both of the successful creation and of its corresponding PID.
%
% (oneway)
%
-spec createInitialActor( wooper:state(), class_name(), [ method_argument() ],
						  pid() ) -> oneway_return().
createInitialActor( State, ActorClassName, ActorConstructionParameters,
					InitiatorPid ) ->

	%io:format( "Load-balancer creating initial actor of class ~s.~n",
	%		   [ ActorClassName ] ),

	% Checks that the simulation is not started yet:
	false = class_Actor:is_running( State ),

	% Checks that we are not involved in the reading of an initialisation file:
	undefined = ?getAttr(base_actor_identifier),

	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	{ UpdatedState, ActorPid } = create_initial_actor( ActorClassName,
	   ActorConstructionParameters, SelectedNode, InitiatorPid, SelectedState ),

	% To be able to send 'onFirstDiasca' actor messages at simulation start:
	RecordedState = appendToAttribute( UpdatedState, initial_actors, ActorPid ),

	% onInitialActorCreated/2 triggered on the initiator by spawn_successful/2.

	?wooper_return_state_only( RecordedState ).



% Creates specified actor on a computing node which is entirely determined by
% the specified placement hint, while the simulation is not running yet
% (i.e. not to be called by actors wanting to create other actors while the
% simulation is running - see createPlacedOtherActor/4 instead).
%
% Mostly meant to be called directly from simulation scenarios, test cases,
% etc. to recreate the initial situation before the simulation is started;
% needed for bootstrap.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Will trigger back a onInitialActorCreated/2 oneway message so that the caller
% is notified both of the successful creation and of its corresponding PID.
%
% (oneway)
%
-spec createInitialPlacedActor( wooper:state(), class_name(),
		  [ method_argument() ], placement_hint(), pid() ) -> oneway_return().
createInitialPlacedActor( State, ActorClassName, ActorConstructionParameters,
						  PlacementHint, InitiatorPid ) ->

	% Same structure as createInitialActor/4 above:

	% Checks that the simulation is not started yet:
	false = class_Actor:is_running( State ),

	% Checks that we are not involved in the reading of an initialisation file:
	undefined = ?getAttr(base_actor_identifier),

	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	{ UpdatedState, ActorPid } = create_initial_actor( ActorClassName,
	   ActorConstructionParameters, SelectedNode, InitiatorPid, State ),

	% To be able to send 'onFirstDiasca' actor messages at simulation start:
	RecordedState = appendToAttribute( UpdatedState, initial_actors, ActorPid ),

	% onInitialActorCreated/2 triggered on the initiator by spawn_successful/2.

	?wooper_return_state_only( RecordedState ).



% Creates the specified list of (initial) actors, each on an automatically
% selected computing node, while the simulation is not running yet (i.e. not to
% be called by actors wanting to create other actors while the simulation is
% running - see createRuntimeActor/4 instead).
%
% Mostly meant to be called directly from simulation scenarios, test cases,
% etc. to recreate the initial situation before the simulation is started;
% needed for simulation bootstrap.
%
% The ActorConstructionList parameter is a list of specifications for actor
% creation (each made of a tuple containing an actor class name, a list of
% construction parameters and, possibly, a placement hint).
%
% Actor creations will be done as much as possible in parallel, over the
% available computing nodes: this request is to be used for bulk actor
% creations.
%
% Will trigger back a onInitialActorsCreated/2 oneway message so that the caller
% is notified both of the successful creations and of its corresponding PIDs.
%
% (oneway)
%
-spec createInitialActors( wooper:state(),
						   [ class_Actor:instance_creation_spec() ], pid() ) ->
								 oneway_return().
createInitialActors( State, InstanceCreationSpecs, InitiatorPid ) ->

	% Checks that the simulation is not started yet:
	false = class_Actor:is_running( State ),

	% Checks that we are not involved in the reading of an initialisation file:
	undefined = ?getAttr(base_actor_identifier),

	%CreationCount = length( InstanceCreationSpecs ),

	%io:format( "Load-balancer creating ~B initial actors for "
	%			"initiator ~w.~n", [ CreationCount, InitiatorPid ] ),

	% We could imagine as well sending batches of creations to each node
	% (i.e. aggregating a set of creations, sent in one message to each node,
	% instead of sending one message per creation):

	% Next enhancement: creates instances directly when placement is determined.
	{ CreationInfos, NewState } = place_and_prepare_creations(
									InstanceCreationSpecs, State ),

	NewActorPids = [

	  begin

		  CreatedPid = apply( Classname, remote_synchronisable_new_link,
							  PlacedConstructionParams ),

		  %io:format( " - constructing a (now placed) instance of ~s "
		  %			 " with ~p, its PID is ~w~n",
		  %			 [ Classname, PlacedConstructionParams, CreatedPid ] ),

		  CreatedPid

	  end
	  || { Classname, PlacedConstructionParams } <- CreationInfos ],

	% To be able to send 'onFirstDiasca' actor messages at simulation start:
	NewInitialActors = ?getAttr(initial_actors) ++ NewActorPids,

	% Associate all these new actors to this initiator:
	SpawnEntries = [ { APid, InitiatorPid } || APid <- NewActorPids ],

	NewSpawnTable = table:addEntries( SpawnEntries, ?getAttr(spawn_table) ),

	InitiatorTable = ?getAttr(initiator_requests),

	% Reference and depletion lists:
	NewSpawnEntry = { NewActorPids, NewActorPids },

	NewInitiatorTable = case table:lookupEntry( InitiatorPid,
												InitiatorTable ) of

		key_not_found ->
			% The list will contain a single list for the moment:
			table:addEntry( InitiatorPid, [ NewSpawnEntry ], InitiatorTable );

		{ value, InitiatorList } ->
			% Another (list) item added (list not appended):
			table:addEntry( InitiatorPid, [ NewSpawnEntry | InitiatorList ],
							InitiatorTable )

	end,

	FinalState = setAttributes( NewState, [
						{ initial_actors, NewInitialActors },
						{ spawn_table, NewSpawnTable },
						{ initiator_requests, NewInitiatorTable }
										   ] ),

	% onInitialActorsCreated/2 will be triggered on the initiator side by
	% spawn_successful/2.

	?wooper_return_state_only( FinalState ).



% Places the instances as specified by the creation specs, and returns all
% information needed to create them immediately, with an updated state that
% considers that these creations are done.
%
% (helper)
%
-spec place_and_prepare_creations( [ class_Actor:instance_creation_spec() ],
								   wooper:state() ) ->
			{ [ { class_name(), [ method_argument() ] } ], wooper:state() }.
place_and_prepare_creations( InstanceCreationSpecs, State ) ->

	FirstAai = ?getAttr(next_actor_identifier),

	InstancesPerClass = ?getAttr(instances_per_class),
	InstancesPerNode = ?getAttr(instances_per_node),

	place_and_prepare_creations( InstanceCreationSpecs, _CurrentAAI=FirstAai,
								 InstancesPerClass, InstancesPerNode,
								 _AccCreationInfos=[], State ).



% (helper)
%
place_and_prepare_creations( _InstanceCreationSpecs=[], LastAAI,
							 InstancesPerClass, InstancesPerNode,
							 AccCreationInfos, State ) ->

	% All creations managed, updating the state now:

	% Last minus previous current one:
	CreationCount = LastAAI - ?getAttr(next_actor_identifier),

	NewActorCount = ?getAttr(current_actor_count) + CreationCount,

	NewState = setAttributes( State, [

		{ next_actor_identifier, LastAAI },
		{ current_actor_count, NewActorCount },
		{ instances_per_class, InstancesPerClass },
		{ instances_per_node, InstancesPerNode }

							  ] ),

	% The contract is to preserve the order of creation information:
	{ lists:reverse( AccCreationInfos ), NewState };



place_and_prepare_creations( _InstanceCreationSpecs=[
				{ Classname, ConstructionParameters } | T ] ,
							 CurrentAAI,
							 InstancesPerClass, InstancesPerNode,
							 AccCreationInfos, State ) ->

	% No placement hint specified here, hence using default policy:
	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	prepare_creations( Classname, ConstructionParameters, SelectedNode,
					   T, CurrentAAI, InstancesPerClass, InstancesPerNode,
					   AccCreationInfos, SelectedState );

place_and_prepare_creations( _InstanceCreationSpecs=[
				{ Classname, ConstructionParameters, PlacementHint } | T ] ,
							 CurrentAAI, InstancesPerClass, InstancesPerNode,
							 AccCreationInfos, State ) ->

	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	prepare_creations( Classname, ConstructionParameters, SelectedNode,
					   T, CurrentAAI, InstancesPerClass, InstancesPerNode,
					   AccCreationInfos, State ).



% Registers creation and recurses.
%
% (helper, gathering creations that are placed or not)
%
prepare_creations( Classname, ConstructionParameters, SelectedNode,
				   InstanceCreationSpecs, AAI, InstancesPerClass,
				   InstancesPerNode, CreationInfos, State ) ->

	display_synthetic_reporting( AAI, Classname, SelectedNode ),

	ActorSettings = get_actor_settings( AAI, State ),

	NewInstancesPerClass = record_creation_in_class_table( Classname,
														   InstancesPerClass ),

	NewInstancesPerNode = record_creation_in_node_table( SelectedNode,
														 InstancesPerNode ),

	CreationParams = [ SelectedNode, ActorSettings | ConstructionParameters ],

	CreationInfo = { Classname, CreationParams },

	place_and_prepare_creations( InstanceCreationSpecs, AAI + 1,
								 NewInstancesPerClass, NewInstancesPerNode,
								 [ CreationInfo | CreationInfos ], State ).




% Creates specified actor on an automatically selected computing node, at
% runtime, i.e. while the simulation is running (to be called by actors wanting
% to create other actors in the course of their behaviour).
%
% Primarily meant to be called transparently from an actor making use of the
% class_Actor:create_actor/3 helper function.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - SenderPid is the PID of the sender
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Triggers back on the caller (generally the actor at the origin of the creation
% request) the onActorCreated/4 actor oneway (with a tag determined by default),
% to notify the creating actor that the requested actor was created.
%
% The tag allows the caller to discriminate among multiple pending creation
% requests.
%
% No user tag is specified here, thus the calling actor will receive back the
% default tag, i.e. a { ActorClassName, ActorConstructionParameters } pair.
%
% (actor oneway)
%
-spec createRuntimeActor( wooper:state(), class_name(), [ method_argument() ],
						  class_Actor:actor_pid() ) -> oneway_return().
createRuntimeActor( State, ActorClassName, ActorConstructionParameters,
					SenderPid ) ->

	% No tag specified here, using default one then:
	DefaultTag = { ActorClassName, ActorConstructionParameters },

	createRuntimeActor( State, ActorClassName, ActorConstructionParameters,
						DefaultTag, SenderPid ).



% Creates specified actor on an automatically selected computing node, with a
% user-specified tag, while the simulation is running (i.e. to be called by
% actors wanting to create other actors in the course of their behaviour).
%
% Primarily meant to be called transparently from an actor making use of the
% class_Actor:create_actor/4 helper function.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - ActorTag is the user-defined tag to discriminate between its actor creation
%
% - SenderPid is the PID of the sender
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Triggers back on the caller (generally the actor at the origin of the creation
% request) the onActorCreated/4 actor oneway (with the specified tag), to notify
% the creating actor that the requested actor was created.
%
% The tag allows the caller to discriminate among multiple pending creation
% requests.
%
% (actor oneway)
%
-spec createRuntimeActor( wooper:state(), class_name(), [ method_argument() ],
			  class_Actor:tag(), class_Actor:actor_pid() ) -> oneway_return().
createRuntimeActor( State, ActorClassName, ActorConstructionParameters,
					ActorTag, SenderPid ) ->

	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	UpdatedState = create_runtime_actor( ActorClassName,
			ActorConstructionParameters, ActorTag, SelectedNode, SenderPid,
			SelectedState ),

	% We could send back directly to the initiator that the corresponding actor
	% is created (and its PID), yet the actual creation may spread over multiple
	% diascas (ex: if itself performing nested creations) and we prefer
	% validating a bit later rather than propagating a faulty PID), so the
	% initiator and the created actor will be notified later, only when the
	% spawn will be reported as successful, i.e. once the construction
	% succeeded.
	%
	% So these sendings will be done in spawn_successful/2:
	%
	%FirstSentState = class_BroadcastingActor:send_actor_message( SenderPid,
	%	{ onActorCreated, [ ActorPid, DefaultTag ] }, UpdatedState ),

	%SecondSentState = class_BroadcastingActor:send_actor_message( ActorPid,
	%										 onFirstDiasca, FirstSentState ),

	% No need to schedule the next diasca explicitly, as it is a by-product of
	% the sending of an actor message.

	?wooper_return_state_only( UpdatedState ).



% Creates specified actor on a computing node which is entirely determined by
% the specified placement hint, while the simulation is already running (i.e. to
% be called by actors wanting to create other actors in the course of their
% behaviour).
%
% Primarily meant to be called transparently from an actor making use of the
% class_Actor:create_placed_actor/{4,5} helper functions.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Triggers back on the caller (generally the actor at the origin of the creation
% request) the onActorCreated/4 actor oneway (with a tag determined by default),
% to notify the creating actor that the requested actor was created.
%
% The tag allows the caller to discriminate among multiple pending creation
% requests.
%
% No user tag is specified here, thus the calling actor will receive back the
% default tag, i.e. a { ActorClassName, ActorConstructionParameters } pair.
%
% (actor oneway)
%
-spec createRuntimePlacedActor( wooper:state(), class_name(),
			[ method_argument() ], placement_hint(), pid() ) -> oneway_return().
createRuntimePlacedActor( State, ActorClassName, ActorConstructionParameters,
				   PlacementHint, SenderPid ) ->

	DefaultTag = { ActorClassName, ActorConstructionParameters },

	createRuntimePlacedActor( State, ActorClassName,
		  ActorConstructionParameters, DefaultTag, PlacementHint, SenderPid ).



% Creates specified actor, with a user-specified tag, on a computing node which
% is entirely determined by the specified placement hint, while the simulation
% is running yet (i.e. to be called by actors wanting to create other actors in
% the course of their behaviour).
%
% Primarily meant to be called transparently from an actor making use of the
% class_Actor:create_placed_actor/{4,5} helper functions.
%
% Method parameters are:
%
% - ActorClassName is the classname of the actor to create (ex:
% 'class_TestActor')
%
% - ActorConstructionParameters is the list of parameters that will be used to
% construct that actor (ex: [ "MyActorName", 50 ])
%
% - ActorTag is the user-defined tag to discriminate between its actor creation
%
% - PlacementHint can be any Erlang term (ex: an atom); it allows to create all
% actors (both initial or simulation-time ones) for which the same placement
% hint was specified on the same computing node, for best performances
%
% The actor will be created with following parameters: first its target node,
% then its AAI, then all the parameters in ActorConstructionParameters.
%
% Triggers back on the caller (generally the actor at the origin of the creation
% request) the onActorCreated/4 actor oneway (with the specified tag), to notify
% the creating actor that the requested actor was created.
%
% The tag allows the caller to discriminate among multiple pending creation
% requests.
%
% (actor oneway)
%
-spec createRuntimePlacedActor( wooper:state(), class_Actor:tag(), class_name(),
		[ method_argument() ], placement_hint(), pid() ) -> oneway_return().
createRuntimePlacedActor( State, ActorClassName, ActorConstructionParameters,
						  ActorTag, PlacementHint, SenderPid ) ->

	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	UpdatedState = create_runtime_actor( ActorClassName,
		ActorConstructionParameters, ActorTag, SelectedNode, SenderPid, State ),

	% A user tag was specified here, thus relying on it:

	% Done when notified of successful spawn:

	%FirstSentState = class_BroadcastingActor:send_actor_message( SenderPid,
	%	{ onActorCreated, [ ActorPid, ActorTag ] }, UpdatedState ),

	%SecondSentState = class_BroadcastingActor:send_actor_message( ActorPid,
	%										onFirstDiasca, FirstSentState ),

	% No need to schedule the next tick explicitly, as it is a by-product of the
	% sending of an actor message.

	?wooper_return_state_only( UpdatedState ).




% Notification sent by a created actor that its spawn is successful.
%
% (standard WOOPER message sent after a '*synchronisable_new' call, interpreted
% here as a oneway)
%
-spec spawn_successful( wooper:state(), class_Actor:actor_pid() ) ->
							  oneway_return().
spawn_successful( State, CreatedActorPid ) ->

	% For runtime creations, we expect these messages to be intercepted by the
	% inner receive done:
	%
	false = class_Actor:is_running( State ),

	NewState = spawn_successful_helper( CreatedActorPid, State ),

	?wooper_return_state_only( NewState ).



% Helper, common to initial and runtime creations.
%
% Returns an updated state.
%
spawn_successful_helper( CreatedActorPid, State ) ->

	%io:format( "Load-balancer received notification of spawn success "
	%            "for ~w.~n", [ CreatedActorPid ] ),

	% First, determines the corresponding spawn initiator:
	{ InitiatorPid, NewSpawnTable } = table:extractEntry( _K=CreatedActorPid,
												?getAttr(spawn_table) ),

	InitiatorTable = ?getAttr(initiator_requests),

	SpawnRequests = table:getEntry( InitiatorPid, InitiatorTable ),

	%io:format( "SpawnRequests = ~p~n", [ SpawnRequests ] ),

	% Searches for the spawned actor in the pending requests for this initiator,
	% to be found either as a single PID, or as a PID in the pair of lists:
	%
	{ NewSpawnRequests, NewState } = case search_for_spawn( CreatedActorPid,
											   SpawnRequests, _Acc=[] ) of

		{ single_initial, NewReqList } ->

			% Single-actor, tagless notification, thus an initial creation:
			false = class_Actor:is_running( State ),

			%io:format( "Load-balancer notifying initiator ~w of the creation "
			%		   "of initial actor ~w.~n",
			%		   [ InitiatorPid, CreatedActorPid ] ),

			% A PID, not a list on purpose:
			InitiatorPid ! { onInitialActorCreated, CreatedActorPid },

			{ NewReqList, State };


		{ not_last_initial, NewReqList } ->

			false = class_Actor:is_running( State ),

			%io:format( "Load-balancer will notify initiator ~w of the "
			%			"creation of initial actor ~w when its full list will "
			%           "be exhausted.~n", [ InitiatorPid, CreatedActorPid ] ),

			{ NewReqList, State };


		{ all_initial, NewReqList, ReferenceList } ->

			false = class_Actor:is_running( State ),

			%io:format( "Load-balancer notifying initiator ~w of the creation "
			%			"of initial actors ~w.~n",
			%			[ InitiatorPid, ReferenceList ] ),

			InitiatorPid ! { onInitialActorsCreated, [ ReferenceList ] },

			{ NewReqList, State };


		{ single_runtime, NewReqList, ActorTag } ->

			true = class_Actor:is_running( State ),

			%io:format( "Load-balancer notifying initiator actor ~w of the "
			%			"creation of runtime actor ~w (tag: ~p).~n",
			%			[ InitiatorPid, CreatedActorPid, ActorTag ] ),

			FirstSentState = class_BroadcastingActor:send_actor_message(
				InitiatorPid, { onActorCreated, [ CreatedActorPid, ActorTag ] },
				State ),

			SecondSentState = class_BroadcastingActor:send_actor_message(
				CreatedActorPid, onFirstDiasca, FirstSentState ),

			{ NewReqList, SecondSentState }


	end,

	% Maybe it was the last spawn entry:
	NewInitiatorTable = case NewSpawnRequests of

		[] ->
			% We prefer removing the full entry rather than having an empty
			% list associated to this initiator:
			%
			table:removeEntry( InitiatorPid, InitiatorTable );

		_ ->
			table:addEntry( InitiatorPid, NewSpawnRequests, InitiatorTable )

	end,

	% Finally, we unlink this actor from the load balancer, as by design it is
	% already linked with its own local time manager (otherwise the load
	% balancer would have a list of links as long as there are actors, hence not
	% scalable, knowing the time managers hierarchically divide the actor
	% population in manageable sets). Therefore at any moment an actor is always
	% linked to at least one simulation-stopper manager.
	%
	erlang:unlink( CreatedActorPid ),

	setAttributes( NewState, [
						   { spawn_table, NewSpawnTable },
						   { initiator_requests, NewInitiatorTable }
						  ] ).



% Searches for the PID of the created actor, removes it, and returns an updated
% spawn request:
%
% (we used to play initially with the size of tuples returned and the type of
% some of their elements to discriminate the outputs of this heler, yet it is
% too error-prone so tagged tuples were finally preferred)
%
search_for_spawn( CreatedActorPid, _ReqList=[], _Acc ) ->
	% This spawned actor should have been registered:
	throw( { inconsistent_initiator_table, CreatedActorPid } );

search_for_spawn( CreatedActorPid, _ReqList=[ CreatedActorPid | T ], Acc ) ->
	% Actor found as single initial spawn, thus removed:
	% (order does not matter for this list)
	NewReqList = T ++ Acc,
	{ single_initial, NewReqList };

search_for_spawn( CreatedActorPid, _ReqList=[ R={ RefList, DepleteList } | T ],
			Acc ) when is_list( RefList ) ->

	% This may be an initial actor in this pair of lists:
	case list_utils:delete_if_existing( CreatedActorPid, DepleteList ) of

		not_found ->
			% So must be in the remaining of the overall request list:
			search_for_spawn( CreatedActorPid, T, [ R | Acc ] );

		_DepletedList=[] ->
			% Found as was last; all actors of the list are thus spawned now:
			NewReqList = T ++ Acc,
			{ all_initial, NewReqList, RefList };

		DepletedList ->
			% Initial actor found, yet still others are waited in this list:
			NewListPair = { RefList, DepletedList },
			NewReqList = [ NewListPair | T ] ++ Acc,
			{ not_last_initial, NewReqList }

	end;

search_for_spawn( CreatedActorPid,
				  _ReqList=[ { CreatedActorPid, ActorTag } | T ], Acc ) ->
	% Actor found as (single) runtime spawn, thus removed:
	% (order does not matter for this list)
	NewReqList = T ++ Acc,
	{ single_runtime, NewReqList, ActorTag }.




% Returns, based on specified identifier-related information, the node on which
% the corresponding instance must be created, and which actor settings should be
% used for that.
%
% The specified identifier information may or may not be an actual identifier
% and, if yes, it may or may not be the identifier of this particular instance;
% a specified identifier is meant to be a mere placement guideline.
%
% Note: the load balancer does not create the corresponding instance, but
% considers that is will be created afterwards.
%
% (request)
%
-spec getActorCreationInformation( wooper:state(),
		instance_loading:identifier_info(), instance_loading:line_number(),
		class_name() ) -> request_return(
			{ net_utils:atom_node_name(), class_Actor:actor_settings() } ).
getActorCreationInformation( State, _IdentifierInfo=none, LineNumber,
							 Classname ) ->

	% Here, no specific node can be deduced, we just rely on the default
	% placement policy:
	%
	{ SelectedState, SelectedNode } = select_node_by_heuristic( State ),

	{ LastState, ActorSettings } = register_created_instance( SelectedNode,
									  LineNumber, Classname, SelectedState ),

	?wooper_return_state_result( LastState, { SelectedNode, ActorSettings } );


getActorCreationInformation( State, _IdentifierInfo=UserIdentifier, LineNumber,
							 Classname ) ->

	% Here we have a user identifier (not necessarily the one of the instance to
	% create), which we use as a placement hint:
	%
	SelectedNode = select_node_based_on_hint( _PlacementHint=UserIdentifier,
											  State ),

	{ LastState, ActorSettings } = register_created_instance( SelectedNode,
									  LineNumber, Classname, State ),

	?wooper_return_state_result( LastState, { SelectedNode, ActorSettings } ).



% Returns, based on specified placement hint, the node on which the
% corresponding instance must be created, and which actor settings should be
% used for that.
%
% Note: the load balancer does not create the corresponding instance, but
% considers that is will be created afterwards.
%
% (request)
%
-spec getActorCreationInformationFromHint( wooper:state(),
		placement_hint(), instance_loading:line_number(),
		class_name() ) -> request_return(
			{ net_utils:atom_node_name(), class_Actor:actor_settings() } ).
getActorCreationInformationFromHint( State, PlacementHint, LineNumber,
									 Classname ) ->

	% Here, supposedly no user identifier applies, we thus solely rely on the
	% placement hint instead:
	%
	SelectedNode = select_node_based_on_hint( PlacementHint, State ),

	{ LastState, ActorSettings } = register_created_instance( SelectedNode,
									  LineNumber, Classname, State ),

	?wooper_return_state_result( LastState, { SelectedNode, ActorSettings } ).



% Returns the computing node on which the instance corresponding to the
% specified user identifier shall be created.
%
% (const request)
%
-spec getNodeForUserIdentifier( wooper:state(),
			instance_loading:user_identifier() ) ->
							request_return( net_utils:atom_node_name()).
getNodeForUserIdentifier( State, UserIdentifier ) ->

	SelectedNode = select_node_based_on_hint( _PlacementHint=UserIdentifier,
											  State ),

	?wooper_return_state_result( State, SelectedNode ).



% Registers specified eady created initial actors, from their PID.
%
% (request)
%
-spec registerInitialActors( wooper:state(), [ pid() ] ) ->
							   request_return( 'initial_actors_registered' ).
registerInitialActors( State, AdditionalInitialActors ) ->

	NewInitialActors = AdditionalInitialActors ++ ?getAttr(initial_actors),

	% Actor count and class/node table expected to be already updated.

	NewState = setAttribute( State, initial_actors, NewInitialActors ),

	?wooper_return_state_result( NewState, initial_actors_registered ).



% Registers a (probably initial) instance created externally (typically while
% loading them from file).
%
% Returns an updated state and the actor's creation settings.
%
% (helper)
%
register_created_instance( TargetNode, LineNumber, Classname, State ) ->

	% Check would have no effect: undefined =/= ?getAttr(base_actor_identifier),
	case ?getAttr(base_actor_identifier) of

		undefined ->
			throw( invalid_loading_condition );

		_ ->
			ok

	end,

	% LineNumber > 0, base derived from next AAI:
	ActorAai = ?getAttr(base_actor_identifier) + LineNumber,

	NextAAI = ?getAttr(next_actor_identifier),

	% Because of chunks and parallelism, lines are processed in an arbitrary
	% order, not according to their line number:
	%
	NewNextAAI = max( NextAAI, ActorAai + 1 ),

	%io:format( "register_created_instance: assigned AAI ~B, "
	%			"new next AAI is ~B.~n", [ ActorAai, NewNextAAI ] ),

	%?debug_fmt( "Registration of the creation of actor of "
	%			"class ~s on ~w, with AAI ~B.",
	%			[ Classname, TargetNode, ActorAai ] ),

	{ ActorSettings, LoadState } = get_loaded_actor_settings( ActorAai, State ),

	NewActorCount = ?getAttr(current_actor_count) + 1,

	NewClassTable = record_creation_in_class_table( Classname,
											 ?getAttr(instances_per_class) ),

	NewNodeTable = record_creation_in_node_table( TargetNode,
											 ?getAttr(instances_per_node) ),

	% We have not the PID here, hence we cannot updated initial_actors.

	FinalState = setAttributes( LoadState, [

				   { next_actor_identifier, NewNextAAI },
				   { current_actor_count, NewActorCount },
				   { instances_per_class, NewClassTable },
				   { instances_per_node, NewNodeTable }

										   ] ),

	{ FinalState, ActorSettings }.



% Overridden so that initial actors can be triggered for their first diasca,
% with their onFirstDiasca/2 actor oneway.
%
% This method is itself called because the load balancer is always scheduled for
% a spontaneous behaviour at tick offset 0 (diasca 0).
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	% Initial actors may be very numerous (potentially, millions). So we use the
	% class_BroadcastingActor implementation to rely on more efficient larger
	% (table-based) containers instead of plain lists; but, even with these,
	% large simulations would be too demanding; so we prefer smoothing the load
	% and creating as many diascas as needed for that, notifying initial actors
	% of their first diasca by chunks of, say, a few thousands actors.

	%InitialActorsCount = length( ?getAttr(initial_actors) ) ,

	%?debug_fmt( "Notifying the ~B initial actors of their first diasca.",
	%			[ InitialActorsCount ] ),

	class_PluginManager:notify( on_simulation_bootstrap_start ),

	?debug( "Notifying the initial actors of their first diasca." ),

	%io:format( "Notifying all ~B initial actors of their first diasca "
	%		   "at ~s.~n",
	%		   [ InitialActorsCount, time_utils:get_textual_timestamp() ] ),

	% Will ultimately exhaust the initial_actors list:
	TriggeredState = class_BroadcastingActor:send_actor_messages_over_diascas(
						initial_actors, onFirstDiasca, State ),

	?debug( "All initial actors just notified of their first diasca, "
			"waiting for their processing." ),

	%io:format( "All actors notified of their first diasca at ~s.~n",
	%		 [ time_utils:get_textual_timestamp() ] ),

	% No more spontaneous schedulings planned for the load balancer.

	class_PluginManager:notify( on_simulation_bootstrap_stop ),

	?wooper_return_state_only( TriggeredState ).



% Allows to keep track of actor deletion as well, in this single, centralized
% place.
%
% (oneway)
%
-spec notifyDeletion( wooper:state(), class_Actor:actor_pid(), class_name(),
					  net_utils:atom_node_name() ) -> oneway_return().
notifyDeletion( State, _ActorPid, ActorClassname, Node ) ->

	%io:format( "## Deletion of actor ~p (~p) on node ~p.~n",
	%		  [ ActorPid, ActorClassname, Node ] ),

	NewActorCount = ?getAttr(current_actor_count) - 1,

	NewClassTable = record_deletion_in_class_table( ActorClassname,
										?getAttr(instances_per_class) ),

	NewNodeTable = record_deletion_in_node_table( Node,
										?getAttr(instances_per_node) ),

	?wooper_return_state_only( setAttributes( State, [

				{ current_actor_count, NewActorCount },
				{ instances_per_class, NewClassTable },
				{ instances_per_node,  NewNodeTable }

											   ] ) ).



% Returns the instance counts, per class and per node.
%
% Note: this involves operations that may be a bit expensive (enumeration and
% sending of the result) but this request is called only when the performance
% tracking is activated, and this load balancer might be, if needed, created on
% the same node as the performance tracker.

% (const request)
%
-spec getInstanceCounts( wooper:state() ) -> request_return(
		{ 'instance_counts', table:entries(), table:entries() } ).
getInstanceCounts( State ) ->

	%io:format( "instances_per_node = ~p.~n",
	%		  [ table:enumerate( ?getAttr(instances_per_node) ) ] ),

	% The first atom is to allow for easier discrimination in terms of parallel
	% messages received by the performance tracker:
	%
	TimedCounts = { instance_counts,
					table:enumerate( ?getAttr(instances_per_class) ),
					table:enumerate( ?getAttr(instances_per_node) )
				   },

	?wooper_return_state_result( State, TimedCounts ).



% Returns (asynchronously) the overall number of model instances.
%
% Used notably by the root time manager so that the console tracker can display
% actor counts.
%
% (oneway)
%
-spec getOverallInstanceCount( wooper:state(), pid() ) -> oneway_return().
getOverallInstanceCount( State, CallerPid ) ->

	CallerPid ! { notifyOverallActorCount, ?getAttr(current_actor_count) },

	?wooper_return_state_only( State ).



% Requests this load-balancer to trace its state, using specified label for
% that.
%
% (const oneway)
%
-spec traceState( wooper:state(), string() ) -> oneway_return().
traceState( State, Label ) ->
	trace_state( Label, State ),
	?wooper_return_state_only( State ).



% Static methods section.


% Returns a textual description of specified load balancing settings record.
%
% (static)
%
-spec settings_to_string( #load_balancing_settings{} ) -> string().
settings_to_string( LoadBalancingSettings ) ->

	#load_balancing_settings{

		placement_policy = Placement

	} = LoadBalancingSettings,

	PlacementString = "placement policy will be " ++ case Placement of

		round_robin ->
			"round-robin"

	end,

	text_utils:string_list_to_string( [ PlacementString ] ).



% Returns the atom corresponding to the name the load balancer should be
% registered as.
%
% Note: executed on the caller node.
%
% (static)
%
-spec get_registration_name() -> naming_utils:registration_name().
get_registration_name() ->
	% Ex: 'sim_diasca_load_balancer':
	?load_balancer_name.



% Returns the PID of the (unique) load balancer.
%
% (static method, to be used by clients of the load balancer)
%
-spec get_balancer() -> pid().
get_balancer() ->
	naming_utils:wait_for_global_registration_of( get_registration_name() ).






% Section for helper functions (not methods).


% Interprets the specified seeding and reordering information, for
% initialisation.
%
-spec manage_seeding( evaluation_requested_properties() ) ->
		 { random_utils:seed(), string(), class_Actor:message_ordering_mode() }.
manage_seeding( fastest ) ->

	% Using the default (constant) seed here, even if no reordering will be
	% performed: stochastic actors need a seed anyway.
	%
	% (even in fastest mode, a random seed is needed as otherwise stochastic
	% actors would all behave the same)
	%
	DefaultSeed = ?default_reproducible_seed,

	random_utils:start_random_source( DefaultSeed ),

	% No hash-based sorting, no permutation:
	{ DefaultSeed, io_lib:format( "Simulation will run in fastest mode, "
								  "with no message reordering, and using "
								  "default seed (~p).", [ DefaultSeed ] ),
	  unordered } ;


manage_seeding( reproducible ) ->

	% A random seed is needed for stochastic actors, even if in reproducible
	% mode.

	% Root seed is a default (constant) seed here:
	DefaultSeed = ?default_reproducible_seed,

	random_utils:start_random_source( DefaultSeed ),

	% Hash-based sorting, no permutation (using the 'identity' here which is not
	% any less true than others):
	%
	{ DefaultSeed, io_lib:format( "Simulation will be totally reproducible, "
								  "using default seed (~p).", [ DefaultSeed ] ),
	  constant_arbitrary_order } ;


manage_seeding( { reproducible, SetSeed={ A, B, C } } ) ->

	% Random seed used for uniform permutations of messages *and* for stochastic
	% variables.

	% Using the user-specified seed:
	random_utils:start_random_source( A, B, C ),

	% Hash-based sorting and permutation needed here:
	{ SetSeed, io_lib:format( "Simulation will be totally reproducible, "
							  "using user-specified seed ~p.", [ SetSeed ] ),
	  constant_permuted_order } ;


manage_seeding( ergodic ) ->

	% Random seed used for uniform permutations of messages *and* for stochastic
	% variables:

	% Use a time-based seed, i.e. a seed that should not be the same twice
	% (since it is based on wallclock-time):
	%
	% (not using the time_based_seed parameter, as we want to report what is the
	% actual seed used)
	%
	TimeSeed = { A, B, C } = time_utils:get_precise_timestamp(),

	random_utils:start_random_source( A, B, C ),

	% Hash-based sorting and permutation needed here:
	{ TimeSeed, io_lib:format( "Simulation will run in ergodic mode, "
							   "using time-based seed ~p.", [ TimeSeed ] ),
	  constant_permuted_order }.



% Returns the actor settings corresponding to the next actor to be created, in
% the context of a direct, programmatic creation.
%
% (helper)
%
-spec get_actor_settings( class_Actor:aai(), wooper:state() ) ->
								class_Actor:actor_settings().
get_actor_settings( AAI, State ) ->

	% Picks up a new seed:
	ActorSeed = random_utils:get_random_seed(),

	#actor_settings{
					 aai=AAI,
					 seed=ActorSeed,
					 message_ordering_mode=?getAttr(message_ordering_mode)
				   }.



% Returns the actor settings corresponding to the specified actor to be created,
% in the context of a loading-based creation, and an updated state.
%
% (helper)
%
-spec get_loaded_actor_settings( class_Actor:aai(), wooper:state() ) ->
							{ class_Actor:actor_settings(), wooper:state() }.
get_loaded_actor_settings( AAI, State ) ->

	% Here we have to manage the fact that AAIs might be requested in any order,
	% whereas we want to associate them reproducible random seeds.
	%
	% As a consequence, we record in seed_table the AAI -> Seed associations for
	% the AAIs that are smaller than the specified one - all the prior AAIs
	% whose seed must be generated so that we can determine the seed for the
	% current AAI of interest.

	SeedTable = ?getAttr(seed_table),

	NextAAI = ?getAttr(next_actor_identifier),

	%io:format( "get_loaded_actor_settings: requesting AAI ~B, while table "
	%			"is: ~p.~n",
	%		   [ AAI, lists:sort( table:keys( SeedTable ) ) ] ),

	{ NewSeedTable, ActorSeed } = case table:lookupEntry( _K=AAI,
														  SeedTable ) of

		key_not_found ->

			% Seed for this AAI not computed yet, we will thus create all
			% intermediary ones between the last computed one and this one:
			%
			expand_seed_table( _From=NextAAI, _To=AAI, SeedTable );

		{ value, AlreadyAvailableSeed } ->
			LightenTable = table:removeEntry( _Key=AAI, SeedTable ),
			{ LightenTable, AlreadyAvailableSeed }

	end,

	ActorSettings = #actor_settings{
					 aai=AAI,
					 seed=ActorSeed,
					 message_ordering_mode=?getAttr(message_ordering_mode)
	},

	{ ActorSettings, setAttribute( State, seed_table, NewSeedTable ) }.



% Returns { NewSeedTable, ActorSeed }, where:
%
% - NewSeedTable is an expanded seed table, recording for all AAIs in [FromAAI,
% ToAAI] their seed (we include ToAAI as later we will probably have to expand
% again that table)
%
% - ActorSeed is the seed of actor whose AAI is ToAAI
%
expand_seed_table( _From=AAI, _To=AAI, SeedTable ) ->
	add_seed_for( AAI, SeedTable );

expand_seed_table( FromAAI, ToAAI, SeedTable ) ->
	{ NewSeedTable, _ActorSeed } = add_seed_for( FromAAI, SeedTable ),
	expand_seed_table( FromAAI + 1, ToAAI, NewSeedTable ).



% Adds a seed for specified AAI, and returns { NewSeedTable, ActorSeed }.
%
add_seed_for( AAI, SeedTable ) ->

	% Check:
	case table:hasEntry( _K=AAI, SeedTable ) of

		true ->
			throw( { not_overriding_aai, AAI } );

		false ->
			ok

	end,

	ActorSeed = random_utils:get_random_seed(),

	NewSeedTable = table:addEntry( _K=AAI, _V=ActorSeed, SeedTable ),

	{ NewSeedTable, ActorSeed }.




% Inspects the already launched nodes that can be used for the simulation.
%
% Nodes are specified by their names (strings).
%
% Returns a list of compute_node records, corresponding to available and running
% named Erlang nodes.
%
% The State variable is only needed to be able to send traces.
%
inspect_computing_nodes( NodeNames, NodeAvailabilityTolerance, State ) ->
	inspect_computing_nodes( NodeNames, NodeAvailabilityTolerance, _Acc=[],
							 State ).



inspect_computing_nodes( _NodeNames=[], _NodeAvailabilityTolerance, Acc,
						 _State ) ->
	Acc;

inspect_computing_nodes( [ NodeName| OtherNodes ], NodeAvailabilityTolerance,
						 Acc, State ) ->

	case net_utils:check_node_availability( NodeName, with_waiting ) of

		{ true, _Duration } ->
			NewRecord = create_compute_node_record_for( NodeName ),
			inspect_computing_nodes( OtherNodes, NodeAvailabilityTolerance,
									 [ NewRecord | Acc ], State );

		{ false, _Duration } ->
			case NodeAvailabilityTolerance of

				fail_on_unavailable_node ->
					?fatal_fmt( "Node named ~s not found available, "
								"hence not selected, and the load balancer "
								"settings do not allow that.", [ NodeName ] ),
					throw( { unavailable_computing_node, NodeName } );

				allow_unavailable_nodes ->
					?warning_fmt( "Node named ~s not found available, "
								  "hence not selected, but the load balancer "
								  "settings allow that.", [ NodeName ] ),
					inspect_computing_nodes( OtherNodes,
						NodeAvailabilityTolerance, Acc, State )

			end

	end.



compute_nodes_to_string( ComputeNodes ) ->
	compute_nodes_to_string( ComputeNodes, [] ).


compute_nodes_to_string( _ComputeNodes=[], Acc ) ->
	Acc;

compute_nodes_to_string( _ComputeNodes=[ H | T ], Acc ) ->
	compute_nodes_to_string( T,
		" + " ++ compute_node_to_string( H ) ++ "\n" ++ Acc ).


% Helper:
compute_node_to_string( ComputeNode ) ->
	% Extracts all fields:
	#compute_node{ name=Name } = ComputeNode,
	atom_to_list( Name ).



% Determines on which node the next actor should be created, according to the
% current placement policy.
%
% Returns an updated state and the determined node.
%
select_node_by_heuristic( State ) ->

	case ?getAttr(placement_policy) of

		round_robin ->
			select_node_with_round_robin( State )

	end.



% Determines on which node the next actor should be created, according to the
% round-robin placement policy.
%
% Returns an updated state and the determined node.
%
select_node_with_round_robin( State ) ->

	NodeRing = ?getAttr(placement_policy_data),

	{ SelectedNode, NewNodeRing } = ring_utils:head( NodeRing ),

	NewState = setAttribute( State, placement_policy_data, NewNodeRing ),

	{ NewState, SelectedNode }.



% Returns the node which corresponds to the specified placement hint.
%
% (const state, hence not returned)
%
select_node_based_on_hint( PlacementHint, State ) ->

	NodeRing = ?getAttr(placement_policy_data),

	NodeCount = ring_utils:size( NodeRing ),

	% Hash depends only on the hint, and will be in [1,NodeCount]:
	Hash = erlang:phash2( PlacementHint, NodeCount ) + 1,

	OriginalNodeList = ring_utils:get_reference_list( NodeRing ),

	% Returns the selected node:
	lists:nth( Hash, OriginalNodeList ).



% Returns the list of node names (as atoms) extracted from the list of computing
% node records.
%
get_node_list_from( ComputingNodeRecords ) ->
	get_node_list_from( ComputingNodeRecords, [] ).


get_node_list_from( _ComputingNodeRecords=[], Acc ) ->
	Acc;

get_node_list_from( _ComputingNodeRecords=[ #compute_node{ name=Nodename } | T],
					Acc ) ->
	get_node_list_from( T, [ Nodename | Acc ] ).



% To be called when needing to create a new initial actor, on specified node.
%
% Returns a pair made of an updated state and of the PID of the newly created
% actor, throws an exception on failure.
%
% To allow for nested creations, the creation is now asynchronous: this call
% will return while the spawned actor is possibly still being constructed; its
% PID is already known, yet, a synchronisable creation being used, this load
% balancer will wait until having received its spawn_successful message to deem
% it constructed and possibly to notify the requester of the corresponding
% creation.
%
% (internal helper function)
%
-spec create_initial_actor( class_name(), [ method_argument() ],
					net_utils:atom_node_name(), pid(), wooper:state() ) ->
						  { wooper:state(), pid() }.
create_initial_actor( ActorClassName, ActorConstructionParameters, Node,
					  InitiatorPid, State ) ->

	% Uncomment to better understand errors involving
	% remote_synchronisable_new_link:

	%io:format( "Will create now an initial actor ~w with parameters ~p "
	%		   "on node ~w.~n",
	%		   [ ActorClassName, ActorConstructionParameters, Node ] ),

	ActorAai = ?getAttr(next_actor_identifier),

	ActorSettings = get_actor_settings( ActorAai, State ),

	FullConstructParams = [ Node, ActorSettings | ActorConstructionParameters ],

	% Will trigger back a call to the spawn_successful/2 oneway:
	ActorPid = try

				   apply( ActorClassName, remote_synchronisable_new_link,
						  FullConstructParams )

			   catch

				   error:undef ->

					   handle_undef_creation( ActorClassName, Node,
						   ActorSettings, ActorConstructionParameters, State )

				end,

	display_synthetic_reporting( ActorAai, ActorClassName, Node ),

	NewActorCount = ?getAttr(current_actor_count) + 1,

	% ?debug_fmt(
	%	"Creation of initial actor of class ~s with parameters ~p resulted in "
	%	"the process ~w being spawn on ~w, with AAI ~B.",
	%	[ ActorClassName, ActorConstructionParameters, ActorPid,
	%	Node, ActorAai ] ),

	%io:format( "## Creation of initial actor ~p (~p) on node ~s.~n",
	%		   [ ActorPid, ActorClassName, Node ] ),

	NewClassTable = record_creation_in_class_table( ActorClassName,
									 ?getAttr(instances_per_class) ),

	NewNodeTable = record_creation_in_node_table( Node,
									 ?getAttr(instances_per_node) ),

	%basic_utils:display( "NewClassTable = ~s",
	%					[ table:toString( NewClassTable ) ] ),

	%basic_utils:display( "NewNodeTable = ~s",
	%					[ table:toString( NewNodeTable ) ] ),

	NewSpawnTable = table:addEntry( _K=ActorPid, _V=InitiatorPid,
									?getAttr(spawn_table) ),

	InitiatorTable = ?getAttr(initiator_requests),

	NewInitiatorTable = case table:lookupEntry( InitiatorPid,
												InitiatorTable ) of

		key_not_found ->
			table:addEntry( InitiatorPid, [ ActorPid ], InitiatorTable );

		{ value, InitiatorList } ->
			table:addEntry( InitiatorPid, [ ActorPid | InitiatorList ],
							InitiatorTable )

	end,

	NewState = setAttributes( State, [

				{ spawn_table, NewSpawnTable },
				{ initiator_requests, NewInitiatorTable },
				{ next_actor_identifier, ActorAai+1 },
				{ current_actor_count, NewActorCount },
				{ instances_per_class, NewClassTable },
				{ instances_per_node, NewNodeTable }

											 ] ),
	{ NewState, ActorPid }.



% To be called when needing to create a new runtime actor, on specified node.
%
% Returns an updated state, throws an exception on failure.
%
% To allow for nested creations, the creation is now asynchronous: this call
% will return while the spawned actor is possibly still being constructed; its
% PID is already known, yet, a synchronisable creation being used, this load
% balancer will wait until having received its spawn_successful message to deem
% it constructed and possibly to notify the requester of the corresponding
% creation.
%
% Note: a mere variation of create_initial_actor/5.
%
% (internal helper function)
%
-spec create_runtime_actor( class_name(), [ method_argument() ],
	class_Actor:tag(), net_utils:atom_node_name(), pid(), wooper:state() ) ->
								  wooper:state().
create_runtime_actor( ActorClassName, ActorConstructionParameters, ActorTag,
					  Node, InitiatorPid, State ) ->

	% Checks that the simulation is already started:
	true = class_Actor:is_running( State ),

	ActorAai = ?getAttr(next_actor_identifier),

	ActorSettings = get_actor_settings( ActorAai, State ),

	FullConstructParams = [ Node, ActorSettings | ActorConstructionParameters ],

	%trace_utils:debug_fmt( "Will create now a runtime actor ~w with "
	%   "parameters ~p and tag ~p on node ~w, resulting in following "
	%   "full construction parameters:~n~p.",
	%   [ ActorClassName, ActorConstructionParameters, ActorTag, Node,
	%	  FullConstructParams ] ),


	% Will trigger back a call to the spawn_successful/2 oneway:

	ActorPid = try

				   apply( ActorClassName, remote_synchronisable_new_link,
						  FullConstructParams )

			   catch

				   error:undef ->

					   handle_undef_creation( ActorClassName, Node,
						   ActorSettings, ActorConstructionParameters, State )

				end,

	display_synthetic_reporting( ActorAai, ActorClassName, Node ),

	NewActorCount = ?getAttr(current_actor_count) + 1,

	% ?debug_fmt(
	%	"Creation of runtime actor of class ~s with parameters ~p resulted in "
	%	"the process ~w being spawn on ~w, with AAI ~B.",
	%	[ ActorClassName, ActorConstructionParameters, ActorPid,
	%	Node, ActorAai ] ),

	%trace_utils:debug_fmt( "## Creation of runtime actor ~p (~p) on node ~s.",
	%		   [ ActorPid, ActorClassName, Node ] ),

	NewClassTable = record_creation_in_class_table( ActorClassName,
									 ?getAttr(instances_per_class) ),

	NewNodeTable = record_creation_in_node_table( Node,
									 ?getAttr(instances_per_node) ),

	%basic_utils:display( "NewClassTable = ~s",
	%					[ table:toString( NewClassTable ) ] ),

	%basic_utils:display( "NewNodeTable = ~s",
	%					[ table:toString( NewNodeTable ) ] ),

	NewSpawnTable = table:addEntry( _K=ActorPid, _V=InitiatorPid,
									?getAttr(spawn_table) ),

	InitiatorTable = ?getAttr(initiator_requests),

	ActorEntry = { ActorPid, ActorTag },

	NewInitiatorTable = case table:lookupEntry( InitiatorPid,
												InitiatorTable ) of

		key_not_found ->
			table:addEntry( InitiatorPid, [ ActorEntry ], InitiatorTable );

		{ value, InitiatorList } ->
			table:addEntry( InitiatorPid, [ ActorEntry | InitiatorList ],
							InitiatorTable )

	end,

	CreatedState = setAttributes( State, [
						   { spawn_table, NewSpawnTable },
						   { initiator_requests, NewInitiatorTable },
						   { next_actor_identifier, ActorAai+1 },
						   { current_actor_count, NewActorCount },
						   { instances_per_class, NewClassTable },
						   { instances_per_node, NewNodeTable }
										 ] ),

	% We must block the current diasca until all spawn operations completed, yet
	% serve any intermediary one; returns an updated state:
	%
	wait_for_spawn_ack_from( ActorPid, CreatedState ).



% (helper)
%
handle_undef_creation( ActorClassName, Node, ActorSettings,
					   ActorConstructionParameters, State ) ->

	Arity = length( ActorConstructionParameters ) + 2,

	FunName = remote_synchronisable_new_link,

	FullParams = [ Node, ActorSettings | ActorConstructionParameters ],

	case code_utils:is_beam_in_path( ActorClassName ) of

		not_found ->

			CodePathString = text_utils:strings_to_string(
							   code_utils:get_code_path() ),

			Filename = code_utils:get_beam_filename( ActorClassName ),

			?error_fmt( "Unable to create an instance of '~s', as no '~s' file "
						"can be found in code path, which is, on node '~s':~s~n"
						"Most likely causes: sources may not be recompiled, "
						"directory may not be listed in the BEAM_DIRS make "
						"variable (see then the relevant GNUmakevars.inc) "
						"and/or not deployed (see then the "
						"'additional_elements_to_deploy' field of the "
						"deployment settings record).~n",
						[ ActorClassName, Filename, Node, CodePathString ] ),

			throw( { beam_not_found, Filename } );


		[ SingleBeam ] ->

			FunId = { FunName, Arity },

			case meta_utils:is_function_exported( ActorClassName,
												  FunName, Arity ) of

				true ->
					?error_fmt( "Internal error: module ~s found (as '~s'), "
								"function ~s/~B exported, yet 'undef' raised.",
								[ ActorClassName, SingleBeam, FunName,
								  Arity ] ),

					throw( { undef, { ActorClassName, FunId } } );


				false ->

					case meta_utils:get_arities_for( ActorClassName,
													 FunName ) of

						[] ->
							?error_fmt( "Module '~s' found (as '~s'), yet no "
								"~s function (of any arity) exported.",
								[ ActorClassName, SingleBeam, FunName ] ),
							throw( { function_not_exported_for,
									 { ActorClassName, FunId } } );

						[ OtherArity ] ->
							case Arity > OtherArity of

								true ->
									?error_fmt( "Too many construction "
												"parameters for ~s:~s: "
												"expected ~B, got ~B, "
												"namely:~n~p.",
										[ ActorClassName, FunName, OtherArity,
										  Arity, FullParams ] ),
									throw( { too_many_construction_parameters,
											 ActorClassName,
											 { expected, OtherArity },
											 { got, Arity } } );

								false ->
									?error_fmt( "Too few construction "
												"parameters for ~s:~s: "
												"expected ~B, got ~B, "
												"namely:~n~p.",
										[ ActorClassName, FunName, OtherArity,
										  Arity, FullParams ] ),
									throw( { too_few_construction_parameters,
											 ActorClassName, { got, Arity },
											 { expected, OtherArity } } )

							end;

						OtherArities ->
							?error_fmt( "Incorrect number of construction "
										"parameters for '~s': expected arity "
										"in ~w, got ~B, namely:~n~p.",
										[ ActorClassName, Arity,
										  OtherArities ] ),
							throw( { wrong_construction_parameters_count,
									 { expected_among, OtherArities },
									 { got, Arity } } )

						 end

			end;


		MultipleBeams ->

			CodePathString = text_utils:strings_to_string(
							   code_utils:get_code_path() ),

			?error_fmt( "Unable to create an instance of '~s', and multiple "
						"corresponding beam files were detected:~s. "
						"Corresponding code path is:~s.",
						[ ActorClassName,
						  text_utils:strings_to_string( MultipleBeams ),
						  CodePathString ] ),

			throw( { multiple_beams_detected, MultipleBeams } )


	end.



% Waits until the spawn of specified actor is acknowledged.
%
% (helper)
%
wait_for_spawn_ack_from( ActorPid, State ) ->

	%io:format( "In waiting loop for spawn acknowledgement for ~w.~n",
	%		   [ ActorPid ] ),

	% Now we are waiting for, ultimately, the spawn_successful message for this
	% specific actor (maybe in-between it will induce nested creations that we
	% should of course manage):
	%
	receive

		% Forced match, for the expected actor:
		{ spawn_successful, ActorPid } ->
			% Then we can ultimately unblock this diasca:
			spawn_successful_helper( ActorPid, State )


		% Note: the two next clauses are disabled, as it would be useful only if
		% created actors could themselves create actors from their constructor,
		% which is not the case (they indeed have to be synchronised first)

		% Management of any intermediate creation request; we have to intercept
		% the corresponding actor messages, and only them (placed or not, with
		% or without tag):
		%
		%{ receiveActorMessage, MessageTickOffset, MessageDiasca,
		%  ActorOneway={ OnewayName, _OnewayParams }, SenderPid, SenderAAI }
		%  when OnewayName =:= createRuntimeActor
		%	   orelse OnewayName =:= createRuntimePlacedActor ->
		%	NewState = executeOneway( State, receiveActorMessage,
		%							  [ MessageTickOffset, MessageDiasca,
		%								ActorOneway, SenderPid, SenderAAI ] ),
		%	spawn_successful_helper( ActorPid, NewState );


		% Another actor PID means that ActorPid created at least another actor
		% from its constructor (hence went previously in the previous
		% receiveActorMessage clause); we manage intermediary actors and records
		% that are ready, yet still wait for the root one:
		%
		%{ spawn_successful, OtherActorPid } ->
		%	NewState = spawn_successful_helper( OtherActorPid, State ),
		%	wait_for_spawn_ack_from( ActorPid, NewState )

		% We let all other messages (typically {acknowledged,actor_pid()}) in
		% the mailbox, otherwise of course they would be lost and the load
		% balancer will block the simulation:
		%
		%M ->
		%	io:format( "(load-balancer ignored following message: ~p)~n",
		%			   [ M ] ),
		%	wait_for_spawn_ack_from( ActorPid, State )

	after 5000 ->

			%io:format( "(load-balancer still waiting for spawn "
			%           "acknowledgement of actor ~w)~n", [ ActorPid ] ),

			wait_for_spawn_ack_from( ActorPid, State )

	end.



% Records the creation of an instance of specified class, in specified class
% table.
%
% Returns an updated table.
%
% (helper)
%
record_creation_in_class_table( ActorClassName, ClassTable ) ->

	case table:lookupEntry( ActorClassName, ClassTable ) of

		key_not_found ->
			% New class to register:
			table:addEntry( _K=ActorClassName,
							_V={ _CreationCount=1, _DeletionCount=0 },
							ClassTable );

		{ value, { CreationCount, DeletionCount } } ->
			% Just an update here:
			table:addEntry( _K=ActorClassName,
							_V={ CreationCount+1, DeletionCount }, ClassTable )

	end.



% Records the deletion of an instance of specified class, in specified class
% table.
%
% Returns an updated table.
%
% (helper)
%
record_deletion_in_class_table( ActorClassName, ClassTable ) ->

	%io:format( "ActorClassName = ~s, ClassTable =~n~s~n",
	%		[ ActorClassName, table:toString(ClassTable) ] ),

	% Exists necessarily already:
	{ CreationCount, DeletionCount } = table:getEntry( ActorClassName,
													   ClassTable ),

	% Just an update here:
	table:addEntry( _K=ActorClassName, _V={ CreationCount, DeletionCount+1 },
					ClassTable ).




% Records the creation of an instance on specified node, in specified node
% table.
%
% Returns an updated table.
%
% (helper)
%
record_creation_in_node_table( ActorNode, NodeTable ) ->
	% No node expected to be discovered at runtime:
	table:addToEntry( ActorNode, _Increment=1, NodeTable ).



% Records the deletion of an instance on specified node, in specified node
% table.
%
% Returns an updated table.
%
% (helper)
%
record_deletion_in_node_table( ActorNode, NodeTable ) ->
	table:addToEntry( ActorNode, _Increment=-1, NodeTable ).




% Hooks for serialisation/deserialisation.
%
% Note: the load balancer being an actor, it will be for example serialised with
% no specific request or treatment.



% Triggered just before serialisation.
%
% The state used here is dedicated to serialisation (i.e. it is not the actual,
% resulting state).
%
-spec pre_serialise_hook( wooper:state() ) -> wooper:state().
pre_serialise_hook( State ) ->

	% Some terms are impacted by serialisation:
	setAttribute( State, compute_nodes, ?term_restoration_marker ).



% Triggered just after serialisation, based on the selected entries.
%
% The value returned by this hook will be converted "as is" into a binary, which
% will be written.
%
-spec post_serialise_hook( class_name(),
		 wooper_serialisation:term_serialisation(), wooper:state() ) -> term().
post_serialise_hook( Classname, Entries, _State ) ->
	{ Classname, Entries }.



% Triggered just before deserialisation.
%
-spec pre_deserialise_hook( term(), basic_utils:user_data() ) ->
								  wooper_serialisation: term_serialisation().
pre_deserialise_hook( _SerialisedEntries={ _Classname, Entries }, _UserData ) ->
	Entries.



% Triggered just after deserialisation.
%
-spec post_deserialise_hook( wooper:state() ) -> wooper:state().
post_deserialise_hook( State ) ->

	% We need to update the computing nodes this balancer knows:
	%
	% (note: possibly the user node is now eligible while previously it was not)
	%
	NewComputingNodes = nodes(),

	NodeRecords = [ create_compute_node_record_for( NodeName )
					|| NodeName <- NewComputingNodes ],

	setAttribute( State, compute_nodes, NodeRecords ).



% Returns a computing node record corresponding to specified node.
%
% (helper)
%
create_compute_node_record_for( NodeName ) ->
	#compute_node{ name=NodeName }.



% Traces the state of this load-balancer (for debugging purposes).
%
-spec trace_state( string(), wooper:state() ) -> basic_utils:void().
trace_state( Label, State ) ->

	% For nested bullet lists:
	SubBullet = "    * ",

	SpawnString = text_utils:format( "spawn_table: ~s",
					 [ table:toString( ?getAttr(spawn_table), SubBullet ) ] ),

	InitiatorString = text_utils:format( "initiator_requests: ~s",
					 [ table:toString( ?getAttr(initiator_requests),
									   SubBullet ) ] ),

	Instances = table:enumerate( ?getAttr(instances_per_class) ),

	InstanceString = case [ text_utils:format(
						  "instances for class ~s: ~B created, ~B destructed",
						  [ Class, Created, Destructed ] )
						|| { Class, { Created, Destructed } } <- Instances ] of

						  [] ->
							  "(none)";

						  I ->
							  text_utils:strings_to_string( I, SubBullet )

	end,

	PerClassString = "instances_per_class:" ++ InstanceString,

	PerNodeString = text_utils:format( "instances_per_node: ~s",
					 [ table:toString( ?getAttr(instances_per_node),
									   SubBullet ) ] ),

	InitialActors = ?getAttr(initial_actors),
	InitialActorString = text_utils:format( "~B initial_actors: ~p",
							[ length( InitialActors ), InitialActors ] ),

	SeedString = case ?getAttr(seed_table) of

					 undefined ->
						 "seed_table=undefined";

					 SeedTable ->
						 text_utils:format( "seed_table: ~s",
								[ table:toString( SeedTable, SubBullet ) ] )

	end,

	Strings = [

			   text_utils:format( "next_actor_identifier: ~p",
								  [ ?getAttr(next_actor_identifier) ] ),
			   SpawnString,
			   InitiatorString,
			   text_utils:format( "base_actor_identifier: ~p",
								  [ ?getAttr(base_actor_identifier) ] ),
			   text_utils:format( "current_actor_count (including this "
								  "load balancer): ~p",
								  [ ?getAttr(current_actor_count) ] ),
			   PerClassString,
			   PerNodeString,
			   InitialActorString,
			   SeedString,
			   text_utils:format( "PID: ~w, AAI: ~p",
								  [ self(), ?getAttr(actor_abstract_id) ] )
			  ],

	?debug_fmt( "Load-balancer state ~s: ~s",
				[ Label, text_utils:strings_to_string( Strings ) ] ).



% Displays a trace to allow for the monitoring of the creation of larger actor
% populations.
%
-spec display_synthetic_reporting( class_Actor:aai(), class_name(),
		   net_utils:atom_node_name() ) -> basic_utils:void().





% Displays, in production mode, a notification once 500 new actors have been
% created. Useful for large-scale runs.

-ifdef(exec_target_is_production).


% In (safer, with real-life simulation sizes) production mode here:


display_synthetic_reporting( ActorAai, ActorClassName, Node ) ->

	case ActorAai rem 500 of

		0 ->
			io:format( " + creating actor #~B, of class ~p, on node ~s, "
					   "at ~s~n",
					   [ ActorAai, ActorClassName, Node,
						 time_utils:get_textual_timestamp() ] );

		_ ->
			ok

	end.



-else. % exec_target_is_production



% In development mode here:


display_synthetic_reporting( ActorAai, ActorClassName, Node ) ->

	case ActorAai rem 500 of

		0 ->
			io:format( " + creating actor #~B, of class ~p, on node ~s, "
					   "at ~s~n",
					   [ ActorAai, ActorClassName, Node,
						 time_utils:get_textual_timestamp() ] );

		_ ->
			ok

	end.
	%ok.


-endif. % exec_target_is_production
