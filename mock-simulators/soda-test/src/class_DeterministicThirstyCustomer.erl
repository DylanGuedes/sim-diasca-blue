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


% Class modeling a deterministic thirsty customer.
%
-module(class_DeterministicThirstyCustomer).


% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).



% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CustomerName,
		 KnownMachinePid, RepletionDuration, InitialBudget ).



% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/5, new_link/5,
		 synchronous_new/5, synchronous_new_link/5,
		 synchronous_timed_new/5, synchronous_timed_new_link/5,
		 remote_new/6, remote_new_link/6, remote_synchronous_new/6,
		 remote_synchronous_new_link/6, remote_synchronisable_new_link/6,
		 remote_synchronous_timed_new/6, remote_synchronous_timed_new_link/6,
		 construct/6, destruct/1 ).


% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2, setCanCost/3,
		 getCan/2, onNoCanAvailable/2, onNotEnoughMoney/2 ).


% For common types defined in this Soda-Test example:
-include("soda_test_types.hrl").


% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization,
		 "Soda-test.DeterministicThirstyCustomer" ).


% Allows to use macros for trace sending:
-include("sim_diasca_for_models.hrl").



% Class-specific attributes of a deterministic thirsty customer are:
%
% - known_machine_pid :: pid() is the PID of the soda-vending machine this
% customer knows
%
% - can_cost :: amount() is the cost of a can from this machine (as a
% floating-point sum, in euros)
%
% - repletion_duration :: class_TimeManager:tick_offset() is the number of ticks
% after which, once this customer has drunk, he becomes thirsty again
%
% - next_thirsty_tick :: class_TimeManager:tick_offset() is the next tick offset
% at which this customer will be thirsty again
%
% - current_money :: amount() is the (floating-point) number of euros this
% customer has in pocket
%
% - transaction_in_progress :: boolean() tells whether a transaction with a
% machine is in progress




% Creates a new deterministic thirsty customer.
%
% Parameters are:
%
% - ActorSettings corresponds to the engine settings for this actor
%
% - CustomerName is the name of this customer (as a plain string)
%
% - KnownMachinePid is the PID of the soda vending machine this customer may
% know
%
% - RepletionDuration is the duration, expressed in integer virtual minutes, of
% the period during which, once having drunk a can, this customer is not thirsty
% anymore
%
% - InitialBudget is the amount of money this actor has in his pockets initially
%
-spec construct( wooper:state(), class_Actor:actor_settings(),
	   class_Actor:name(), pid(), duration(), amount() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->

	ActorState = class_Actor:construct( State, ActorSettings,
										?trace_categorize( CustomerName ) ),

	?send_info_fmt( ActorState,
		"Creating a new deterministic thirsty customer named '~s', "
		"having initially ~.2f euro(s), knowing the following vending machine: "
		"~w and being thirsty ~B minutes after having drunk.",
		[ CustomerName, InitialBudget, KnownMachinePid, RepletionDuration ] ),

	% From minutes to ticks:
	TickRepletionDuration = class_Actor:convert_seconds_to_ticks(
									 60*RepletionDuration, ActorState ),

	setAttributes( ActorState, [
		{ known_machine_pid, KnownMachinePid },
		{ can_cost, undefined },
		{ repletion_duration, TickRepletionDuration },
		{ next_thirsty_tick, undefined },
		{ current_money, InitialBudget },
		{ transaction_in_progress, false } ] ).



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	?info_fmt( "Deleting thirsty customer named '~s', "
			   "who had finally ~.2f euros left in pocket.",
			   [ ?getAttr(name), ?getAttr(current_money) ] ),

	% Then allow chaining:
	State.





% Methods section.


% Management section of the actor.


% The core of the customer behaviour.
%
% (oneway)
%
-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	NewState = case ?getAttr(can_cost) of

		undefined ->
			request_cost( State );

		requested ->
			?info( "Price quote from the machine being requested." ),

			% We already know we will be scheduled at next diasca, knowing then
			% the cost of a can; nothing to do here, just wait:
			State;

		_ ->
			manage_thirst( State )

	end,

	?wooper_return_state_only( NewState ).



% Simply schedules this just created actor at the next tick (diasca 0).
%
% (actor oneway)
%
-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->

	ScheduledState = executeOneway( State, scheduleNextSpontaneousTick ),

	?wooper_return_state_only( ScheduledState ).



% Called by the known machine, in return to a getCanCost call.
%
% (actor oneway)
%
-spec setCanCost( wooper:state(), amount(), class_Actor:actor_pid() ) ->
						class_Actor:actor_oneway_return().
setCanCost( State, CanCost, MachinePid ) ->

	?debug_fmt( "The vending machine answered that a can costs ~w euros.",
				[ CanCost ] ),

	% Sanity checks:
	MachinePid = ?getAttr(known_machine_pid),
	requested = ?getAttr(can_cost),

	% Now, we enter the thirsty/not thirsty loop, starting as if not thirsty at
	% all:
	ThirstyState = set_next_thirsty_tick( State ),

	?wooper_return_state_only( setAttribute( ThirstyState, can_cost,
											 CanCost ) ).



% Called by the machine in return to a orderSoda call, when a can was available.
%
% (actor oneway)
%
-spec getCan( wooper:state(), class_Actor:actor_pid() ) ->
					class_Actor:actor_oneway_return().
getCan( State, _MachinePid ) ->

	% To test the proper simulation teardown should an actor fail:
	%basic_utils:crash(),

	% To test how a method taking a longer duration is reported:
	%timer:sleep( 5 * 60 * 1000 ),
	%basic_utils:freeze(),

	?info( "Received a can, drank it, no more thirsty for a while." ),

	MoneyState = subtractFromAttribute( State, current_money,
										?getAttr(can_cost) ),

	FinishState = setAttribute( MoneyState, transaction_in_progress, false ),

	?wooper_return_state_only( set_next_thirsty_tick(FinishState) ).



% Called whenever a can was requested to a machine whereas none is available.
%
% (actor oneway)
%
-spec onNoCanAvailable( wooper:state(), class_Actor:actor_pid() ) ->
							  class_Actor:actor_oneway_return().
onNoCanAvailable( State, _MachinePid ) ->

	?info( "Could not have soda, the machine had no can left." ),

	?wooper_return_state_only( setAttribute( State,
											 transaction_in_progress, false ) ).



% Called whenever the customer requested a new can but actually cannot afford it
% (this should never happen).
%
% (actor oneway)
%
-spec onNotEnoughMoney( wooper:state(), class_Actor:actor_pid() ) ->
							  class_Actor:actor_oneway_return().
onNotEnoughMoney( State, MachinePid ) ->

	?error_fmt( "Still having ~.2f euros but unable to buy a can from ~w that"
				"should cost ~.2f euros, this soda vending machine is a crook.",
				[ ?getAttr(current_money), MachinePid, ?getAttr(can_cost) ] ),

	?wooper_return_state_only(
	   setAttribute( State, transaction_in_progress, false ) ).



% Helper functions.


% Requests the known machine to return the cost of one of its cans.
%
% Triggers back a setCanCost/3 call.
%
% Returns an updated state.
%
% (helper)
%
-spec request_cost( wooper:state() ) -> wooper:state().
request_cost( State ) ->

	?info( "Investigating how much costs a soda, requesting the machine." ),

	class_Actor:send_actor_message( ?getAttr(known_machine_pid), getCanCost,
							 setAttribute( State, can_cost, requested ) ).





% Determines the behaviour of the customer with regard to cans.
%
% Returns an updated state.
%
% (helper)
%
-spec manage_thirst( wooper:state() ) -> wooper:state().
manage_thirst( State ) ->

	case is_thirsty( State ) of

		true ->
			case ?getAttr(transaction_in_progress) of

				true ->
					% Do nothing until it is over, just wait:
					?debug( "Cost transaction in progress, waiting..."),
					State;

				false ->
					% Here we need to drink, let's try to do so by ordering a
					% soda: (we specify our budget, but the machine has the
					% final word)
					Budget = ?getAttr(current_money),
					CanCost = ?getAttr(can_cost),
					case CanCost of

						Cost when Cost > Budget ->

							?info_fmt( "Thirsty, but not having enough money: "
								"a can costs ~.2f euros, whereas having only "
								"~.2f euro(s).", [ CanCost, Budget ] ),

							% No scheduling planned here, implies being passive
							% from now on:
							%
							State;

						_ ->

							% We should be able to afford the can:

							?info_fmt(
								"Thirsty and having enough money (~.2f euros), "
								"trying to buy a can.", [ Budget ] ),

							class_Actor:send_actor_message(
							  ?getAttr(known_machine_pid),
							  { orderSoda, Budget },
							  setAttribute( State, transaction_in_progress,
											true ) )

					end

			end;

		false ->
			%?info( "Feeling fine, not thirsty currently." ),
			% We could jump directly to the moment this customer will be thirsty
			% again:
			executeOneway( State, scheduleNextSpontaneousTick )

	end.



% Returns whether this customer is thirsty.
%
% (helper)
%
-spec is_thirsty( wooper:state() ) -> boolean().
is_thirsty( State ) ->

	CurrentTick = class_Actor:get_current_tick( State ),

	case ?getAttr(next_thirsty_tick) of

		ThirstTick when CurrentTick >= ThirstTick ->
			true;

		_ ->
			false

	end.



% Computes the next thirsty tick and records it.
%
% Returns an updated state.
%
% (helper)
%
-spec set_next_thirsty_tick( wooper:state() ) -> wooper:state().
set_next_thirsty_tick( State ) ->

	TickDuration = ?getAttr(repletion_duration),

	NextThirstyTick = class_Actor:get_current_tick( State ) + TickDuration,

	ThirstyState = setAttribute( State, next_thirsty_tick, NextThirstyTick ),

	?trace_fmt( "Determined that will be thristy again in ~B ticks.",
				[ TickDuration ] ),

	executeOneway( ThirstyState, scheduleNextSpontaneousTick ).
