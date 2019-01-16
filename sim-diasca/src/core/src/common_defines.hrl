% Copyright (C) 2012-2017 EDF R&D

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


% This header file lists common defines and typing shorthand.


% Some shorthands and typing:


% The registration name of the simulation case:
%
-define( case_main_process_name, sim_diasca_simulation_case ).


% For actor_pid() and all (avoid too many explicit includes):
-include("class_Actor.hrl").


% PID of an agent of the engine (an engine base object):
-type agent_pid() :: pid().


% PID of the deployment manager:
-type deployment_manager_pid() :: agent_pid().

% PID of a computing host manager:
-type computing_host_manager_pid() :: agent_pid().

% PID of the load balancer:
-type load_balancer_pid() :: agent_pid().


% PID of a data exchanger:
-type data_exchanger_pid() :: agent_pid().


% PID of the result manager:
-type result_manager_pid() :: agent_pid().

% PID of a result producer:
-type result_producer_pid() :: agent_pid().

% PID of a probe:
-type probe_pid() :: class_Probe:probe_pid().

% PID of the datalogger:
-type datalogger_pid() :: agent_pid().


% PID of the plugin manager:
-type plugin_manager_pid() :: agent_pid().


% PID of the performance tracker:
-type performance_tracker_pid() :: agent_pid().

% PID of an instance tracker:
-type instance_tracker_pid() :: agent_pid().


% PID of the resilience manager:
-type resilience_manager_pid() :: agent_pid().

% PID of a resilience agent:
-type resilience_agent_pid() :: agent_pid().
