%
%
%		Payloads
%	    @@@@@@@@@@@@
%
% Description: Mention the payloads and there body here which will be carried over by the agent.
%

% This list the tasks that the agent can perform based in its payloads
clause(task_info/3).
task_info(guid,[1],[[(ex1_task,1)]]).


% This is the body of the payloads

clause(ex1_task/1).
ex1_task(guid):-
	node_info(Nde,_,_),
	(write(guid),write(`,Executing Task, 1, at node`:Nde))~>R,
	send_log(R),write(R),!.

payload_retract:-
		retractall(ex1_task(guid)),
		!.


