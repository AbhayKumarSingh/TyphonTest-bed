%=======================================================================================
%			Task Manager	
%		    ----------------	
% Author:		Shashi Shekhar Jha {special8688@gmail.com}
% Date:		03-January-2012 	
% Description:	Task Manager is responsilble for book-keeping the pheromone activities,
%			task or problem management at the node and mobile agent's activities. 
%
%=======================================================================================
:-ensure_loaded(system(chimera)).

:- dynamic(task_manager_port/1). % Stores the task_manager's port number.

:- dynamic(task_db/3). % task_db is the database of all the tasks available at the node 
				% it has three parameters: Task_ID, Status (positive: which means 
				% solution is available at the node & negative: which means solution
				% is not available), and A List of the predicates which constitutes
				% the solution.				

:- dynamic(log_server/1). % It keeps the name of the log server for logging the data.

:- dynamic(log_link/1). % It keeps the Link number to the log_server.

:- dynamic(node_task_register/1). % It keeps the list of registered tasks at the node.

:- dynamic(current_task/1). % It keeps the task Id for the current task that the node needs.

:- dynamic(self_heal/2).   % It keeps the Current task ID and Breaking task ID for Self-healing.

:- dynamic(waiting_room/1). % If no neighbour is available at the node, then the agent enters waiting_room.

:- dynamic (neighbors/5). % For the neighbours of the present node
% ========================================================
%  Implementation by Abhay for flooding mechanism for resource discovery


:- dynamic solutionOnCurrentNode/2, problemOnCurrentNode/1, agentPassedWithProblemID/1.

initial_config:-
	retractall(solutionOnCurrentNode(_,_)), assert(solutionOnCurrentNode(none,-1)), retractall(problemOnCurrentNode(_)), assert(problemOnCurrentNode(none)), retractall(agentPassedWithProblemID(_)),assert(agentPassedWithProblemID(( none,none ))),
% =======================================================
nothing.

%% Defines the type of tasks agents are capable of executing.
clause(agent_types/1).
agents_types([[1],[2],[3],[4],[5]]).


%%
%	Start Task Manager Agent
%%

% start_task_manager/1 start an static agent named task manager at the node which manages
% all the task related book-keeping activities.

clause(start_task_manager/1).
start_task_manager(P):-
	  agent_create( taskmanager, manager_handler, P ),
		retractall(task_manager_port(_)),
		assert(task_manager_port(P)),
		nl,write(`=========== TASK MANAGER ==========`),
		nl,write(`Task Manager started at Port`:P),nl, 
		%start_dynamic_network_manager,		%% For Dynamic network function... Comment it if this functionality is not required
	  !.

%-----------------------------Start Task Manager Agent End------------------------------

%%
%	Create Task
%%

% create_task/1 will register a task on the platform and start pheromoning for the same.
% It may also check if the solution is available or not. Whenever a mobile agent will 
% appear will a solution, the same will be resgitered against the task ID and the task 
% status will show that a solution is available.

clause(create_task/1).
create_task(T_ID):-
	task_db(T_ID,Status,Solution)-> write(`Task already exists. Task's Status`:Status),
						  ( Status = positive ->
						  		agent_post(taskmanager,[],execute(Solution));
								nothing
						  );
					 	assert(task_db(T_ID,negative,_)),
	 !.

%-----------------------------Create Task End--------------------------------------------

%%
%	Clear Task
%%

% clear_task/1 retracts all the information associated with the given task_id.

clause(clear_task/1).
clear_task(T_ID):-
	retractall(task_db(T_ID,_,_)),
	!.

%-----------------------------Clear Task End--------------------------------------------

%%
% 	Handler to retrieve the solution brought by a mobile agent
%%

% This handler retrieves the solution informtion from a mobile agent if the same is available

manager_hadler(TMName,TMLink,retrieve(T_ID,Solution)):-
		retractall(task_db(T_ID,_,_)),
		assert(task_db(T_ID,positive,Solution)),
		agent_post(taskmanager,[],execute(Solution)).

% This handler execute the predicates in the solution list one by one. The List of Predicates in the solution 
% is assumed to have either zero (/0) arity or bounded arity.
manager_handler(TMNAME,TMLINK,execute(Solution)):-
		execute_sol(Solution).

manager_handler(TMNAME,TMLINK,exec_sol(Sol)):-
		exec_pred(Sol).
		
% Handler to check the response

manager_handler(Name,Link,check(Val)):-
	write(`~M~J I am Task Manager I got:`),write(Val).

% Handler to make a task undone and change the task_db/3.

manager_handler(Name,Link,undo_task(T_ID)):-
	clear_task(T_ID),
	assert(task_db(T_ID,negative,_)),
	set_current_task(T_ID),
	write(`~M~JAll previous task is undone`),
	write(`~M~JTask `:T_ID),write(` has been generated again!!......`),
	node_info(Myname,_,_),pheromone_clear(_,Myname),pheromone_generate(T_ID,100,100,1),
	(write(`Task regenerated,Node`:Myname),write(`,Task ID`:T_ID))~>Data,
	send_log(Data),write(`~M~JLog sent for task regeneration...`)
	,!.

manager_handler(Name,Link,self_heal(Curr_ID,T_ID)):-
				catch(Err,retract(self_heal(_,_))),
				assert(self_heal(Curr_ID,T_ID)),!.

%Handler to the ACK from the robot or client that the task is done so that the node can start
% asking for the next task 

manager_handler(Name,Link,task_done(T_ID)):-
		retractall(task_db(T_ID,_,Solution)),
		assert(task_db(T_ID,positive,Solution)),
		start_next_task(T_ID),write(`~M~J--Task Execution Completed--`:T_ID),!.

% =======================Abhay defined handlers========================
manager_handler( Name, Link, assertSolution( Type, SolutionData ) ):-
	retractall( solutionOnCurrentNode( _, _ ) ), assert( solutionOnCurrentNode( Type, SolutionData ) ).

manager_handler( Name, Link, assertProblem( Type ) ):-
	retractall( problemOnCurrentNode( _ ) ), assert( problemOnCurrentNode( Type ) ).

% ======================Abhay defined handlers end here================


%% send_update/0 send the updated agent population information in the queue to the Logger.pl
clause(send_update/0).
send_update:-
		intranode_queue(Lii),length(Lii,No_of_agents),
		agents_types(X),
		get_agent_type_list(X,ListofAgentTypes,ListofAgentTypesNames),
		node_info(Ndnm,_,_),
		%agent_create(taskmanager,Link,`172.16.27.130`,48000),
		%agent_post(taskmanager,Link,data(Ndnm,No_of_agents,ListofAgentTypes,ListofAgentTypesNames)),
		%agent_close(taskmanager,Link ),
		%write(`~M~JUpdate Sent`:Link),
		(write(Ndnm),write(`-`),write(No_of_agents),write(`-`),write(ListofAgentTypes),write(`-`),write(ListofAgentTypesNames))~>Data,
		send_logger(Data),
		!.

send_update:- write(`~M~Jsend_update Failed`),!.

send_logger(Data):-
		hide(Sname,0),
		log_server(Server),
		repeat,
		screate(Sname,(Server,48000)),
		ms(repeat,T),T>10,
		%write(`~M~JClient Socket`:Sname),ttyflush,
		sstat(Sname,Status),
		write(`Status`:Status),ttyflush,
		18 is Status,!,
		write(Data)~> Datum,
		name(Data,Data_List),
		string_chars(Datum,Data_List),
		ssend(Sname,Datum),
		%write(Datum),ttyflush,
		repeat,
		%ms(repeat,Tr),Tr>10,
		%write(`~M~J Srecv`),
		srecv(Sname,R),
		%write(`~M~J Received`:R),ttyflush,!,
		sclose(Sname),!,
		%write(`||||Socket Close`),
		%write(`~M~JLog Entry Sent Successfully......`),ttyflush,
		!.

send_logger(Data):-write(`~M~Jsend_logger failed`),!.

%%get_agent_type_list/3 taskes the list of agent types as inputs and outputs the list
%% of number of different agent types in the same order.
clause(get_agent_type_list/3).
get_agent_type_list([],[],[]). 

get_agent_type_list([H|T],Out,Outname):-
		get_agent_type_list(T,Out1,Outname1),
		intranode_queue(Q),
		get_agent_type(H,Q,Agents),
		length(Agents,Name_len),
		append([Name_len],Out1,Out),
		append([Agents],Outname1,Outname),
		!.

%% get_agent_type/2 gives the list of agent of the given type.
clause(get_agent_type/2).
get_agent_type(Type,[],[]).


get_agent_type(Type,[H|T],Agents):-
			task_info(H,Type,_),
			get_agent_type(Type,T,Agents1),
			append([H],Agents1,Agents),!.

get_agent_type(Type,[H|T],Agents):- get_agent_type(Type,T,Agents).

%-----------------------------Task Manager Handler End----------------------------------




%%
% 	Executing a predicate
%%	

clause(execute_sol/1).
execute_sol(Sol):-
	nl,write(`Starting Tasks solution execution`),nl,
		forall(
				member((Pred,Arity),Sol),
				(
				functor(Predicate,Pred,Arity),
				exec_pred(Predicate)	
				)
			),
		write(`~MExecution Completed......~M`).


%This predicate is used to execute the predicate supplied as a parameter to it.

clause(exec_pred/1).
exec_pred(X):- X,nl,write(`Executed`:X).

%--------------------------------- Execute predicate End -------------------------------------------------------------------------------------


%%
%    Neighbour's list
%%

% neighbour_list/1 outputs a list of neighbours of the current node.

clause(neighbour_list/1).
neighbour_list(FLIST):-
	findall(Name,neighbors(Name,_,_ ,_,_),FLIST),
		!.
clause(addlist/3).
addlist(LIST,Name,[Name|LIST]).

clause(nlist/1).
nlist([]).
%--------------------------------- Neighbour List End ---------------------------------------------------------------------------------------

%%
%		Finding List is a SubList or not
%%

% is_sub_list/3 is used to find out whether List1 is a sublist of List2 or not.
% Both List1 and List2 are inputs and if List is a sublist of List 2 the predicate 
% will return a success otherwise it will fail.
clause(is_sub_list/3).

is_sub_list(List1,[],X):-fail.

is_sub_list([],List2,X):- member(X,List2,1),!.

is_sub_list(List1,[X|T],X):- not(member(X,List1)),!.

is_sub_list(List1,[_|T],X):- is_sub_list(List1,T,X).

%--------------------------- is_sub_list End-------------------------------------------------------

%%
%		Finding First appearing node of sublist in MainList
%%

% The find_least_node/3 finds the first appearing node of sublist from the mainList

clause(find_least_node/3).

find_least_node(List1,[],_).

find_least_node(List1,[H|T],X):-member(H,List1,Z),find_least(List1,T,P,Z),member(X,List1,P).

find_least(Il,[H|T],P,Z):- member(H,Il,K),K<Z->find_least(Il,T,P,K);find_least(Il,T,P,Z).

find_least(Il,[],P,Z):- P = Z,!.
						  
%--------------------------- Find_least_node End-------------------------------------------------------

%%
%		Connect to the Log_manager server and send the log entry
%%

clause(connect_log/1).
connect_log(Sname):- hide(Sname,0),
		     	log_server(Server),
				repeat,
		     	catch( Err, (screate(Sname,(Server,49000)),!)),
				Err = 0,
			connect_check(Sname),!.
			
			
connect_log(Sname):- write(`~M~JConnect Log failed. Trying Again...`).

clause(connect_check/1).
connect_check(Sname):-ms(repeat,T),T>10,
			sstat(Sname,Status),
			18 is Status,!.

connect_check(Sname):-write(`~M~JConnect Check Failed in Task Manager`), connect_check(Sname).

clause(disconnect_log/1).
disconnect_log(Sname):-
				ms(repeat,T),T>10,
				srecv(Sname,R),
				%write(`~M~J Received`:R),ttyflush,
				sclose(Sname),
				!.

disconnect_log(Sname):- write(`~M~JDisconnect Log Failed `),disconnect_log(Sname).


clause(send_log/1).
/*send_log(Data):-
		log_server(Server),
		%Random is rand(103),
		%ms(repeat,T),T>Random,
		agent_create(taskmanager,Link,Server,49000),
		name(Data,Data_List),
		string_chars(Datum,Data_List),
		agent_post(taskmanager,Link,data(Datum)),
		!.*/


send_log(Data):- connect_log(Sname),
			name(Data,Data_List),
			string_chars(Datum,Data_List),
			ssend(Sname,Datum),
			disconnect_log(Sname),
			!.
			
send_log(Data):- write(`~M~JSend Log Failed with data`:Data),!.

/*
send_log1(Data):-
		hide(Sname,0),
		log_server(Server),
		repeat,
		screate(Sname,(Server,49000)),
		Random is rand(103),
		ms(repeat,T),T>Random,
		%write(`~M~JClient Socket`:Sname),ttyflush,
		sstat(Sname,Status),
		%write(`Status`:Status),
		18 is Status,!,
		%write(Data)~> Datum,
		name(Data,Data_List),
		string_chars(Datum,Data_List),
		ssend(Sname,Datum),
		repeat,
		srecv(Sname,R),!,
		%write(`~M~J Received`:R),ttyflush,
		sclose(Sname),!,
		%write(`||||Socket Close`),
		%write(`~M~JLog Entry Sent Successfully......`),ttyflush,
		!.
*/

/*
send_log(Data):- nothing,!.
send_log1(Data):- nothing,!.
*/

clause(set_logserver/1).
set_logserver(Server):-
	retractall(log_server(_)),
	assert(log_server(Server)).
log_server(`172.16.117.121`).

% connect_logserver creates a static agent log_client and establishes a link with the log_agent 
% running at the logging server.

clause(connect_logserver/0).
connect_logserver:-
	catch(Err,agent_close(log_client)),
	log_server(S)->
		(
		agent_create(log_client,log_Chandler,P),
		log_server(Server),
		agent_create(log_client,Link,Server,49001),
		assert(log_link(Link))
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.

% start_logserver post to the log_client agent to start the log server. NOTE: log_agent agent at the 	logging
% server should be up for this.

clause(start_logserver/0).
start_logserver:-
	log_server(S)->
		(
			log_link(L)->
				(
					agent_post(log_client,L,start(X))
				);
				write(`Log Server Not connected. Use connect_logserver.`),
				abort
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.
% close_logserver closes the data logging at the logging server.
clause(close_logserver/0).
close_logserver:-
	log_server(S)->
		(
			log_link(L)->
				(
					agent_post(log_client,L,close(X))
				);
				write(`Log Server Not connected. Use connect_logserver.`),
				abort
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.
% reset_logserver restarts the data logging at the logging server.
clause(reset_logserver/0).
reset_logserver:-
	log_server(S)->
		(
			log_link(L)->
				(
					agent_post(log_client,L,reset(X))
				);
				write(`Log Server Not connected. Use connect_logserver.`),
				abort
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.

%-------------------------------------------------- Logging Data End-----------------------------------


clause(manage_queue/4).
manage_queue(InList,Len,Val,OutList):-
	length(InList,Llen),
	Llen < Len -> append(InList,[Val],OutList);
			( 
				member(F,InList,1),
			  	remove(F,InList,RList),
				append(RList,[Val],OutList)
			),!.

cluase(sum_list/2).	
sum_list([],0).
sum_list([H|T],Sum):- sum_list(T,S),Sum is S + H.	

clause(calc_rew/4).	
calc_rew(List,Curr_rs,Max_rs,Out):-
		sum_list(List,Sum),
		length(List,ListLen),
		Term1 is Sum/ListLen,
		%write(`~M~JTerm1`:Term1),
		Term2 is Curr_rs/Max_rs,
		%write(`~M~JTerm2`:Term2),
		Out is Term1*Term2,
		%write(`~M~JOut`:Out),
		!.

clause(get_specific_task/3).
get_specific_task([],In,[]).
get_specific_task([Task_H|Task_R],In,Out):-
			member(In,Task_H,1),
			Out = Task_H,
			!.
get_specific_task([Task_H|Task_R],In,Out):-
			get_specific_task(Task_R,In,Out).

%selected_task_list/3 outputs a list of task Id's from the complete list
% upto the T_ID passed as parameter. hence it cuts the original list in two parts
% based on the passed T_ID and returns the first part.
clause(selected_task_list/3).
selected_task_list([],T,[]).
selected_task_list([H|R],T,[T]):-
			[T1,_,_] = H,
			T1 = T,
			!.
selected_task_list([H|R],T,[T1|O]):-
		[T1,_,_] = H,
		selected_task_list(R,T,O).

%get_next_task/3 gives the next unserviced task from the task list 

clause(get_next_task/3).
get_next_task(CurrP,List,Task):- member(Task,List,CurrP)-> (task_db(Task,negative,_),!);
						Task = 'over',(write(`~M~JAll the task is over!!!!......`),!).
				       
get_next_task(CurrP,List,Task):- P1 is CurrP + 1,!,%write(`~M~JP1`:P1),
						get_next_task(P1,List,Task). %,write(`~M~JTask`:Task:P1).


% start_next_task/3 starts next task in the sequence of Task_list passed to it.
clause(start_next_task/1).

start_next_task(Curr):-				%% This induces self-healing and break-downs particular T_ID
			self_heal(Curr,T_ID),
			agent_post(taskmanager,[],undo_task(T_ID)),!.


start_next_task(Curr):-
		node_task_register(Task_List),
		member(Curr,Task_List,P),
		get_next_task(P,Task_List,Task),
		(
			not (Task = 'over') -> (
					
					set_current_task(Task),
					pheromone_generate(Task,100,100,1),
					node_info(Name,_,_),
					(
						write(`Node state changed,Node name:`),
						write(Name),
						write(`,Completed Task:`),
						write(Curr),
						write(`, Current Task:`),
						write(Task)

					)~>Data,
					write(Data),
					send_log(Data),
					write(`~M~JNode Specific State data sent-------------`)

					);nothing
		),!.

start_next_task(Curr):- write(`~M~JError in start_next_task from task_manager.`),!.

%check_serviceble_task/2 checks for a task ID whose order is less than or equal to the current execution task.

clause(check_serviceble_task/2).
check_serviceble_task(GUID,T_ID):-write(`~M~JTask Check Post 1`),
		current_execution(GUID,T_ID,_,_),!.

check_serviceble_task(GUID,T_ID):-write(`~M~JTask Check Post 2 `),
		task_resource(GUID,TList),
		get_specific_task(TList,T_ID,T_ID_List),%write(T_ID_List),
		[ID,Order,Rs] = T_ID_List,
		current_execution(GUID,T,O,_),!,%write(`Curr O`:O),
		(
			(Order < O)->
					(
						retractall(current_execution(GUID,_,_,_)),
						assert(current_execution(GUID,ID,Order,Rs))
		

					);(	write(`~M~JTask is of higher Order...`),!,fail)
		),write(`~M~JTask Check Post 2 Finish`),
		!.


% register_tasks/1 keeps the sequence of tasks that is required to be executed by the 
% node in the entered order.
clause(register_tasks/1).
register_tasks(List):-
	retractall(node_task_register(_)),
	assert(node_task_register(List)),
	retractall(task_db(_,_,_)),
	forall(
			member(T_ID,List),
			assert(task_db(T_ID,negative,_))

		),!.
clause(clear_register_tasks/1).
clear_register_tasks:-
	catch(Err,retractall(node_task_register(_))),!.

% set_current_task/1 it sets the current task that the node will be requesting for.
clause(set_current_task/1).
set_current_task(T_ID):-
		retractall(current_task(_)),
		assert(current_task(T_ID)),
		node_info(Myname,_,_),
		(
			write(`Node's Task Request,`),
			write(`Node name`:Myname),
			write(`,Task ID,`),write(T_ID)
		)~>Data,
		send_log(Data),
		write(`~M~JLog for setting current task sent -------------`)
		,!.
% remove_list/3 removes the elements in a list from a given list and returns the remaining elements in the list
clause(remove_list/3).
remove_list([],List,List).

remove_list([H|T],InList,OuList):-
				member(H,InList),
				removeall(H,InList,Rest),
				remove_list(T,Rest,OuList),
				!.

remove_list([H|T],InList,OuList):- remove_list(T,InList,OuList),!.



%------------------------------ Task Switching Utilities End-------------------------




