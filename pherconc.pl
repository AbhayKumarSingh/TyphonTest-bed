%  PherConC Implemetation: Pheromone Part
%  Author: Shashi Shekhar Jha {special8688@gmail.com}
%  Date: 30/12/2011   Version: 1.0

% Implementation of PherConC as repoted by 
% Godfrey and Nair 
% => "A pheromone based mobile agent migration strategy for servicing networked robots."
% => In Proceedings of the 5th International ICST Conference on Bio-Inspired Models of Network, Information, and Computing Systems
% => BIO-NETICS, 2010
%
% phermone has five components: PheromoneID(ProblemID), Source ID, Concentration, Time-out, Neighbour ID.

% Various Parameters associated with Pheromone at a Node=>
% 	Pheromone Concentration at the node
% 	Pheromone Concentartion gradient.
% 	Spanning length of the pheromone, upto how many hops pheromone should diffuse.
% 	Pheromone time-out at the node.
% 	the time-out gradient.
% 	Maximum concentration of the pheromone
%	Maximum time-out value of the pheromone.

%%
% 	Declarations
%%
:- dynamic pheromone/5.			
:- dynamic pheromone_concs/5.

:- dynamic pheromone_timeout/5.
:- dynamic pheromone_spanlen/3.

:- dynamic pheromone_sourcenode/2.
:- dynamic pheromone_nextnode/3.

:-dynamic pheromone_timer_flag/1.
:-dynamic pheromone_timer_interval/1.

pheromone_timer_interval(10000).
pheromone_timer_flag(0).
nothing.
%------------------- Declaration Section End ----------------------------------------%

%%
% asserting retracting of Pheromone Parameters
%%


% The following clause sets the parameter values for a particular phermoone based on its P_ID.

clause(pheromone_setparam/9).
pheromone_setparam(P_ID,Conc,Cmax,DelC,Tout,Tmax,DelT,SpanLen,S_ID):-

		retractall(pheromone_concs(P_ID,S_ID,_,_,_)),
		assert(pheromone_concs(P_ID,S_ID,Conc,Cmax,DelC)),

		retractall(pheromone_timeout(P_ID,S_ID,_,_,_)),
		assert(pheromone_timeout(P_ID,S_ID,Tout,Tmax,DelT)),

		retractall(pheromone_spanlen(P_ID,S_ID,_)),
		assert(pheromone_spanlen(P_ID,S_ID,SpanLen)),
		
		retractall(pheromone_sourcenode(P_ID,S_ID)),
		assert(pheromone_sourcenode(P_ID,S_ID)),
		%write(`~M~J All Pheromone Parameters are set for P_ID`:P_ID-`Source`:S_ID),
		
		!.
pheromone_setparam(P_ID,Conc,Cmax,DelC,Tout,Tmax,DelT,SpanLen,S_ID):-
			write(`~M~JPheromone Set Param Failed for some reason.`).
% The following clause clears a particular phermone based on its P_ID.

clause(pheromone_clear/1).
pheromone_clear(P_ID,S_ID):-

		retractall(pheromone_concs(P_ID,S_ID,_,_,_)),
		retractall(pheromone_timeout(P_ID,S_ID,_,_,_)),
		retractall(pheromone_spanlen(P_ID,S_ID,_)),
		retractall(pheromone_sourcenode(P_ID,S_ID)),
		retractall(pheromone(P_ID,S_ID,_,_,_)),nl,
		%write(`Pheromone P_ID`:P_ID:`Source`:S_ID:`removed from the node`),
		!.
pheromone_clear(P_ID,S_ID):- write(`~M~J Pheromone_clear failed for some reason`).
%--------------------------------- assert/retract pheromone param End--------------------------%


%%
% Equations related with pheomone Management
%%

% The concentration gradient is calculated as delc = 100/d. where d is the spanning length.
% the calc_delc/2 clause calculates the delc based on d and retuns the resutls.

clause(calc_delc/2).
calc_delc(SpanLen,DelC):-
		DelC is (100/SpanLen),!.
calc_delc(SpanLen,DelC):- write(`~M~Jcalc_delc failed `).
% The time-out gradient del_t is cakcukated as delt = Tmax/d, where d is the spanning length.
% The calc_delt/3 clause calculates the del_t based on d and Tmax and returns the result.

clause(calc_delt/3).
calc_delt(SpanLen,Tmax,DelT):-
	DelT is (Tmax/SpanLen),!.

% The pheromone concentation at the nth (n\=1) node is given by Conc(n) = Conc(n-1) - DelC.
% The calc_conc/3 calculates the current concentartion of pheromone using Conc(n-1) and DelC.

clause(calc_conc/3).
calc_conc(Conc_old, Delc, Conc):-
	Conc is (Conc_old - Delc), !.

calc_delt(SpanLen,Tmax,DelT):-write(`~M~Jcalc_delt failed `).

% The pheromone timeout at the nth (n\=1) is given by Tout(n)= Tout(n-1) - DelT.
% After each timestep the value of Tout(n) is decremented by DelT amount.

clause(calc_tout/3).
calc_tout(Tout_old,Delt,Tout):-
	Tout is (Tout_old - Delt), !.

calc_tout(Tout_old,Delt,Tout):-
		write(`~M~Jcalc_tout failed`).
%-------------------------------------------- Equation End ------------------------------------%

%%
%	Pheromone generation
%%

% To generate a pheromone trail from a source node phermone_generate/1 will be used. 
% It generates a pheromone with Cmax concentration and Tmax  timeout value.
% The other input parameters are the pheromone ID and Pheromone Spanning lenght.

clause(pheromone_generate/4).
pheromone_generate(P_ID,Cmax,Tmax,SpanLen):-
	catch(Err,pheromone_timer),
	node_info(S_ID,IP,Port),
	%write(`Generating Pheromones...`),nl,
	calc_delc(SpanLen,DelC),
	calc_delt(SpanLen,Tmax,Delt),
	catch(Er,pheromone_clear(P_ID,S_ID)),
	pheromone_setparam(P_ID,Cmax,Cmax,DelC,Tmax,Tmax,Delt,SpanLen,S_ID),
	pheromone_concs(P_ID,S_ID,Conc,Cmax,DelC),
	pheromone_timeout(P_ID,S_ID,Timeout,Tmax,Delt),
	write(`~M~JPheromone params => P_ID`:P_ID-` SourceNode`:S_ID-`Concentration`:Conc-`Timeout`:Timeout-`Span`:SpanLen),nl,
	assert(pheromone(P_ID,S_ID,Conc,Timeout,S_ID)),

	pheromone_diffuse(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen),   % To stop pheromone diffusion comment this line
	!.

pheromone_generate(P_ID,Cmax,Tmax,SpanLen):- write(`~M~JSource Node pheromone generation failed`).

% To generate a pheromone trail in continuation to a emanating pheromone from a source

clause(pheromone_generate/8).	
pheromone_generate(P_ID, S_ID,Conc_last, Cmax, Tout_last, Tmax, SpanLen,Node_last):-
	write(`~M~JPheromones received from Node`:Node_last),
	catch(Err,pheromone_timer),
	%write(`~M~JReceived pheromone params=> P_ID`:P_ID-` SourceNode`:S_ID-`Concentration`:Conc_last-`Timeout`:Tout_last-`Span`:SpanLen),nl,
	ttyflush,
	calc_delc(SpanLen,DelC),
	calc_delt(SpanLen,Tmax,Delt),
	calc_conc(Conc_last,DelC,Conc),
	calc_tout(Tout_last,Delt,Tout),
	%write(`~M~JReceived Pheromones modified params Conc`:Conc-`Timeout`:Tout),
	(
		(pheromone(P_ID,S_ID,C_old,T_old,N))->
					(
						%write(`~M~JSimilar Pheromone found`:P_ID-S_ID:`Concentration`:C_old),ttyflush,
						(C_old=<Conc,Conc>0)->
									(
										catch(Er,pheromone_clear(P_ID,S_ID)),
										pheromone_setparam(P_ID,Conc,Cmax,DelC,Tout,Tmax,Delt,SpanLen,S_ID),
										assert(pheromone(P_ID,S_ID,Conc,Tout,Node_last)),
										write(`~M~JPheromone inserted at this node ~Mparams=> P_ID`:P_ID-`SourceNode`:S_ID-`Concentration`:Conc-`Timeout`:Tout),nl,
										ttyflush,
										pheromone_diffuse(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen)
									);nothing%write(`~M~J Similar Pheromone of low quality not inserted..`) 
					);
					(
						(Conc>0)->
								(
										catch(Er,pheromone_clear(P_ID,S_ID)),
										pheromone_setparam(P_ID,Conc,Cmax,DelC,Tout,Tmax,Delt,SpanLen,S_ID),
										assert(pheromone(P_ID,S_ID,Conc,Tout,Node_last)),
										write(`~M~JPheromone inserted at this node ~Mparams=> P_ID`:P_ID-`SourceNode`:S_ID-`Concentration`:Conc-`Timeout`:Tout),nl,
										ttyflush,
										pheromone_diffuse(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen)
								 );nothing %write(`~M~JSince Concentration is below Zero so not inserted....`)
					
		
					)
	), 
	!.

pheromone_generate(P_ID, S_ID,Conc_last, Cmax, Tout_last, Tmax, SpanLen,Node_last):-
				write(`~M~J Pheromone generation for diffused pheromones failed for some reason.`).
%------------------------------------------- Pheromone generation End ----------------------------%

%%
%	Pheromone diffusion
%%

% pheromone_diffuse/7 clause diffuses pheromones to all the neighbouring nodes and posts the pheromone_generate/8 to them.

caluse(pheromone_diffuse/7).
pheromone_diffuse(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen):-
	node_info(MName,MIP,MPort),
	pheromone(P_ID,S_ID,Conc,Timeout,Node_last),
	forall(neighbors(Name,IP,Port,X,Y),
				(
					(Node_last = Name)->
						(
							nothing
						);
						(
							connect(IP,Port,Link),
							agent_post(platform,Link,pheromone_diffuse(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen,MName)),
							write(`~M~J Pheros posted on`:IP:Port) %	,disconnect(Link)
							
						)
				)
		),write(`~MPheromones Diffused..`),ttyflush,
		!.

pheromone_diffuse(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen):- write(`~M~JPheromone Diffusion failed`).

%-------------------------------------------- Pheromone diffusion End----------------------------%

%%
%	pheromone timers
%%

% pheromone_timer/1 starts the pheromone_timer_handler/2 as soon as any pheromone arrives at a node, then it keep on running for each pheromone_timer_interval/1.
% pheromone_timer_flag/1 is used to just ensure that pheromone_timer should start when a pheromone arrives at a node.
clause(pheromone_timer/0).
pheromone_timer:- 
	%write(`~M~JStarting Pheromone Timer....`),
	pheromone_timer_flag(0),
	pheromone_timer_interval(X),
	retract(pheromone_timer_flag(0)),
	assert(pheromone_timer_flag(1)),
	agent_create(pheromone,phero_handler,P),
	%write(`~M~J Pheromone Agent Created `),
	timer_create(pheromone_timer,pheromone_timer_handler),
	timer_set(pheromone_timer,X).

pheromone_timer:- nothing,%write(`~M~JPheromone_timer failed As programmed to do so`),
			!.


cluase(phero_handler/3).
phero_handler(PhName,Link,timer_hand(Name,(X,_))):-
	list_pheromones(P_List),
		length(P_List,Len),%write(`~M~JEntering Pheromone Timer Handler. Total number of pheromones at node node is`:Len),ttyflush,
	forall(			
			(
				%write(`~M~JRetriving Pheromones`),
				pheromone(P_ID,S_ID,Conc,Timeout,Node_last)
								
			),
			(
				%write(`~M~JCurrent Processing Pheromone`:P_ID-S_ID-Conc-Timeout-Node_last),
				node_info(MName,MIP,MPort),
				( (S_ID = MName,Conc = 100, Timeout = 100)->(
						(
						write(MName),
						write(`,Pheromone stat,`),
						write(`,Params ID-S-C-T-N,`),write(P_ID),write(`,`),write(S_ID),write(`,`),write(Conc),write(`,`),write(Timeout),write(`,`),write(Node_last)

						)~>Data,
					send_log(Data)%,write(`~M~JLog Sent For Pheromone....`)
					);nothing
				),
				retract(pheromone(P_ID,S_ID,Conc,Timeout,Node_last)),
				pheromone_timeout(P_ID,S_ID,Timeout,Tmax,Delt),
				calc_tout(Timeout,Delt,Tout),
				retractall(pheromone_timeout(P_ID,S_ID,_,_,_)),
				assert(pheromone_timeout(P_ID,S_ID,Tout,Tmax,Delt)),
			%	write(`~M~JProcessed Pheromone ID`:P_ID:`Source`:S_ID:`Timeout values`:Timeout-Tout),nl,ttyflush,
				assert(pheromone(P_ID,S_ID,Conc,Tout,Node_last)),
				(
					Tout=<0->
						(
							pheromone_clear(P_ID,S_ID),
							(task_db(P_ID,negative,_),node_info(S_ID,_,_))->
								(
									%write(`~M~J========================Relaying Pheromones==================`),
									pheromone_generate(P_ID,100,100,2)
								);
								(
									nothing%,write(`~M~JDone Nothing for Relaying Pheromone`:P_ID-S_ID)
						 		)
						);(nothing%,write(`~M~JDone Nothing for Timeout in pheromone_timer_handler`)
						)
				)

			)
		),
	%write(`~M~JResetting Pheromone Timer`),
	reset_pheromone_timer(Name,X).


phero_handler(PhName,Link,timer_hand(Name,(X,_))):- write(`~M~J Pheromone Agent handler Failed.......`).



clause(pheromone_timer_handler/2).
pheromone_timer_handler(Name,(X,_)):-
		agent_post(pheromone,[],timer_hand(Name,(X,_))),!.
		

pheromone_timer_handler(Name,(X,_)):- write(`~M~JTimer Handler Failed for some reason`),
							write(`~M~J Closing pheromone_timer....`),
							node_info(MName,MIP,MPort),
							(
							write(`Pheromone Handler Failed,`),
							write(`Node`:MName)		

							)~>Data,
							send_log(Data),write(`~M~JLog Sent for Timer Failure`),
							pheromone_clear(_,_),
							agent_close(pheromone),
							timer_close(pheromone_timer).
	
clause(reset_pheromone_timer/2).
reset_pheromone_timer(Name,X):-
		pheromone(P_ID,S_ID,Conc,Timeout,Node_last)->timer_set(Name,X);
										(
											%write(`~M~JNo Pheromones Available. Closing Pheromone Timer`),
											agent_close(pheromone),
											timer_close(pheromone_timer),
											retract(pheromone_timer_flag(1)),
											assert(pheromone_timer_flag(0))

										),
		!.

reset_pheromone_timer(Name,X):- write(`~M~JReset Pheromone Timer failed.`).

%-------------------------------------------- Pheromone timers End----------------------------%	

%%
%	Intersection of two lists
%%

clause(intersect/3).
intersect([ ], X, [ ]).

intersect([X|R], Y, [X|Z]) :- member(X, Y), !, intersect(R, Y, Z).

intersect([X|R], Y, Z) :- intersect(R, Y, Z).

%%------------------------- List intersection End-----------------------------


%%
%	List all the pheromones present at the node
%%

% list_pheromones/1 returns the list of all the phoromones P_IDs which are available 
% at the node
clause(list_pheromones/1).
list_pheromones(P_List):-
	findall(P_ID,pheromone(P_ID,S_ID,Conc,Timeout,Next_node),P_List),!.

list_pheromones(P_List):-
			write(`~M~J------List_pheromoes failed--------`).

%----------------------------- List pheromone End---------------------------------

%%
%	Find serviceble pheromones
%%

% find_servicable_pheromones finds all the pheromones at a node that the mobile can service .
% It is taken in care that more than one pheromone of same P_ID can occur at the node and
% agent is assumed to carry multiplt task solutions. The inputs are task ID list and P_ID list.
clause(find_service_pheromones/3).
find_service_pheromones(P_ID,Task_ID,Output):-
	intersect(P_ID,Task_ID,Output),!.

find_service_pheromones(P_ID,Task_ID,Output):- write(`~M~Jfind_service_pheromones failed for some reason`).
%%--------------------------- Find serviceble pheromones End--------------------

%%
%	Find Maximum Concentration pheromone
%%

% max_con_phero/2 finds the maximum concentration pheromones among the given list of pheromones.
% the output will be a list as [Conc,P_ID], where P_ID is pheromone ID and Conc is its concentration.

clause(max_con_phero/2).
max_con_phero(Pheromone_list,Max_pheromone):-
			unique_list(Pheromone_list,Phero_list),
			append_conc_phero(Phero_list,Modif_list),
			sort(Modif_list,Sort_modif),	
			reverse(Sort_modif,F_List),
			member(Max_pheromone,F_List,1),
				!.
max_con_phero(Pheromone_list,Max_pheromone):- write(`~M~J max_con_phero failed for some reason`).

append_conc_phero([],[]).
append_conc_phero([P_ID|Pheros],Return1):-
	findall(Conc,pheromone(P_ID,S_ID,Conc,Timeout,Next_node),Conc_List),
			list_2d(Conc_List,P_ID,Conc_PID_List),
			append_conc_phero(Pheros,Return),
			append(Return,Conc_PID_List,Return1).

% list_2d/3 {list_2d(Input_list,element to append,Output 2d list)}
% converts a 1d list in 2d, with second parameter added to each element of the list 
% provided	in the first parameter.

clause(list_2d/3).
list_2d([],Element,[]).
list_2d([X|R],Element,[[X,Element]|Return]):- not(member(X,R)), list_2d(R,Element,Return).
list_2d([X|R],Element,Return):- list_2d(R,Element,Return).

% unique_list/2 find the list of unique elements from an input list in first parameter.
clause(unique_list/2).
unique_list([],[]).
unique_list([X|R],[X|Z]):- not(member(X,R)),unique_list(R,Z).
unique_list([X|R],Z):- unique_list(R,Z).

%--------------------------- Max Pheromone End---------------------------------------------------

%%
%		Clear Neighbouring Pheromones
%% 

clause(clear_nearby_pheromone/1).
clear_nearby_pheromone(P_ID):-
		node_info(MName,MIP,MPort),
	pheromone_clear(P_ID,MName),
	forall(neighbors(Name,IP,Port,X,Y),
				(
					connect(IP,Port,Link),
					agent_post(platform,Link,pheromone_clear(P_ID,MName)) %	,disconnect(Link)
				)
		),write(`~MPheromones Cleared..P_ID`:P_ID),ttyflush,!.


	


%--------------------------- Clear Neighbouring Pheromones End--------------------------------------


%%
%		Connect to the Log_manager server and send the log entry
%%

clause(pher_send_log/1).
pher_send_log(Data):-
	
		hide(Sname,0),
		pher_log_server(Server),
		screate(Sname,(Server,49001)),
		ms(repeat,T),T>1000,
		%write(`~M~JClient Socket`:Sname),
		repeat,
		sstat(Sname,Status),
		%write(` `:Status),
		18 is Status,
		write(Data)~> Datum,
		ssend(Sname,Datum),
		repeat,
		srecv(Sname,R),
		%write(`~M~J Received`:R),
		sclose(Sname),
		%write(`||||Socket Close`),
		write(`~M~JLog Entry Sent Successfully......`),!.

clause(pher_set_logserver/1).
pher_set_logserver(Server):-
	retractall(pher_log_server(_)),
	assert(pher_log_server(Server)).
pher_log_server(`localhost`).

% connect_logserver creates a static agent log_client and establishes a link with the log_agent 
% running at the logging server.

clause(pher_connect_logserver/0).
pher_connect_logserver:-
	catch(Err,agent_close(pher_log_client)),
	pher_log_server(S)->
		(
		agent_create(pher_log_client,pher_log_Chandler,P),
		pher_log_server(Server),
		agent_create(pher_log_client,Link,Server,49001),
		assert(pher_log_link(Link))
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.
% start_logserver post to the log_client agent to start the log server. NOTE: log_agent agent at the 	logging
% server should be up for this.

clause(pher_strat_logserver/0).
pher_start_logserver:-
	pher_log_server(S)->
		(
			pher_log_link(L)->
				(
					agent_post(pher_log_client,L,start(X))
				);
				write(`Log Server Not connected. Use connect_logserver.`),
				abort
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.
% close_logserver closes the data logging at the logging server.
clause(pher_close_logserver/0).
pher_close_logserver:-
	log_server(S)->
		(
			pher_log_link(L)->
				(
					agent_post(pher_log_client,L,close(X))
				);
				write(`Log Server Not connected. Use connect_logserver.`),
				abort
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.
% reset_logserver restarts the data logging at the logging server.
clause(pher_reset_logserver/0).
pher_reset_logserver:-
	pher_log_server(S)->
		(
			pher_log_link(L)->
				(
					agent_post(pher_log_client,L,reset(X))
				);
				write(`Log Server Not connected. Use connect_logserver.`),
				abort
		);
		write(`No Log Server defined. Use set_logserver(Server). `),
		abort.

%-------------------------------------------------- Logging Data End-----------------------------------











