%================================================================================================================================
% Author :: Jatin Matani Btech CSE % EMAIL  :: jforjatin@gmail.com
% This is platform v2.0 ... completly restructured for a more 'object' based approach over a code
% based approach of the previous version of platform.

%=================================================================================================================================

:- ensure_loaded( system(chimera) ). /* Load chimera module */

% setup/0 sets up the dynamic clauses used later on 
% 1. database, execs are from old version of platform. Do not pay much attention to them
% 2. platform_port holds the port on which the platform is running. Form : typhlet_port(port)
% 3. typhlet_GUID holds the identifier, handler predicate and port information about all the typhlets running on the current platform. Form : typhlet_GUID(id,handler_predicate,Port)
% 4. typhlet_payload holds the name of the predicates added to the typhlet as payload. Form : typhlet_payload(id, [(predicate1,arity1),(predicate2,arity2),...])
% 5. outgoing_typhlet holds the information about the outgoing typhlet (moving outside of the platform). The information is identifier
%    and the timestamp. This is later used for calculating hop times for outgoing typhlets. Form : outgoing_typhlet(id, Top, Res). Take a look at time/2 in WIN_REF.pdf
% 6. hoptime_stat holds the hop time information. Holds the statistics. 
% 7. typhlet_token holds the list of token a typhlet has. Form : typhlet_token(id,[token list])
% 8. transit_GUID, transit_payload, transit_token and transit_code are same as typhlet_GUID,typhlet_payload .. respectively.
%    When typhlets are moving, then transit predicates are used to hold information till we are sure that the typhlet has reached its destination.

setup:-
 (dynamic (database/1)),                 
 (dynamic (platform_port/1)),
 (dynamic (execs/1)),
 (dynamic (typhlet_GUID/3)),  
 (dynamic (typhlet_payload/2)),
 (dynamic (outgoing_typhlet/3)),
 (dynamic (hoptime_stat/4)),
 (dynamic (typhlet_token/2)),			 
 (dynamic (platform_token/1)),
 (dynamic (transit_GUID/3)),  
 (dynamic (transit_payload/2)),
 (dynamic (transit_token/2)),			 
 (dynamic (transit_code/2)),	
 (dynamic (transfer_queue/1)),
 (dynamic (transit_agent/2)),
 (dynamic (code_base/2)).


% This is the primilary stuff. The same functionality could have been achieved using some other means. Following code has been picked up
% from previous version of the platform. Can be ignored. In a nutshell, it asserts the predicate execs/1 ..
% EXAMPLE :execs(my_predicate/2) is equivalent to  writing dynamic my_predicate/2

do:-assert(database(assert((execs(X):-dynamic X)))).
start_db:- setup,do,database(C),C.			



%%INTERFACE FUNCTION%% : means that this is visible to user
% platform_start/1 starts the typhlet platform at a port which can be specified by the user. It creates a platform agent, writes some output for 
% the user. It also hides some of the code, so that only the interface predicates are visible to the user and not the entire code. This is achieved by
% hide_mycode/0. Hoptime statistics are set to 0, and port number is also stored.
platform_start(Port) :-							
   start_db,
   agent_create( platform, handler, Port ),nl,
   assert(platform_port(Port)),
   output(OriginalStream),output(0),
   write('===================='),
   write('===================='),nl,
   write('Welcome to Typhon - Mobile Agent Platform'),nl,
   write('===================='),
   write('===================='),
   nl,nl,
   write('This platform is running on '),
   write(Port),nl,
   nl,
   output(OriginalStream),
   assert(hoptime_stat(0,0,0,0)),
   assert(transfer_queue([])),
   hide_mycode.
   
   

%%INTERFACE FUNCTION%%
% platform_close/0 closes the platform agent, retracts the port information, kills all the typhlet that are running and retracts all the code
% previously loaded by the platform using platform_assert_file. That code is stored in code_base.
platform_close :-								
   agent_close(platform),			
   retract(platform_port(_)),		
   forall(typhlet_GUID(GUID,_,_),typhlet_kill(GUID)),
   forall(code_base(FileName,FileCodeList),retract_code(FileCodeList)),  
   retractall(code_base(_,_)),							 
   output(OriginalStream),output(0),
   write('Platform has been closed'),nl,  
   output(OriginalStream).
   

   
%%INTERFACE FUNCTION%%
% platform_reset/0 closes and starts the platform at the same port
platform_reset:- 								 
	platform_port(Port),platform_close, platform_start(Port).		 



%%INTERFACE FUNCTION%%
% set_token/1 sets the token of the current platform. Incoming typhlet having this token is allowed entry, others are denied entry. This 
% implements a security feature of the platform. However this is not complete and is just a prototype. *Need to finish security module*
set_token(Token):- retract(platform_token(_)),assert(platform_token(Token)).		 



%%INTERFACE FUNCTION%%
% connect/3 creates a link between two platforms. The other platform is specified by IP and Port. Results in Link which is used later on 
% for platform-to-platform communication. Just uses the agent_create/4 from Chimera. <SIMPLE>
connect(IP,Port,Link):-					 
   repeat,
   catch( Err, (agent_create(platform,Link,IP,Port),!)),
   Err = 0.


% disconnect/1 breaks the above connection. Usual heuristic dictates that connections should be terminated. The issue however is who calls it?
% The connect usually is followed by migrate calls. Mobile agent calls connect, and then move. So as soon as move is called, the code is inturrupted
% and move is done. But if a disconnect is written after the move (obviously) , it is not queried (code has already retracted and moved to new location)
% so disconnect would never be called. < NOT IN USER MANUAL > 
disconnect(Link):-
   catch(A,agent_close(platform,Link),B). 





%%INTERFACE FUNCTION %%
% platform_assert_file loads the content of the file into Prolog. Using consult etc are the same thing, but we need to keep track of 
% the code that has been asserted. Hence this function has been made. It uses file_to_list/2, assert_code/1 which are predicates defined 
% below in utilities. Its asserts a mapping of the code vs FILE to keep track of this code. This is used later for saving platform state.


platform_assert_file(FILE):-
file_to_list(FILE,L),
assert_code(L),
assert(code_base(FILE,L)).  %%  FILE mapped LIST .. required only once .. when loading the code into the platform





%%INTERFACE FUNCTION%%
% save_platform_state/1 saves all the code that has been loaded previously using platform_assert_file and 
% the typhlet code(its handler code and payload code) into a file. Its a kind of local backup. Can be loaded back into the system
% The interface function loads up an event for the platform. The platform handler then handles the event. For the sake of readability, I have written
% the handler code here instead of writing it later. {PS : the position of the code does not matter.}
% Why event based? Because when you are saving a state, you open a file and push stuff into it. The problem occurs when other event occurs and interrupt. 
% These events may issue 'write' calls, which would go into our open stream. But having the entire process in a event handling (which is not interruptable)
% gives you a sense of 'mutual exclusion'.



% Intially it looks up at code_base to find out all the generic code, saves it to the file. Then for all the typhlets, captures the code
% using assimilate_code and saves it. At the last, it stores the meta info such as what typhlet is linked to what code etc which it uses
% later on to resume the state. Ex : typhlet_payload contains the names of all predicates linked to each typhlet. This needs to be fed back
% when we resume from the 'saved state'.


save_platform_state(FileName):- 
	(atom(FileName)->nothing;abort),
	agent_post(platform,[],save_state(FileName)).

handler(platform,Link,save_state(FileName)):-                         	
	tell(FileName),							 
	forall(code_base(_,FileCodeList),								%save all code contained in files(that was loaded using platform_assert_file)
			(
				forall(member(FileCode,FileCodeList),(portray_clause(FileCode)~>F,write(F)))
			)
		),
	forall(								 
			typhlet_GUID(GUID,Handler,Port),		 				%save typhlet code
			(
				assimilate_code(GUID,Result),
				forall(
						(
							member(Code,Result)
						),
						(	
							portray_clause(Code)~>Y,write(Y)
						)
					)
				
			)
		),
	
	(
	 	clauses(typhlet_payload(Gg,Ll),PayloadMeta)->						%save payload list (meta information)
		forall(member(M,PayloadMeta),(portray_clause(M)~>Ss,write(Ss)));nothing
	),
	
	(
		forall(typhlet_GUID(G1,H1,P1),(portray_clause((start_agent(G1,H1,P1)))~>S1,write(S1))), %apparently, hiding the typhlet_GUID is causing trouble
		clauses(typhlet_GUID(G,H,P),List)->								%save typhlet's port and handler info (meta information)
		forall(member(L,List),(portray_clause(L)~>S,write(S)));nothing
	),
	(
		clauses(code_base(FName,FCode),FCodeMeta),
		forall(member(FM,FCodeMeta),(portray_clause(FM)~>Ff,write(Ff)))			%save which file contained what code (meta information)
	),
	told.




%%INTERFACE FUNCTION%%
% load_platform_state/1 loads the state back. It loads the predicates, loads the typhlet code(their handler and payload) and starts the typhlets.
% Basically, read the file, assert everything, and using the meta information, start the typhlets at their respective ports. Load the meta information
% as well

load_platform_state(FileName):-   					
	(atom(FileName)->nothing;abort),
	agent_post(platform,[],load_state(FileName)).

handler(platform,Link,load_state(FileName)):-
	file_to_list(FileName,L),
	forall(
			member(Code,L), 	%assert code,	%start agents,	%restore meta information (agent, payload , code_base).	
			(
				%Code = clause(Predicate)-> execs(Predicate); (Code = (typhlet_GUID(G,H,P))-> (agent_create(G,H,X),assert(typhlet_GUID(G,H,X))); assert(Code)) 
				Code = clause(Predicate)-> execs(Predicate); (Code = (start_agent(G,H,P))-> (agent_create(G,H,X),assert(typhlet_GUID(G,H,X))); assert(Code)) 
			)
		).	
	




	%=====================================================typhlet predicates================================================================

%%INTERFACE FUNCTION%
% typhlet_create/3 creates a typhlet. It uses agent_create/3 of Chimera to create an agent. A unique 8character GUID is generated using gensym
% and handler code for the typhlet is generated and asserted. Assume that abc123 is the name of the agent, then agent_create(abc123,user_handler,Port) is
% invoked. 
% The handler code provided by user is ' user_handler(guid,Link,Event):- .. '. Instead a handler code of the form ' user_handler(abc123,Link,Event):- ..'
% is asserted where abc123 is the actual name of the agent. So when the Event is posted on the agent, then since 
% its name is abc123, only that agent carries the task and no other. Refer to 'how it works.pdf'

typhlet_create(GUID,Handler,Port):-   				
	((var(GUID),atom(Handler))->nothing;abort),	
	gensym(GUID),					% use predefined hide predicate to create unique Atom. gensym checks for var nature of argument 
	genhandler(GUID,Handler,Result),		% the Result is obtained in form of code which needs to be asserted
      assert_code(Result),				% Result asserted : Result contains new unique handler. handler(actual identifier,_,_):- blah blah..
	assert(typhlet_GUID(GUID,Handler,Port)),	% a mapping in the agent db ... lets see how can this be used !
	assert(typhlet_payload(GUID,[])),		% intialisation of payload list. Intially no payloads attached. Therefore empty list
	assert(typhlet_token(GUID,[])),		% The tokens contained by the typhlet. Intially it has none.
	agent_create(GUID,Handler,Port),		% So, now create an agent with the name GUID found earlier and handler.
	!.  			



%%INTERFACE FUNCTION %%
% Interface function for agent_create/4 of Chimera
typhlet_create(Name,Link,Ip,Port):-			
	agent_create(Name,Link,Ip,Port).





%%INTERFACE FUNCTION%%
% Interface function for agent_post/3 of Chimera
typhlet_post(Name,Link,Event):-			
	agent_post(Name,Link,Event).



%%INTERFACE FUNCTION%%
% Typhlet_kill closes the typhlet using agent_close/3 of Chimera, retracts all the relevent code and deletes the meta
% informations. assimilate_code/2 is used for gathering all the code, retract_code/1 retracts it. Other calls are mainly
% to remove typhlet meta info about payloads and tokens.
typhlet_kill(GUID):-							
	(atom(GUID)->nothing;abort),			 
	assimilate_code(GUID,AgentCode),
	agent_close(GUID),
	retract(typhlet_GUID(GUID,Handler,Port)),	
	retract(typhlet_payload(GUID,PayloadList)),
	retract(typhlet_token(GUID,ListOfTokens)),	 
	retract_code(AgentCode),!.                                





%%INTERFACE FUNCTION%%
% typhlet_save/2 writes the typhlet information into a file. This is for possible usage later on. 
% assmilate_code/2 for capturing the code, and then writing the code into the file<SIMPLE>.

typhlet_save(GUID,FileName):-						
	((atom(GUID),atom(FileName))->nothing;abort),		 
	assimilate_code(GUID,Result),
	tell(FileName),
	forall(
			(
			member(Code,Result)
			),
			(
			portray_clause(Code)~>Y,write(Y)
			)
		),
%	agent_post(GUID,[],save(GUID,FileName)),     %//for event generation
	told.

%========================================================payloads=======================================================================

%%INTERFACE FUNCTION%%
% add_payload /2 adds the payload to the typhlet. To achieve this, it creates a unique copy of the payload. The payload predicate in its 
% parameter contains a term 'guid'[user_code]. Example : ability(guid,X):-print(X). So we replace the 'guid' with the actual identifier/name of the 
% agent. Now comes the question why?
% Note that when I made the handler of the typhlet, i subsitituted all 'guid's by actual identifiers. So in the handler of a typhlet, if 
% user writes something like this :
% myhandler(guid, Link, message(X)):-ability(guid,X).
% Then instead of this handler, something like this is created and asserted [Assume abc123 is the identifier alloted by the system]
% myhandler(abc123, Link, message(X)):- ability(abc123,X).
% So when this typhlet/agent is notified of the Event 'message(X)'; then myhandler with abc123 as its first parameter is invoked. (DUE TO CHIMERA)
% and so when adding ability as payload I create ability(abc123,X):- print(X). as the payload, and assert it. 
% THUS we have a unique handler code, and unique payload code, their uniqueness defined by identifier.

add_payload(GUID,AddList):-						
	((atom(GUID),list(AddList))->nothing;abort),		
	typhlet_payload(GUID,List),
	generatePayloadCode(GUID,AddList,PayloadCode),  	 %capture the generic defination as reported by user in codefile, then 
	assert_code(PayloadCode),					 %do text manipulation to generate unique code for agent. Assert it
	append(AddList,List,NewList),
	retract(typhlet_payload(GUID,List)),
	assert(typhlet_payload(GUID,NewList)),
	agent_post(GUID,[],add_payload(GUID,AddList)).		 %//for Event generation. 



%%INTERFACE FUNCTION%%
% remove_payload/2 captures the payload code that is specific to the typhlet. (having the identifier in defination instead of 'guid'). 
% After capturing the code,  it retracts it. The predicate_header here creates payload_predicate(typhlet_name,_,_...) sort of header which 
% later is later used by clauses/2 to fetch the body of the payload. The permanent list of payload for that typhlet is edited finally.

remove_payload(GUID,ToRemove):-					
	((atom(GUID),list(ToRemove))->nothing;abort),		%//
	typhlet_payload(GUID,List),
	forall( 	(
			 member((Predicate,Arity),ToRemove)
			),
			(	
			  predicate_header(GUID,Predicate,Arity,Result),
			  (clauses(Result,Body)->retract_code(Body);nothing)
			)
		),
	delete_list(ToRemove,List,NewList),  
	retract(typhlet_payload(GUID,List)),
	assert(typhlet_payload(GUID,NewList)),!.




%%INTERFACE FUNCTION%%
% shed_payload/2 basically removes the payload from the agent and puts it in the platform. What ? Let's consider an example
% ability(abc123,X):-write(abc123),write(X). The <- is payload attached to a typhlet named abc123.
% If u shed, ability; the above code is retracted. And ability(guid,X):-write(guid),write(X) is asserted back.
% This would be the original way the user might have written the ability predicate in his/her file. 
% ability is now again available to be attached to some other typhlet. 

shed_payload(GUID,ToShed):-						
	%shedding means retracting the agent specific code, and asserting the generic defination
	((atom(GUID),list(ToShed))->nothing;abort),		%//
	copy_payload(GUID,ToShed),		%thus storing it at the platform {CUT PASTE}
	remove_payload(GUID,ToShed).		% DOES THE payload already exists??? !!!!!!!!!!! this defination would be added NOT OVERWRITTEN
	



%%INTERFACE FUNCTION%%
% copy_payload copies the payload. Does not retract it. So the tyhphlet retains its payload while platform also gains the generic defination
% of the payload. The rest of the predicates are pretty much similar and self explanatory


copy_payload(GUID,ToCopy):-						
	((atom(GUID),list(ToCopy))->nothing;abort),		%//
	forall( 	(
			member((Predicate,Arity),ToCopy)
			),
			(	
			generate_generic_predicate(GUID,Predicate,Arity,Result),       %make generic defination%
			assert_code(Result)
			)
		),!.
%%INTERFACE FUNCTION%%
removeall_payload(GUID):-						
	(atom(GUID)->nothing;abort),		 
	typhlet_payload(GUID,List),
	remove_payload(GUID,List).


%%INTERFACE FUNCTION%%
shedall_payload(GUID):-                  				
	(atom(GUID)->nothing;abort),					 
	typhlet_payload(GUID,List),
	shed_payload(GUID,List).			% !!!!!!!!!!!!!!!!!!!! CHECK if defination already exists ?

%%INTERFACE FUNCTION%%
copyall_payload(GUID):-							
	(atom(GUID)->nothing;abort),					
	typhlet_payload(GUID,List),
	copy_payload(GUID,List).





%%=========================================Token/Security====================================================%%

%%INTERFACE FUNCTION%%
% add_token adds the a token ( presented as a list [token1,token2] ) with the typhlet. while remove_token removes
% token from the list. <SIMPLE>
add_token(GUID,List):-							
	(list(List)->nothing;abort),            					%//
	typhlet_token(GUID,OldList),				%// need to put error chceks here
	append(OldList,List,NewList),				%//
	retract(typhlet_token(GUID,OldList)),			%//
	assert(typhlet_token(GUID,NewList)).			%//

remove_token(GUID,ToRemove):-						%INTERFACE FUNCTION
	(list(List)->nothing;abort),					%//
	typhlet_token(GUID,List),					%//
	delete_list(ToRemove,List,NewList),			%//
	retract(typhlet_token(GUID,List)),			%//
	assert(typhlet_token(GUID,NewList)).			%//





%%========================================MOBILITY=============================================================%%

%the move is send/ack process



%%INTERFACE FUNCTION%%
% cloning means creating a copy of the typhlet at a 'remote' location. Of course, that's the usual way of doing things. 
% {{Future}} feature note for developer : make a clone_typhlet/1 ..for making a clone and starting it here and not somewhere else.
% Would that be a better feature than this ? .. But again the same can be achieved by doing clone_typhlet(xyz,[]).. try this my man !

% clone _typhlet creates a new identifier first for the copy of the typhlet. Then all the code of typhlet is assimilated and all the places,
% where ID1 is written, it is replaced by ID2. Then you send the code to the target destination. The code includes both, Handler code, 
% and payload code. To calculate time taken for the HOP, a timestamp is saved by time(Top,Res) and it is saved with outgoing_typhlet/3.
% Later on when the typhclone reaches the destination, an ack is sent which is used to calculate the time taken for hop.[more on hop time later]

clone_typhlet(GUID1,Link):-					
	((atom(GUID1),integer(Link))->nothing;abort),	
	time(Top,Res),
	gensym(GUID2),
	assert(outgoing_typhlet(GUID2,Top,Res)),
	assimilate_code(GUID1,AgentCode),
	replaceGUID(AgentCode,GUID1,GUID2,CloneCode), 
	typhlet_payload(GUID1,PayloadList),
	typhlet_GUID(GUID1,Handler,_),
	typhlet_token(GUID1,ListOfTokens),	
	time(1,TT),time(TT,Date,Time),
	agent_post(platform,Link,send(GUID2,Handler,PayloadList,CloneCode,ListOfTokens,Time)),!.			

%% Added by shashi to perform local cloning at the node
clone_typhlet_local(GUID1,GUID2):-
	((atom(GUID1),var(GUID2))->nothing;abort),	
	gensym(GUID2),
	assimilate_code(GUID1,AgentCode),
	replaceGUID(AgentCode,GUID1,GUID2,CloneCode), 
	typhlet_payload(GUID1,PayloadList),
	typhlet_GUID(GUID1,Handler,_),
	typhlet_token(GUID1,ListOfTokens),	
	%% creating clone typhlet
	assert(typhlet_payload(GUID2,PayloadList)),   			% intialization of payload list
	assert(typhlet_token(GUID2,ListOfTokens)),				%//
      assert_code(CloneCode),
	agent_create(GUID2,Handler,Port),	
	time(1,TT),time(TT,Date,Time),						%//
	output(OriginalStream),output(0),
	nl,write('Cloned Typhlet $'),
	write(GUID2),
	write('$ has been created at this port : '),			%//
	write(Port),write(' at -'),write(Time),nl,		
	output(OriginalStream),
	assert(typhlet_GUID(GUID2,Handler,Port)),
	!.	


%%INTERFACE FUNCTION%%
% move_typhlet is just assmilating the code, and sending it to the destination. The code is retracted after sending it. 
% The code however is not discarded right away. It is stored as a temporary. This is because if a negative ack comes from the recieving platform
% the code, would be activated once again, and the typhlet would be started again.
move_typhlet(GUID,Link):-									% This predicate is modified by Shashi on 20-feb-2012
	((atom(GUID),integer(Link))->nothing;abort),		
	assimilate_code(GUID,AgentCode),
	typhlet_payload(GUID,PayloadList),
	typhlet_GUID(GUID,Handler,Port),
	typhlet_token(GUID,ListOfTokens),time(1,TT),time(TT,Date,Time),				
	agent_post(platform,Link,send(GUID,Handler,PayloadList,AgentCode,ListOfTokens,Time)),			
	%write(`~M~JAgent is posted successfully over the Link`:Link),			% Added by shashi on 17-feb-2012 for tracking agent migration
	time(Top,Res),
	assertz(outgoing_typhlet(GUID,Top,Res)),
	transfer_enqueue(GUID),
	%agent_close(GUID),
	agent_post(platform,[],clean_agent(GUID)).
	

move_typhlet(GUID,Link):- write(`~M~JMove typhlet Failed`).


%%% Added by shashi to enhance move_typhlet on 13 August 2012.
transfer_enqueue(Ele):-
		transfer_queue(Queue),
		(
			member(Ele,Queue)->
						write(`~M~JElement already in the Queue.`:Queue),
						nothing;
						(
							append(Queue,[Ele],N_Queue),
							%write(`~M~JElement added to the transfer queue `:Ele:N_Queue),
							retractall(transfer_queue(_)), assert(transfer_queue(N_Queue))
						)
		),!.

transfer_enqueue(Ele):- write(`~M~J*********Transfer Enqueue Failed**********`),!.

transfer_dequeue(Ele):-
		transfer_queue(Queue),member(Ele,Queue,1),
		remove(Ele,Queue,N_Queue),
		%write(`~M~JElement removed from the transfer queue `:Ele:N_Queue),
		retractall(transfer_queue(_)), assert(transfer_queue(N_Queue)),!.

transfer_dequeue(Ele):- write(`~M~J**************Transfer Dequeue Failed***********`),!.

move_typhlet(GUID,IP,PORT):-									% This predicate is modified by Shashi on 20-feb-2012
	((atom(GUID),integer(PORT))->nothing;abort),		
	assimilate_code(GUID,AgentCode),
	typhlet_payload(GUID,PayloadList),
	typhlet_GUID(GUID,Handler,Port),
	typhlet_token(GUID,ListOfTokens),time(1,TT),time(TT,Date,Time),	
	connect(IP,PORT,Link),			
	agent_post(platform,Link,send(GUID,Handler,PayloadList,AgentCode,ListOfTokens,Time)),			
	%write(`~M~JAgent is posted successfully over the Link`:Link),			% Added by shashi on 17-feb-2012 for tracking agent migration
	assert(transit_agent(GUID,Link)),
	time(Top,Res),
	assertz(outgoing_typhlet(GUID,Top,Res)),
	transfer_enqueue(GUID),
	%agent_close(GUID),
	agent_post(platform,[],clean_agent(GUID)).
	

move_typhlet(GUID,IP,PORT):- write(`~M~JMove typhlet Failed`).

handler(PName,PLink,clean_agent(GUID)):-
	assimilate_code(GUID,AgentCode),
	typhlet_payload(GUID,PayloadList),
	typhlet_GUID(GUID,Handler,Port),
	typhlet_token(GUID,ListOfTokens),

	retract(typhlet_GUID(GUID,Handler,Port)),	
	retract(typhlet_payload(GUID,PayloadList)),
	retract(typhlet_token(GUID,ListOfTokens)),			
	retract_code(AgentCode),

      % added on 20th May, The problem is that a negative authentication(token based authentication) at the destination destroys the agent, since the trace of agent 
      % from outgoing platform is removed (as seen above) .. so an inactive copy of the agent is kept. This shall be removed at postive ack
	% that also brings how much time it took for the hop. Meanwhile, a negative ack lets us restart the agent here. see handler predicate
	% for what happens at neg and pos ack after sending agents(for migration/cloning)
	write(`~M~JAssembled transit code for`:GUID),
	assert(transit_GUID(GUID,Handler,Port)),
	assert(transit_payload(GUID,PayloadList)),
	assert(transit_token(GUID,ListOfTokens)),
	assert(transit_code(GUID,AgentCode)),
	transit_GUID(GUID,H,P),
	write(`~M~JTransit code is asserted for `:GUID),
	!.




%================================================HANDLER CODE FOR PLATFORM==================================================


%%HIDDEN%%
% handler for the platform(chimera agent). Handles events that occur. Below handles a general test event say. Post an say event from remote platform
% to see whether communications are possible. *NOT IMPORTANT*

handler( Name, _ , say(Term) ):-     
   nl,
   write( Term ),
   write( `, my name is ` ),
   write( Name ),
   nl.





% send_ackm(GUID) event is posted to a platform when a migration of typhlet is complete. The handler here computes the time it took for the hop
% Intially, at the time of sending the code, a timestamp was saved. Now when this ackm arrives, we check the time and find the difference. This 
% is done by time/2[CHECK OUT WIN_REF.pdf for how time/2 can be used to find time difference]. After time is calculated, it is posted into the console
% and all the temporary data of the outgoing agent is removed by retracting the transit predicates.

handler(PName,PLink,send_ackm(GUID,X)):-
	write(`~M~JGot ACK from the destination for `:GUID:PLink:X),
	%(disconnect(PLink)-> write(`~M~J Link is Closed`:PLink);write(`~M~JLink close failed`)), % Added by shashi: This is important as it eats up resources.
	%transfer_dequeue(Ele),(Ele = GUID),
	%write(`~M~JAgent Out of transfer queue`:GUID:Ele),
	outgoing_typhlet(GUID,Top,Res),
	time(Top,End),
	Time is End/Res,nl,
	hoptime_rec(Time,outgoing),
	retract(outgoing_typhlet(GUID,Top,Res)),
	nl,
      output(OriginalStream),output(0),
	write('Typhlet Bearing '),write(GUID),
	write(' reached destination : Time : '),
	write(Time),
	nl,catch(Err,agent_close(GUID)),
	output(OriginalStream),
	retract(transit_GUID(GUID,Handler,Port)),     %remove transit information
	retract(transit_payload(GUID,PayloadList)),   %code is discarded, list of payloads and tokens are discarded
	retract(transit_token(GUID,ListOfTokens)),
	retract(transit_code(GUID,AgentCode)),
	!.

handler(PName,PLink,send_ackm(GUID,X)):- write(`~M~JACK Handler Failed..`),!.

% send_nackm(GUID) is posted to a platform, when a migration is not successful. This happens incase the typhlet does not carry the correct token
% required to enter the destination platform[NEGATIVE AUTHENTICATION]. By default, this comes into use when using the security feature of the platform
% The inactive code is retrieved from the transit predicates and the typhlet is started again.
	
handler(PName,PLink,send_nackm(GUID)):-
	outgoing_typhlet(GUID,Top,Res),
	%transfer_dequeue(Ele),(Ele = GUID),
	%(Ele = GUID),
	%write(`~M~JAgent Out of transfer queue`:GUID:Ele),
	time(Top,End),
	Time is End/Res,nl,
	hoptime_rec(Time,outgoing),
	retract(outgoing_typhlet(GUID,Top,Res)),
	nl,
	output(OriginalStream),output(0),
	write('Typhlet Bearing '),write(GUID),
	write(' was not allowed to enter the destination. Restarting the Typhlet here '),
	nl,
	output(OriginalStream),
	transit_GUID(GUID,Handler,Port),     
	transit_payload(GUID,PayloadList),
	transit_token(GUID,ListOfTokens),
	transit_code(GUID,AgentCode),
	
	assert(typhlet_GUID(GUID,Handler,Port)),     %restore the typhlet information
	assert(typhlet_payload(GUID,PayloadList)),   
	assert(typhlet_token(GUID,ListOfTokens)),
	assert_code(AgentCode),				   %re -assert the typhlet code
	agent_create(GUID,Handler,X),

	retract(transit_GUID(GUID,Handler,Port)),     %remove transit information
	retract(transit_payload(GUID,PayloadList)),   
	retract(transit_token(GUID,ListOfTokens)),
	retract(transit_code(GUID,AgentCode)),
	!.

handler(PName,PLink,send_nackm(GUID)):- write(`~M~JNAK Handler Failed`),!.

%% Added by Shashi
restart_agent(GUID):-
	outgoing_typhlet(GUID,Top,Res),
	time(Top,End),
	Time is End/Res,nl,
	hoptime_rec(Time,outgoing),
	retract(outgoing_typhlet(GUID,Top,Res)),
	nl,
	output(OriginalStream),output(0),
	write('Typhlet :'),write(GUID),
	write(' : encountered transit error. Restarting the Typhlet here '),
	nl,
	output(OriginalStream),
	transit_GUID(GUID,Handler,Port),     
	transit_payload(GUID,PayloadList),
	transit_token(GUID,ListOfTokens),
	transit_code(GUID,AgentCode),
	
	assert(typhlet_GUID(GUID,Handler,Port)),     %restore the typhlet information
	assert(typhlet_payload(GUID,PayloadList)),   
	assert(typhlet_token(GUID,ListOfTokens)),
	assert_code(AgentCode),				   %re -assert the typhlet code
	agent_create(GUID,Handler,X),

	retract(transit_GUID(GUID,Handler,Port)),     %remove transit information
	retract(transit_payload(GUID,PayloadList)),   
	retract(transit_token(GUID,ListOfTokens)),
	retract(transit_code(GUID,AgentCode)),
	!.

restart_agent(GUID):- write(`Restarting agent failed`:GUID),!.


% This is the main handler which handles the incoming typhlet. The identifier(GUID), its Handler name(handler), its code and token comes as
% parameters. In case a platform has a security token set(via set_token/1), the incoming typhlet's token list is checked. If it contains the 
% matching token, the execution proceeds, else the entire thing is discarded and a negative ackm is sent to the sending platform. In case the platform
% does not have a security token set, then no checking is done. A postive ackm is send and the typhlet  is started using agent_create/3.
%

handler( PName, PLink, send(GUID,Handler,PayloadList,[Code|CodeList],ListOfTokens,BackTime)):-				%//
	write(`~M~JReceiving an Agent Posted on Link`:PLink:BackTime),
	nl,
	(
	 platform_token(PToken)->(
	  				  member(PToken,ListOfTokens)->nothing                
									     ;(
										 agent_post(PName,PLink,send_nackm(GUID)),fail
										)
				       )
					;nothing	
	),											%// else forget the whole damn thing.
	output(OriginalStream),output(0),
	nl,time(1,TT),time(TT,Date,Time),
	write('Typhlet bearing '),write(GUID),
	write(' has arrived to this platform. '-Time),nl,
	assert(typhlet_payload(GUID,PayloadList)),   			% intialisation of payload list
	assert(typhlet_token(GUID,ListOfTokens)),				%//
      assert_code([Code|CodeList]),
	agent_post(PName,PLink,send_ackm(GUID,Time)),			
	agent_create(GUID,Handler,Port),					%//
	nl,write('Typhlet has been created at this port : '),			%//
	write(Port),nl,		
	output(OriginalStream),
	assert(typhlet_GUID(GUID,Handler,Port)),	
	%agent_post(GUID,[],migration(GUID,Port)),					%//new event for migration ONLY ! for stable version only one event generation
	%agent_post(flood_helper,[],help_move_typhlet(GUID)),
	help_move_typhlet(GUID),
	nl.

handler( PName, PLink, send(GUID,Handler,PayloadList,[Code|CodeList],ListOfTokens)):-
		write(`~M~JError in handler. The posted agent [guid`:GUID:`] not received properly over the Link`:PLink:`May be Backtime not mentioned`).

%%================================================================================================================
%%PHEROMONE SUPPORT %%Pheromone definitions in phercon.pl

% pheromone_diffuse handler : This is invoked when neighboring platform post a diffuse event on this platform. The
% invocation results in calling the pheromone_generate present in pheromone.pl. Since the handler is hidden, this 
% part of handler cannot be written inside another file. So, ensure_loaded pheromone has to be invoked here. Since 
% loading of pheromone.pl is the responsibility of the user, I do a catch instead.					%CHECK
 
handler(Pname,Plink,pheromone_diffuse(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen,MName)):-
	catch(Error,pheromone_generate(P_ID,S_ID,Conc,Cmax,Timeout,Tmax,SpanLen,MName),Culprit).
	
%Added by shashi on 21-feb-2012 for clearing neibhouring pheromones once an RRS is serviced.

handler(Pname,Plink,pheromone_clear(P_ID,MName)):-
		catch(Error,pheromone_clear(P_ID,MName),Culprit).

%% Added by Shashi
% These hanlder is added to track and trace the activity of mobile agent transit

handler(PName,PLink,('read',Param1)):-
	time(1,T),time(T,Date,Time),catch(Err,retract(transit_agent(Ele,PLink))),
	%nl,write(`Event Occured `:'read'),write(' at time':Time:PLink),
	!.

handler(PName,PLink,('write',Param1)):-
	time(1,T),time(T,Date,Time),
	(
		transit_agent(Ele,PLink)-> transfer_dequeue(Elem),((Elem = Ele)-> write(`~M~J Same`:Elem:Ele);write(`~M~J Not Similar`:Elem:Ele))
							,write(`~M~JAgent Out of transfer queue`:Ele);write(``)
	),
	%nl,write(`~M~JEvent Occured `:'write'),write(' at time':Time:PLink),
	!.

handler(PName,PLink,('error',Param1)):-
	time(1,T),time(T,Date,Time),
	%nl,write(`Event Occured `:'error':Param1),write(' at time':Time:PLink),
	(Err_no,Err_name) = Param1,
	(
		(Err_no = 10057, Err_name = 'sck_write')->
								(
									write(`~M~JAgent Failed to migrate. Trying Again...............`),
									findall(GUID,outgoing_typhlet(GUID,Top,Res),List),
									%write(`~M~J Outgoing List `:List),
									transit_agent(Ele,PLink),
									retract(transit_agent(Ele,PLink)),
									%write(`~M~JAgent Out of transfer queue for ERROR restart`:GUID:Ele),
									restart_agent(Ele),
									% agent_post(Ele,[],migration(Ele,Port)),
									help_move_typhlet(Ele), % Actually help_move_typhlet definition needs modification to handle case of this error. Later
									(write(Ele),write(`-Failed to migrate, Has been created again.`))~>DD,
									send_log(DD)
								); nothing
	
	),
	!.

handler(PName,PLink,('error',Param1)):-
			write(`~M~JError handler failed..`),!.


handler(PName,PLink,(Event,Param)):-
	time(1,T),time(T,Date,Time),
	%nl,write(`Event Occured `:Event:Param),write(' at time':Time:PLink),
	!.


%%===========================================UTILITIES=====================================================================

%%HIDDEN%%

% assert_code/1 works on a list of code and asserts it. After asserting, it prints a message in the console. Its an internal predicate
% and therefore kept hidden from the user of this platform{Means that if he creates a assert_code/1 of his own, my predicate here won't
% affect his predicate's working and vica versa }
% It checks for clause(Predicate) when reading from the list and if it sees one, it queries a dynamic/1 via execs/1. This effectively wipes 
% all predicates with the name Predicate/arity. 

assert_code([]):-output(OriginalStream),output(0),nl,write('Agent/Code recieved and asserted'),nl,output(OriginalStream).
assert_code([Code|CodeList]):-
  %write([Code|CodeList]),nl,
  (Code = clause(Predicate)->( output(OriginalStream),output(0),
					 write('Predicate found : '),
  					 write(Predicate),
					 execs(Predicate),
         				 nl,output(OriginalStream)
					);assert(Code)),%,write('Asserting:'),write(Code)
 assert_code(CodeList),!.




%%HIDDEN%%
% retracts_code/1 retracts the list of code and issues an output when done.<SIMPLE>

retract_code([]):-output(OriginalStream),output(0),nl,write('Agent/Code retracted'),nl,output(OriginalStream).
retract_code([Code|CodeList]):-
  (Code = clause(Predicate)->nothing;retract(Code)),
  retract_code(CodeList).


%%HIDDEN%% 
% file_to_list converts a file(full of prolog code) to a list of code. Because I want to send code as onene entity .
% Helps me keep check on the code. There are many ways of doing the above thing. This is one way of implementing it. 
% The inquire builds a list by looping on the read and recursively calling inquire. Finally, that is reversed and presented
% in the unbound LIST variable. This is not an interface function. So I have not pushed up all the bounding checks

file_to_list(FILE,LIST) :- 
   see(FILE), 
   inquire([],R), % gather terms from file
   reverse(R,LIST),
   seen.

inquire(IN,OUT):-
   read(Data), 
   (Data == end_of_file -> OUT = IN ;inquire([Data|IN],OUT) ) . 





% predicate_header/4 prepares the head of the predicate. Example : Predicate = platform, Arity = 3, and What = jatin. 
% The Result obtained is Result = platform(jatin,_,_). 
% Working : its uses functor/3 (CHECK WIN_REF.pdf) to create platform(_,_,_). Then =../2 is used to convert the tuple to a list. we
% get [platform,_,_,_] and we replace the first underscore via replace_underscore/3 by jatin. Finally using =../2 in opposite manner, I get
% platform(jatin,_,_).

% This is used to generate headers for Handler code, and payload code. The user specifies the Handler and payload name. The What is usually 'guid'
% or actual identifier for a typhlet. Once a predicate_header is prepared, we usually call clauses/2 to get the body(code) of the predicate. 
% This is used in lots of predicates below such as generate_unique_predicate, generate_generic_predicate, genhandler etc.


predicate_header(What,Predicate,Arity,Result):-
	functor(R,Predicate,Arity),R=..[H|T],
	replace_underscore(T,What,NT),
	Result=..[H|NT],!.
replace_underscore([H|T],What,[What|T]).





%%HIDDEN%%
% generatePayloadCode is used for generating payload code :D. Given a list of predicates which needs to be 'attached' to a typhlet, we need
% to process those predicates inorder to attach them. Given a predicate : ability(guid,X):-body of ability/2 {THE WAY OF DEFINING A PAYLOAD}
% If we wish to attach ability/2 as payload, then the term 'guid' needs to be replaced by the Typhlet ID(or GUID). So the above code becomes 
% typhlet(agent) specific when ability(actual ID of typhlet,X):-body is generated from above code, by text manipulation. This code is asserted

generatePayloadCode(_,[],[]).
generatePayloadCode(GUID,[(Predicate,Arity)|Tail],PayloadCode):-
	generatePayloadCode(GUID,[(Predicate,Arity)|Tail],[],PayloadCode).
generatePayloadCode(GUID,[(Predicate,Arity)|Tail],TempList,PayloadCode):-
	generate_unique_predicate(GUID,Predicate,Arity,Body),
	append(TempList,Body,NTempList),
	generatePayloadCode(GUID,Tail,NTempList,PayloadCode).
generatePayloadCode(_,[],TempList,TempList).


	

% gensym/1 generates a 8 character atom which is used for creating identifiers for the typhlets. Its used hide/2 to create an atom 
% such that such an atom is not present before in the prolog space. Read hide/2 for more details

gensym(GUID):-
	var(GUID),
	hide(GUID,0),	% the main GUID creation takes place over here
	assert(is_used(GUID)).




% genhandler/2 is similar to what generatePayloadCode. The latter does to payload predicates, while the former does the same thing to Handler predicate
% of a typhlet. It replaces all instances of 'guid' and pushed the actual identifier/name of the typhlet/agent into that code.Uses generate_unique_predicate
% which is the main thing behind the process.

genhandler(GUID,Handler,Result):-
	generate_unique_predicate(GUID,Handler,3,Result).





% generate_unique_predicate/4 is the brain behind all the manipulation. The user specifies code of a handler / payload etc . This code is generic in nature
% since its not attached to any typhlet. However when we attach code (Handler or otherwise) to a typhlet, we replace the 'guid' present in the code by the 
% name/identifier of the typhlet. This makes the said attachment possible. {Try this by doing on a rough copy}. The example given for generatePayloadCode 
% is what happens here. predicate_header/4 is used to make the header predicate(guid,_,_..arity-1 times). The body is then found using clauses. 
% Then generate_unique_predicate/5 is called which recurses through the code of the predicate. It replaces 'guid' by actual idenetifier and pushes
% all the code so generated into Result. The convert to tuple actually is used to convert the code(which is in text form) back to the tuple form (Try
% reading code from a .pl file (use file_to_list/2 ). you will see that the code is in tuples. So regain this back I use covert_to_tuple. Check its code 
% later below

generate_unique_predicate(GUID,Predicate,Arity,Result):-
	var(Result),				                                             
	predicate_header(guid,Predicate,Arity,H),	
	(clauses(H,UserHandlerCode)->(
						generate_unique_predicate(GUID,Predicate,UserHandlerCode,[],R),
						convert_to_tuple(R,Result)
						)
					    ; (
						  output(OriginalStream),output(0),
						  nl,write('Warning :: '),write(Predicate),write(' not found'),nl,
						  output(OriginalStream),
						  unify_with_empty_list(Result,[])
 						)
	),!.	

	
generate_unique_predicate(GUID,Predicate,[Head|Tail],Incoming,Outgoing):-
	portray_clause(Head)~>X,
	name(X,CodeList),
	name(guid,Symbol),
	name(GUID,Glist),
	substitute_all(CodeList,Symbol,Glist,R),
	name(Text,R),
	append(Incoming,[Text],NextIncoming),
	generate_unique_predicate(GUID,Predicate,Tail,NextIncoming,Outgoing).
generate_unique_predicate(_,_,[],I,I).







%%HIDDEN%%
% generate_generate_predicate performs the reverse process of the above predicate. Given a predicate, we find its specific defination with respect 
% to a typhlet. Here for the above example, we will have ability(absadwf,_,_) and we would convert it to ability(guid,_,_):-blahblah. This predicate
% is used when you want to shed a payload. This payload is therfore converted into a generic format (the way a user writes a payload in file) 
% and then pushed into the system. You will see this predicate in use in interface functions such as shed_payload,copy_payload .

generate_generic_predicate(GUID,Predicate,Arity,Result):-                   % a reversal of above predicate
	var(Result),
	predicate_header(GUID,Predicate,Arity,H),
	(clauses(H,UniqueCode)->(
					 generate_generic_predicate(GUID,Predicate,UniqueCode,[],R),
				       convert_to_tuple(R,Result)
					 )
					;(
					   output(OriginalStream),output(0),
					   nl,write('Warning :: Typhlet Predicate '),write(Predicate),write(' not found'),nl,
					   output(OriginalStream),unify_with_empty_list(Result,[])
					 )
	),!.
	
generate_generic_predicate(GUID,Predicate,[Head|Tail],Incoming,Outgoing):-
	portray_clause(Head)~>X,
	name(X,CodeList),
	name(GUID,Glist),
	name(guid,SymList),
	substitute_all(CodeList,Glist,SymList,R),
	name(Text,R),
	append(Incoming,[Text],NextIncoming),
	generate_generic_predicate(GUID,Predicate,Tail,NextIncoming,Outgoing).
generate_generic_predicate(_,_,[],I,I).






%%HIDDEN%%
% convert_to_tuple : converts the incoming code in form of text to tuple type. This is done by converting the text to string and reading from the string. 
% Read/1 reads the code as tuple.
% 'platform_test:-write('hello')' is converted into (platform_test:-write('hello')). Only tuples can be asserted. Text asserted does not serve any purpose
% I suggest you try asserting both the above.
% So given a list of code in text form, this convert each piece of predicate body into tuple. 



convert_to_tuple(IncomingTextList,OutgoingTupleList):- 			
	convert_to_tuple(IncomingTextList,[],OutgoingTupleList).		
convert_to_tuple([Head|Tail],In,Out):-						
	atom_string(Head,String),							
	read(Tuple)<~String,								
	append(In,[Tuple],NextIn),								
	convert_to_tuple(Tail,NextIn,Out).						
convert_to_tuple([],I,I).

%Before coming up with convert_to_tuple, I wrote the contents of R(code I generated; R is in text form) into a file, read it. 
%When I read it, the prolog arranged it in a tuple, thereby helping me assert it without any difficulties
%However, this creates problem of multiple agents opening files ! in parallel , therby making the system
%vunerable to error and breakdown. Now the issue is solved.
	
%==============================================CAPTURING TYPHLET CODE=======================================================

% The following predicates are used for capturing code specific to typhlets. They used predicate_header quite extensively. After predicate_headers,
% clauses/2 is used to fetch the code body and is pushed into a list (usually) 


% assimilate_code/2 fetches all the Typhlet code and presents it as code list in Result. It calls assimilate_payload_code/4, and assimilate_handler_code/3
% which do their job and the results are appended and returned. <Its self explantory> Read the code
assimilate_code(GUID,Result):-
	typhlet_GUID(GUID,Handler,_),
	assimilate_handler_code(GUID,Handler,HandlerCode),
	%write(GUID:Handler:HandlerCode),nl,
	typhlet_payload(GUID,PayloadList),
	assimilate_payload_code(GUID,PayloadList,[],PayloadCode),
	append(HandlerCode,PayloadCode,Result).

assimilate_handler_code(GUID,Handler,HandlerCode):-
	predicate_header(GUID,Handler,3,Result),
%	write(Result),nl,
	(clauses(Result,HandlerCode)->nothing;(output(OriginalStream),output(0),nl,write('Warning :: Handler code for typhlet not found - id '),write(GUID),nl,output(OriginalStream),unify_with_emtpy_list(HandlerCode,[]))).

assimilate_payload_code(GUID,[(Predicate,Arity)|Tail],Incoming,Outgoing):-
	predicate_header(GUID,Predicate,Arity,Result),
	(clauses(Result,Body)->	append(Incoming,Body,NextIncoming) ; (unify_with_empty_list(Body,[]),append(Incoming,Body,NextIncoming))
	),
	assimilate_payload_code(GUID,Tail,NextIncoming,Outgoing).
assimilate_payload_code(_,[],I,I).








%%HIDDEN%%
% replaceGUID/4 is used for cloning purposes. In cloning(see clone_typhlet/2), a new identifier is generated and the original typhlet code is 
% assmilated. This code is AgentCode. All instances of the original identifier in AgentCode is replaced by the new identifier generated before 
% The resulting code is returned as CloneCode. The working of replaceGUID is simple. It parses through the AgentCode, and substitutes using substitute_all/4.
% AgentCode is list of tuples(each tuple being a predicate definiation). Each tuple is converted into string using portray_clause/2. Then the strings 
% are converted into a List of ascii numbers (for the characters of the string) . Substitution is now performed. After the substitution, the code is packed 
% into text(characters again). And pushed into list again. These are then converted into tuples . Result is the CloneCode
% Example : original agent is qwertyu(GUID1) and its code is given below. Lets say the new clone ID(GUID2) is zxcvbn
% AGENTCODE [(hello(qwertyu,X):-abcd(qwertyu),write('heheh')),(bbye(qwertyu):-write(bye))]
% CLONECODE obtained will be : [(hello(zxcvbn,X):-abcd(zxcvbn),write('heheh')),(bbye(zxcvbn):-write(bye))]


replaceGUID(AgentCode,GUID1,GUID2,CloneCode):-
%	write(AgentCode),nl,
	replaceGUID(AgentCode,GUID1,GUID2,[],C),
	convert_to_tuple(C,CloneCode),!.					

replaceGUID([],_,_,Incoming,Incoming).

replaceGUID([Head|Tail],GUID1,GUID2,Incoming,Outgoing):-
%	write([Head|Tail]),nl,
	portray_clause(Head)~>X,
	name(X,CodeList),
	name(GUID1,Remove),
	name(GUID2,Add),
	substitute_all(CodeList,Remove,Add,R),
	name(Text,R),
	append(Incoming,[Text],NextIncoming),	 
	replaceGUID(Tail,GUID1,GUID2,NextIncoming,Outgoing),
	!.






%%HIDDEN%%
% hoptime_rec/2 is used for recording the times. Its essentially used for calcluating the averages. Given the previous no of agents 
% going out/in and the avg out/in time, a new average is calculated each time an agent moves out/in. The time to go out/in is represented by
% T. hoptime_rec with second paramter as outgoing computes the new average outgoing time and updates the statistics. The hoptime-rec with incoming as
% second parameter calculates the same for incoming agents.
hoptime_rec(T,outgoing):-
	hoptime_stat(Out,OutAvg,Inc,IncAvg),
	NewAvg is (((Out *OutAvg)+T)/(Out+1)),
	NewOut is Out +1,
	retract(hoptime_stat(Out,OutAvg,Inc,IncAvg)),
	assert(hoptime_stat(NewOut,NewAvg,Inc,IncAvg)),!.
hoptime_rec(T,incoming):-
	hoptime_stat(Out,OutAvg,Inc,IncAvg),
	NewAvg is (((Inc *IncAvg)+T)/(Inc+1)),
	NewInc is Inc +1,
	retract(hoptime_stat(Out,OutAvg,Inc,IncAvg)),
	assert(hoptime_stat(Out,OutAvg,NewInc,NewAvg)),!.


%%INTERFACE FUNCTION%%
% The user can call hoptime/4 to read the statistics. 
hoptime(Out,OutAvg,Inc,IncAvg):-			 
	var(Out),var(OutAvg),var(Inc),var(IncAvg),   				
	hoptime_stat(Out,OutAvg,Inc,IncAvg).





%%INTERFACE FUNCTION%%
% list_typhlet/0 prints all the typhlets and their ports onto the console. list_typhlet/2 lets you do the same, accept that
% it lets you unify that with variables.
list_typhlet:-							
	forall(	
			(
				typhlet_GUID(G,H,P)
			),
			(
				output(OriginalStream),output(0),
				write('Identifier : '),write(G),write('||| Port : '),write(P),nl,nl,
				output(OriginalStream)

			 )
		 ).
list_typhlet(G,P):-
	typhlet_GUID(G,_,P),!. 			%should I have this cut?






% hides the code from user leaving only the interface meant for him/her.
% FOR THE DEVELOPER : this predicate is used for the hiding the code. So one the console, if you try query, typhlet_GUID or other
% internal predicates, they wont be recognized. So , if you are developing this platform.pl file, and want to test, I would suggest that 
% you comment the call to hide_mycode/0 in platform_start/1(at the beginning of this file). Comment the line where hide_mycode is written and
% then work.

hide_mycode:-
	hide(setup,1),
	hide(database,1),
	hide(platform_port,1),
	hide(execs,1),
	hide(typhlet_GUID,1),
	hide(typhlet_payload,1),
	hide(outgoing_typhlet,1),
	hide(hoptime_stat,1),
	hide(typhlet_token,1),
	hide(transit_GUID,1),
	hide(transit_payload,1),
	hide(transit_token,1),
	hide(transit_code,1),
	hide(platform_token,1),
	hide(code_base,1),
	hide(do,1),
	hide(start_db,1),
	hide(handler,1),
	hide(assert_code,1),
	hide(retract_code,1),
	hide(file_to_list,1),
	hide(inquire,1),
	hide(predicate_header,1),
	hide(replace_underscore,1),
	hide(lame_concatenate,1),
	hide(insert_underscores,1),
	hide(generatePayloadCode,1),
	hide(gensym,1),
	hide(genhandler,1),
	hide(generate_unique_predicate,1),
	hide(generate_generic_predicate,1),
	hide(convert_to_tuple,1),
	hide(write_unique_code,1),
	hide(assimilate_code,1),
	hide(assimilate_handler_code,1),
	hide(assimilate_payload_code,1),
	hide(replaceGUID,1),
	hide(hoptime_rec,1),
	hide(substitute,1),
	hide(substitute_all,1),
	hide(unify_with_empty_list,1),
	hide(redo,1),
	%hide(nothing,1)
	hide(delete,1),
	hide(delete_list,1),
	hide(replace,1).



%SAVES THE ENTIRE THING INTO A .PC file 
save:-save_predicates([setup,do,start_db,platform_start,platform_close,platform_reset,handler,connect,typhlet_create,typhlet_post,typhlet_kill,typhlet_save,add_payload,remove_payload,shed_payload,copy_payload,removeall_payload,shedall_payload,copyall_payload,clone_typhlet,move_typhlet,platform_assert_file,assert_code,retract_code,file_to_list,inquire,lame_concatenate,insert_underscores,generatePayloadCode,gensym,genhandler,generate_unique_predicate,generate_generic_predicate,write_unique_code,assimilate_code,assimilate_handler_code,assimilate_payload_code,replaceGUID,hoptime_rec,hoptime,list_typhlet,hide_mycode,save,substitute,substitute_all,redo,nothing,delete,delete_list,replace,predicate_header,replace_underscore,set_token,save_platform_state,load_platform_state,add_token,remove_token,convert_to_tuple,unify_with_empty_list],'platform.pc').

%test whether the hidden things can be still used or not 

timeout(Name,Status):- write(Name),write(' : gave a timeout'),nl.








substitute(Source, Tag, Content, X) :-
        % make something like a difference list
        append(Tag, After, TagAndAfter),
        % search for a match
        append(Before, TagAndAfter, Source),
        % and finally, compose the output string
        append(Before, Content, BeforeAndContent),
        append(BeforeAndContent, After, X). 

substitute_all(Source, Tag, Content, X) :-
        append(Tag, After, TagAndAfter),
        append(Before, TagAndAfter, Source),
        append(Before, Content, BeforeAndContent),
        append(BeforeAndContent, FinalAfter, X),
        !,
        substitute_all(After, Tag, Content, FinalAfter).

substitute_all(Source, _, _, Source).

'?ABORT?' :-					   %//
	output(OriginalStream),output(0),
	nl,
	write('! ----------------------------------------'),nl,
	write('! ERROR (CHECK PARAMETERS)----------------'),nl,
	write('! ----------------------------------------'),nl,
	%node_info(Ndnm,_,_),
	%(write(`!!,`),write(Ndnm),write(`,Error at node`))~>D,
	%send_log(D),
	beep(600,850),
	output(OriginalStream),
	abort_hook.


%=====================================================
%for unification
unify_with_empty_list([],[]).	
% for looping
redo.
redo:-redo.
%does nothing
nothing.
%adding/deletes elements to a list :: USED FOR ADDING/REMOVING DYNAMIC CLAUSES IN THE CODE BASE
delete(X,[X|T],T).
delete(X,[Y|T],[Y|NT]):-delete(X,T,NT).

delete_list([H|T],[H|T],[]).
delete_list([X1|Y1],Incoming,Outgoing):-
	delete(X1,Incoming,NextIncoming),
	delete_list(Y1,NextIncoming,Outgoing).
delete_list([],Incoming,Incoming).

%replace elements in a list {one element}
replace([],A,B,[]).
replace([H|T],A,B,[B|Result]) :- 
    H=A, 
    replace(T,A,B,Result),!.
replace([H|T],A,B,[H|Result]) :- 
    replace(T,A,B,Result).

%                                            
%magent_save(GUID,FileName):-						%save to file 
%	agent_GUID(GUID,Handler,_),
%	agent_payload(GUID,PayloadList),
%	tell(FileName),
%	lame_concatenate(GUID,Handler,3,H),
%	clauses(H,HandlerCodeList),
%	forall(member(Elem,HandlerCodeList),(portray_clause(Elem)~>X,write(X))),
%	forall(
%			(
%			member((Predicate,Arity),PayloadList)
%			),
%			(
%			lame_concatenate(GUID,Predicate,Arity,R),
%			clauses(R,PredicateCode),
%			forall(member(S,PredicateCode),(portray_clause(S)~>Y,write(Y)))
%			)
%		),
%	told,!.	


%THESE ARE NOT USED ANYMORE IN THE CODE. BETTER FUNCTIONS WERE MADE and REPLACED THEM
%lame_concatenate uses file I/O to prepare the head of a predicate which could be used for clauses/2 to get the predicate code.
%instead we now use the incore predicate_header function.
lame_concatenate(What,Predicate,1,Result):-
	name(What,W),name(Predicate,P),append(P,W,F),name(FileName,F), %//
	tell(FileName),								%//
	write(Predicate),
	write('('),
	write(What),
	write(')'),
	put(46),nl,
	told,
	see(FileName),								%//
	read(Result),
	seen,
	del(FileName).								%//
lame_concatenate(What,Predicate,Arity,Result):-
	Arity > 0,
	name(What,W),name(Predicate,P),append(P,W,F),name(FileName,F),         %//
	tell(FileName),								%//
	write(Predicate),
	write('('),
	write(What),
	UnderscoresLeft is Arity - 1,
	insert_underscores(UnderscoresLeft),
	write(')'),
	put(46),nl,
	told,
	see(FileName),								%//
	read(Result),
	seen,del(FileName),!.							%//
insert_underscores(0).
insert_underscores(No):-
	write(',_'),%write(No),nl,
	UnderscoresLeft is No - 1,
	insert_underscores(UnderscoresLeft).

write_unique_code([Head|Tail],File):-
	tell(File),
	write(Head),
	write_unique_code(Tail,File).
write_unique_code([],File):-told.





