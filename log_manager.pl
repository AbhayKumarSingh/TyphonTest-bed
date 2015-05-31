%=======================================================================================
%			Log Manager	
%		    ----------------	
% Author:		Shashi Shekhar Jha {special8688@gmail.com}
% Date:		03-January-2012 	
% Description:	Log Manager is responsible for logging all the minutes of the activities
%			and datas generated during any run of an experiment. It reads data through 
%			socket and writes the log in "log/date-time-log.txt" file name. 
%
%=======================================================================================


:-dynamic sock_name/2.
:-dynamic curr_milli/2.

:-ensure_loaded(system(chimera)).
% start_log/0 starts the log manager and opens a win-socket. It creates a file with current 
% timestamp and writes all the data into it.

clause(start_log/0).
start_log:-
	time( 1, T ), time( T, Date, Time),T = (_,N),write(Date)~> D,
	copy(4,K)<~D~>Day,string_chars(Date,Dlist),string_chars(Day,Dayl),
	member(T1,Dayl,1),member(T2,Dayl,2),member(T3,Dayl,3),member(T4,Dayl,4),
	remove(T1,Dlist,F1Date),remove(T2,F1Date,F2Date),remove(T3,F2Date,F3Date),
	remove(T4,F3Date,F4Date),string_chars(FDate,F4Date),
	time( N, H, M, S, _ ),consult('events.pl'),outputFileName( OutFile ),(write(`log\`),write( OutFile ),write(`.csv`))~>Name,
	telling( Previous ),
	Astring = 'E:\ResourcesAndOutpForThesis\UsefulScriptsThesis\recentLog.txt',
	open( Astring, write ),
	tell(Astring),
	write( Name ),
	tell( Previous ),
	close(Astring),
	atom_string(Nm,Name),open(Nm,append),
	tell(Nm),assert(log_name(Nm)),
	write(`=====Log created on`-Date-Time-`=========`),
	socket_handler(log_manager,log_sock),
	screate(log_manager,49000),
	%agent_create(log_manager,log_handler,49000),
	output(OP),
	output(0),
	write(`~M~JLog Manager Started on`-Date-Time),
	time(Top,Res),assert(curr_milli(Top,Res)),
	output(OP).


% Log_manager Handler
clause(log_handler/3).
log_handler(Name,Link,data(Data)):-
		ip_link(Link,Inet),
		write_log(Data,Inet),
		output(OP),
		output(0),
		write_log(Data,Inet),
		output(OP),
		retractall(ip_link(Link,Inet)),
		save_log,
		!.

log_handler(Name,Link,('open',Param1,P2)):- 
							output(OP),
							output(0),
							write(Param1),
							output(OP),
							retractall(ip_link(Link,Inet)),
assert(ip_link(Link,Param1)),!.
% Socket handler : [sck_accept] |it creates a server socket on the above listening socket.

log_sock(SCK,sck_accept,0):-
	hide(NewSck,0),
	socket_handler(NewSck,log_sock),
	screate(NewSck,SCK),
	http_inet(NewSck,Inet),
	assert(sock_name(NewSck,Inet)).
	

% Socket handler :[sck_read]

log_sock(SCK,sck_read,0):-
	srecv(SCK,Data),
	sock_name(SCK,Inet),
	write_log(Data,Inet),
	output(OP),
	output(0),
	write_log(Data,Inet),
	output(OP),
	ssend(SCK,`Recorded`).

% Socket handler :[sck_close]

log_sock(SCK,sck_close,_):-
	sock_name(SCK,Inet),
	retractall(sock_name(SCK,Inet)),
	%write_log(`Closed Connection`,Inet),
	%output(OP),
	%output(0),
	%write_log(`Closed Connection`,Inet),
	%output(OP),
	sclose(SCK),
	save_log.

% http_inet/2 is used to locate the Inet address of the socket
% get http data relating to a given socket

http_inet( Sock, Inet ) :-
   sckhdl( Sock, Hand ),
   wintxt( [], -1, -1, Text ),
   wintxt( ([],16'10), 1, 3, `~(10)` ),
   winapi( (wsock32,getpeername), [Hand,[],([],16'10)], 0, _ ),
   wintxt( ([],16'04), 1, 3, Info ),
   wintxt( [], -1, -1, Text ),
   strchr( Info, List ),
   List = [Addr],
   winapi( (wsock32,inet_ntoa), [Addr], 0, Data ),
   wintxt( Data, 0, 0, Inet ).

% write_log/1 writes in the log file 

write_log(Data,Inet):-
	time(1,T),time(T,Date,Time),
	curr_milli(Top,Res),
	time(Top,End),
	MS is End/Res,
	swrite(`~M~J`), 
	swrite(Date),
	swrite(`,`),
	swrite(Time),
	swrite(`,`),
	swrite(MS),
	swrite(`,`),
	swrite(Inet),
	swrite(`,`),
	swrite(Data),
	swrite(`,`).


%close_log/0 closes the files and sockets and thus shuts down the log manager.

clause(close_log/0).
close_log:-
	
	time( 1, T ), time( T, Date, Time),
	write(`~M~J============ End log on`-Date-Time-`============`),
	log_name(Nm),
	told,
	close(Nm),
	retract(log_name(Nm)),
	sclose(log_manager),
	%agent_close(log_manager),
	retract(curr_milli(_,_)),
	write(`~M~J============ End log on`-Date-Time-`============`).

%save_log/0 saves the log files and opens the stream again.
clause(save_log/0).
save_log:-
	told,
	log_name(Nm),
	close(Nm),
	%ms(repeat,T),T>10,
	open(Nm,append),
	tell(Nm).

%reset_log/0 resets the log manager i.e. it stops the existing log and start a new log .
clause(reset_log/0).

reset_log:-
		catch(E,close_log),
		write(`~M~J============ Restartng Log Server ============`),
		start_log.
% The log_manager agent can be invoked from another location to start and close log_manager.

clause(start_logagent/0).
start_logagent:-
	agent_create(log_agent,log_agent_handler,49001).

log_agent_handler(Name,Link,start(X)):-
	start_log.
log_agent_handler(Name,Link,close(X)):-
	close_log.
log_agent_handler(Name,Link,reset(X)):-
	reset_log.

close_logagent:-
	agent_close(log_agent).



