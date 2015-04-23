%:-ensure_loaded(system(chimera)).

%:-agent_create(flood_helper,flood_helper_handler,60000). % Here port address will be assigned properly in future.

%clause(flood_helper_handler/3).
%flood_helper_handler(Name, Link, help_move_typhlet(GUID)):-
clause(help_move_typhlet/1 ).
help_move_typhlet( GUID ):-
	/*visited_nodes(GUID,VList),
	retractall(visited_nodes(GUID,_)),
	node_info(CurrentNode,_,_),
	append(VList,[CurrentNode],FList),
	(write(FList))~>Var,
	send_log(Var),
	assert(visited_nodes(GUID,FList)),
	neighbors(_,IP,PlatformPort,_,_),
	connect(IP,PlatformPort,L),
	move_typhlet(GUID,L).*/
	( write( `Intranode Calculation` )) ~> IntraCal,
	send_log( IntraCal ),
	node_info( CurrentNode, _, _ ),           % this info can be kept out of all the ifs elses
	mode( GUID, CurrentMode ),
	problemID( GUID, ( ProbNode, AgentProblem ) ),
	visited_nodes( GUID, VList ),
	/*( write( CurrentNode ), write( `,` ), write( GUID ), write( `,` ), write( ProbNode ), write( `,` ), write( VList ) ) ~> Var,
	send_log( Var ),*/
	length( VList, L ),
	(( CurrentMode == searching, member( Last, VList, L ))  ->
		(
			( agentPassedWithProblemID( ( ProbNode, AgentProblem ) ) ->	 % right now we consider that only one node has problem.
				(
					% kill the typhlet.
					typhlet_kill( GUID )
				)
			;   % else the problem is fresh
				(
					solutionOnCurrentNode( Solution, _ ),
					( Solution == AgentProblem ->
						(
							% start return mode
							( write( `Solution found for,` ), write( ProbNode ), write( `,Currentnode:,` ), write( CurrentNode ), write( `,` ), write( Solution )) ~> Var2,
							send_log( Var2 ),
							retractall( mode( GUID, _ ) ),
							assert( mode( GUID, returning ) ),
							
							retractall( actualSolution( GUID, _ ) ),
							solutionOnCurrentNode( Solution, SolutionData ),	% assumption is only one solution is there at a node.
							assert( actualSolution( GUID, SolutionData ) ),
							
							% retractall( agentPassedWithProblemID( _ ) ),
							assert( agentPassedWithProblemID(( ProbNode, AgentProblem ))),
							
							move_backwards( GUID, Last, VList )
						)
					; % node does not have the solution
						(
							% flood the agent.
							append( VList, [CurrentNode], NewVList ),
							retractall( visited_nodes( GUID, _ ) ),
							assert( visited_nodes( GUID, NewVList ) ),

							% retractall( agentPassedWithProblemID( _ ) ),
							assert( agentPassedWithProblemID(( ProbNode, AgentProblem ))),
							forall(
								(
									neighbors( NeighNode, IP, Port, _, _ )
								),
								(
									( NeighNode \= Last ->
										(
											clone_typhlet_local(GUID, GUID2),
											move_typhlet(GUID2, IP, Port),
											( write( `Packet sent from,`), write( CurrentNode ), write( `,to,` ), write( NeighNode )) ~> Ss,
											send_log( Ss )
										)
									;
										(
											true
										)
									)

								)
							),
							typhlet_kill( GUID )
						)
					)
				)
			)
		)
	;   %else returning
		(
			/*( write( `Here` ) ) ~> Test,
			send_log( Test ),*/
			problemOnCurrentNode( OriginalProblem ), % Another option is we can use the source node id, which may be better.
			( OriginalProblem == AgentProblem ->
				(
					( node_info( ProbNode, _, _ ) ->
						(
							% consume the solution
							typhlet_kill( GUID ),		%more details can be added later like memorizing the solution, etc.
							% retract has been replaced by retractall (experimental)
							retractall( problemOnCurrentNode( AgentProblem )),
							assert( problemOnCurrentNode( none )),
							( write( `Solution received at,` ), write( ProbNode ), write( `,Currentnode:,` ), write( CurrentNode ), write( `,` ), write( AgentProblem) ) ~> Var1,
							send_log( Var1 )
						)
					;
						(
							( write( `Origin node different,SourceNode:,` ), write( ProbNode ), write( `,Currentnode:,` ), write( CurrentNode ), write( `,` ), write( AgentProblem) ) ~> Var3,
							send_log( Var3 ),
							move_backwards( GUID, Last, VList )
						)
					)
				)
			;  % else someones else problem
				(
					% help the packet return.
					move_backwards( GUID, Last, VList )
				)
			)
		)
	).

clause(move_backwards/3 ).
move_backwards( GUID, Last, VList ):-
	retractall( visited_nodes( GUID, _ ) ),
	append( NewList, [Last], VList ), 					     % actually we are removing last node
	assert( visited_nodes( GUID, NewList ) ),				     % take care of empty list case later maybe
	neighbors( Last, IP, Port, _, _ ),
	%connect( IP, Port, Link ),
	%move_typhlet( GUID, Link ).
	move_typhlet( GUID, IP, Port ),
	node_info( CurrentNode, _, _ ),
	( write( `Packet sent from,`), write( CurrentNode ), write( `,to,` ), write( Last )) ~> Ss,
	send_log( Ss ).

