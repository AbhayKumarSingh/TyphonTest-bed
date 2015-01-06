%
%
%		Initiation
%		=-=-=-=-=-=
%
%	This file is consulted in each of the platforms and carry out 
%	the initiation procedures before starting an agent
%     The Extra payloads mentioned in "Payload.pl" in to be added here.

start_exe:-
	( write( `Intranode Calculation` )) ~> IntraCal,
	send_log( IntraCal ),
	problemOnCurrentNode( ProblemHere ),
	node_info( ThisNode, _, _ ),
	( write( `Problem started at,` ), write( ThisNode ), write( `,` ), write( ProblemHere )) ~> Var,
	send_log( Var ),
	solutionOnCurrentNode( SolutionHere, _ ),
	(	ProblemHere == SolutionHere ->
		(
			true,		% use the solution and tell the log server that the solution is already with you.
			( write( `Solution found for,` ), write( ThisNode ), write( `,Currentnode:,` ), write( ThisNode ), write( `,` ), write( ProblemHere )) ~> Var2,
			send_log( Var2 ),
			( write( `Solution received at,` ), write( ThisNode ), write( `,Currentnode:,` ), write( ThisNode ), write( `,` ), write( ProblemHere ) ) ~> Var1,
			send_log( Var1 )
		)
	;
		(
			platform_assert_file( 'agent_flood.pl' ),  % asserts the agent on this platform
			create_flood_agent( G, Pr ),			% creation of a phercon agent as mentioned in "agent_phercon.pl"
			retractall( visited_nodes( G, _ ) ),
			assert( visited_nodes( G, [ThisNode] ) ),
			retractall( mode( G, _ ) ),
			assert( mode( G, searching ) ),
			retractall( problemID( G, ( _,_ ) ) ),
			assert( problemID( G, (ThisNode, ProblemHere) ) ),
			% retractall( agentPassedWithProblemID( _ ) ),
			assert( agentPassedWithProblemID(( ThisNode, ProblemHere ))),
			forall(
				neighbors( Neigh, IP, Port, _, _ )
				,
				(
					connect( IP, Port, LinkToNeigh ),
					clone_typhlet( G, LinkToNeigh ),
					( write( `Packet sent from,`), write( ThisNode ), write( `,to,` ), write( Neigh )) ~> Ss,
					send_log( Ss )
				)
			),
			typhlet_kill( G )
		)
	).
	%platform_assert_file('payload.pl'),		% asserts the payloads 
	%add_payload(G,[(task_info,3),(ex1_task,1)]),	% attached the payloads onto the agent
	%agent_post(G,[],migration(GUID,P)),			% Starts the agent to move on
	%write(`~M~JAgent Moved Away!!!`),!
	%.





