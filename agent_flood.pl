:-ensure_loaded( system( chimera ) ).

:- dynamic visited_nodes/2. % This predicate keeps a list of visited nodes by the agent.
:- dynamic mode/2. % possible values searching, uninitialized, returning
:- dynamic problemID/2. % possible values none (I think this may be useless), type1, type2, etc. Or it can be modified later as tuple eg.(Sourcenode,type1).
:- dynamic actualSolution/2. % whatever data is there to be attached. -1 denotes no solution
clause( create_flood_agent/2 ).
create_flood_agent( GUID, P ):-
	typhlet_create( GUID, flood_handler, P ),
	add_payload( GUID, [(visited_nodes, 2), (mode, 2), (problemID, 2), (actualSolution, 2)]).


clause( flood_handler/3 ).
flood_handler( guid, Link, formality( Data ) ):-
	write( `formality` ).

clause( visited_nodes/2 ).
visited_nodes( guid, [] ).

clause( mode/2 ).
mode( guid, uninitialized ).

clause( problemID/2 ).
problemID( guid, (none, none) ).

clause( actualSolution/2 ).
actualSolution( guid, -1 ).
