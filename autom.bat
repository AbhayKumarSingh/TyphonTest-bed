start "" "C:\Program Files\WIN-PROLOG 4800\PRO386W.EXE" consult('log_manager.pl'), start_log.
"C:\Program Files\WIN-PROLOG 4800\PRO386W.EXE" consult('master_control.pl'), start_master, exec('runClient.bat','2',_), delay( 10000 ), exec('runClient.bat','1',_), delay( 10000 ), consult('network\network_4_grid_16.pl'), topology_file(Topology), consult( Topology ), delay( 50 ), induce_network2, timer_create( foo, start_handler ), timer_set( foo, 10000 ), !.
