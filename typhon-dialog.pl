new_dialog :- 
   _S1 = [ws_sysmenu,ws_popup,ws_caption,dlg_ownedbyprolog],
   _S2 = [ws_child,ws_tabstop,ws_visible,bs_pushbutton,bs_text,bs_center,bs_vcenter],
   _S3 = [ws_child,ws_visible,ss_left],
   wdcreate(  new_dialog,    `New Dialog`,                                651,  93, 656, 622, _S1 ),
   wccreate( (new_dialog,1), button, `1. Select Typhon Folder`,           200,  30, 180,  30, _S2 ),
   wccreate( (new_dialog,2), button, `2. Provide Network IP Addresses`,   200,  90, 180,  30, _S2 ),
   wccreate( (new_dialog,3), button, `3. Files to be loaded at Run-time`, 200, 150, 180,  30, _S2 ),
   wccreate( (new_dialog,4), button, `4. Load Master Controller`,         200, 210, 180,  30, _S2 ),
   wccreate( (new_dialog,5), button, `5. Configure the Network`,          200, 270, 180,  30, _S2 ),
   wccreate( (new_dialog,6), button, `6. Start Platforms at Nodes`,       200, 330, 180,  30, _S2 ),
   wccreate( (new_dialog,7), button, `7. Start Master Controller`,        200, 390, 180,  30, _S2 ),
   wccreate( (new_dialog,8), button, `8. Generate the Network`,           200, 450, 180,  30, _S2 ),
   wccreate( (new_dialog,9), button, `9. Exit`,                           200, 510, 180,  30, _S2 ),
   wccreate( (new_dialog,0), static, `Master Node`,                        10, 170,  70,  20, _S3 ).

%Insert code after write (`Hi ??`). 

start:-
new_dialog,
wshow(new_dialog,1),
window_handler(new_dialog,new_dialog_hndlr),repeat, 
	wait(0), 
	fail.

new_dialog_hndlr((new_dialog,1),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
write(`~M~J Folder Information is recorded`),exec('notepad.exe','Config\folder.txt',X),
wtext((new_dialog,1),`Typhon Folder Selection: Complete`),
wenable((new_dialog,1),0).
%wstyle((new_dialog,1),16'00008000).
%16'08000000),wstyle((new_dialog,1),16'10000000). %Code needs to be inserted to handle folder file creation.


new_dialog_hndlr((new_dialog,2),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
write(`~M~J IPs Information is recorded`),exec('notepad.exe','Config\IPs.txt',X),
wtext((new_dialog,2),`IP Address List: Procured!`),
wenable((new_dialog,2),0).	%Code needs to be inserted to handle creation of txt file with IP addresses. 
			%This could be done by repeatedly querying the user for the IP strings & saving it to a txt file.


new_dialog_hndlr((new_dialog,3),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
write(`~M~J Files to be loaded is recorded`),exec('notepad.exe','Config\LoadList.txt',X),
wtext((new_dialog,3),`Files@Run-Time: Procured!`),
wenable((new_dialog,3),0). %Code needs to be inserted to handle creation of txt file with files to be loaded addresses. 
			%This could be done by repeatedly querying the user for the IP strings & saving it to a txt file.



new_dialog_hndlr((new_dialog,4),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
write(`~M~J Loading Master_Control`), ensure_loaded('master_control.pl'),
wtext((new_dialog,4),`Master Controller: Loaded!`),
wenable((new_dialog,4),0).

new_dialog_hndlr((new_dialog,5),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,gen_network,
write(`~M~J Generating Network`),wtext((new_dialog,5),`Network Generation: Complete!`),
wenable((new_dialog,5),0).


new_dialog_hndlr((new_dialog,6),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
nl, write(`~M~J Starting Network Control`),start_network_control,wtext((new_dialog,6),`Network Controller: Started!`),
wenable((new_dialog,6),0).

new_dialog_hndlr((new_dialog,7),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
write(`~M~J Strating Master Agent`),start_master,wtext((new_dialog,7),`Master Controller: Started!`),
wenable((new_dialog,7),0).

new_dialog_hndlr((new_dialog,8),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
nl, write(`~M~J Initiating network formation`),induce_network,wtext((new_dialog,8),`Network Induction: Commenced!`),   %WHAT DOES IT DO HERE HOW IS THIS DIFF FROM NETWORK GENERATION?? 
															%CHANGE WORDINGS ACCORDINGLY
wenable((new_dialog,8),0).

new_dialog_hndlr((new_dialog,9),WindowsMsg,_,_):-
WindowsMsg = msg_button,!,
nl, write(`Exiting...`),
wclose(new_dialog). %TRANSFER CONTROL TO MOBILE AGENT HELP OR SOME SUCH.


new_dialog_hndlr(Window, Msg, Data, Result):-
%nl, write(Window - Msg - Data - Result), beep(440,250), ttyflush, 
window_handler(Window, Msg, Data, Result).

close:-
	repeat, 
	wait(0), 
	fail.


