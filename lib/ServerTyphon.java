import java.io.*;
import java.net.*;

public class ServerTyphon{
	ObjectOutputStream output;
	ObjectInputStream input;
	Socket connection;
	ServerSocket server;
    int option;
    String message;
	public static void start(String args[]){
	try{
		System.out.println("Controller service commencing");
		ServerTyphon app = new ServerTyphon();
		app.runServer();
		}
	catch(Exception e){
		e.printStackTrace();
	}
	}
	 public static void stop(String arg[]) {
		try{		
				ServerTyphon app = new ServerTyphon();
				app.server.close();
                System.out.println("Stopping Service");
            }
		catch( Exception eof ) {
			System.out.println("Client terminated connection");
			eof.printStackTrace();
		}
	 }
	 
	public static void main(String[] args) {
	try{
        if ("start".equals(args[0])) {
            start(args);
        } else if ("stop".equals(args[0])) {
            stop(args);
        }
	}
	catch (Exception e){
		 start(args);
	}
    }
	public void runServer(){
		
		String path = null;
		try {
			//Create a ServerSocket.
			server = new ServerSocket( 19000, 100);
			System.out.println("Controller service started");
            while(true) {
                //Wait for a connection.
                connection = server.accept();
                System.out.println("Connection received from" + connection.getInetAddress().getHostName());

                //Get input and output streams.
                output = new ObjectOutputStream( connection.getOutputStream());
                output.flush();
                input = new ObjectInputStream( connection.getInputStream());
                System.out.println("Got I/O streams");

                //Process connection.
                //String message = "SERVER>>> Connection successful";
                //output.writeObject(message);
                //output.flush();


                try{
                    message = (String) input.readObject();
                    option = Integer.parseInt(message);
					if(option==1)
					path = (String) input.readObject();
                }
                catch( ClassNotFoundException cnfex) {
                    System.out.println("Unknown object type received");
                }

                switch(option){
                    case 1:
						System.out.println(path);
						Runtime.getRuntime().exec("cmd.exe /c start \"\" /D \""+path+"\" \"C:\\Program Files\\WIN-PROLOG 4800\\PRO386W.EXE\" ensure_loaded(system(chimera)), ensure_loaded(task_manager),start_task_manager(15000).");//   
                        message = "1done";
                        output.writeObject(message);
                        output.flush();
                        break;

                    case 2:
                        Runtime.getRuntime().exec("C:\\Windows\\System32\\taskkill.exe /F /IM PRO386W.EXE /T");
                        message = "2done";
                        output.writeObject(message);
                        output.flush();
                        break;

                    default:
                        System.out.println("Code should not reach default.");
                        break;
                        
                }
                //Close connection.
                //System.out.println("User terminated connection");
                output.close();
                input.close();
                connection.close();
                System.out.println("Closing this connection.");
            }
		}
		catch( EOFException eof ) {
			System.out.println("Client terminated connection");
		}
		catch( IOException io) {
			io.printStackTrace();
		}
	}
}			
