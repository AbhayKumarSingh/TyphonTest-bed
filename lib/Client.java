import java.io.*;
import java.net.*;
import java.util.Scanner;
import java.nio.charset.Charset;
public class Client{
	ObjectOutputStream output;
	ObjectInputStream input;
	String message = "";
    Socket client;
	String path = null;//"\\\\172.16.27.130\\Shashi's Implementation\\Cloning Controller Implementation";
	public static void main(String args[]){
		
		Client app = new Client();
		app.mainLoop();
	}
	
    int readFromUser(){
        Scanner scan = new Scanner(System.in);
        int option;
        do{
            try{
                option = scan.nextInt();
                break;          
            }
            catch(Exception e){
                System.out.println("You can enter options 1, 2 or 3 only.");
                scan.next();
            }
        }while(true);
        return option;

    }

    void createConnection(String ip){

        try {
            client = new Socket( InetAddress.getByName(ip), 19000);
            //Get input and output streams.
            output = new ObjectOutputStream( client.getOutputStream());
            output.flush();
            input = new ObjectInputStream( client.getInputStream());


        }

        catch( EOFException eof ) {
            System.out.println("....");
        }
        catch( IOException io) {
            io.printStackTrace();
        }

    }

    void closeConnection(){
        try{
            //Close connection.
            System.out.println("Closing connection.");
            output.close();
            input.close();
            client.close();
        }
        catch( IOException io) {
            io.printStackTrace();
        }
    }

    void executeOption(int option){
        InputStream    fis;
        BufferedReader br;
        String         ip;
        
        try{
			//Poen file and read target folder location 
			fis = new FileInputStream("../Config/folder.txt");
            br = new BufferedReader(new InputStreamReader(fis, Charset.forName("UTF-8")));
			path = br.readLine() ;
			System.out.println(path);
			br.close();
			br = null;
            fis = null;
		
		
            //Open file and read the ip's
            fis = new FileInputStream("../Config/IPs.txt");
            br = new BufferedReader(new InputStreamReader(fis, Charset.forName("UTF-8")));
            while ((ip = br.readLine()) != null) {
                //For each ip create connection and send option value.
				System.out.println("IP:"+ip);
                if(ip.trim().length()==0)
                    continue;
                createConnection(ip);
                String message = Integer.toString(option) ;
                output.writeObject(message);
                output.flush();
                if(option==1)  {
				output.writeObject(path);
                output.flush();
				}
                
                //Wait for acknowledgement
                try{
                    message = (String) input.readObject();
                    System.out.println( message);
                }
                catch( ClassNotFoundException cnfex) {
                    System.out.println("Unknown object type received");
                }
                //Close connection.
                while(message == option + "done"){
                    closeConnection();
                }

            }
            //Close file.
            br.close();
            br = null;
            fis = null;
			//Inform about the completion of task
            System.out.println("Task #" + option + " completed.");
			Thread.sleep(2000);
			System.exit(0);
        }
        catch(Exception e){
            System.out.println("Excetions to be handled properly:(known)FileNotFound,IOExcep");
        }
    }

	void mainLoop(){
		
        int option;
        do{
            System.out.println("Following are the options:");
            System.out.println("1. Start platforms at machines with ip in the file.");
            System.out.println("2. Kill all the platforms.");
            System.out.println("3. Quit the program.");

            option = readFromUser();
            
            switch(option) {
                case 1:
                    executeOption(1);
                    break;
                
                case 2:
                    executeOption(2);
                    break;
                
                case 3:
                    System.out.println("Quitting");
                    break;
                
                default:
                    System.out.println("This option was not provied");
                    break;
            }

        }while(option != 3);
	}
}

