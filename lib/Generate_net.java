import java.io.*;
import java.util.*;
import java.nio.charset.Charset;


/**
 * @author : Shashi Shekhar Jha
 * @email  : special8688@gmail.com 
 * @date   : 7-May-2012
 * @description: This program creates a network configuration to be used with Typhon Framework
 *  				in various topologies based on physical systems [Limit 100 nodes as far now]
 */
public class Generate_net {

	/**
	 * @param args
	 */
	static Vector IP = new Vector();
	static Hashtable nodes_per_IP = new Hashtable(); 
	static int nodes = 0;
	static String node_config[][]=new String [100][6];
	static int network [][] = new int[100][100];
	static String input_str = "";
	//static String topo = "ring";
	//static String topo = "star";
	//static String topo = "Partial_mesh";
	//static String topo = "binary_tree";
	static String topo = "grid";
	
	public static void star_topology(PrintWriter out){
		String file_str  = "";
		Enumeration node_ip = nodes_per_IP.keys();
		int x = 0;
		int intial_node = 0, next_switch=0;
		for(int k=0;k<nodes;k++){
			if(k==next_switch){		
			String ipadd = (String) node_config[k][1];
			x = Integer.parseInt(nodes_per_IP.get(ipadd).toString());
			intial_node = k;
			next_switch = x+k;
			System.out.println("x="+x+"|initial_node="+intial_node+"|next_switch="+next_switch+"|IP="+ipadd);
			file_str = "node("+node_config[k][0]+"):-\n"+
			"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"; 
			
			for(int r=1;r<=x;r++){
				
				if((k+r)<nodes)
					file_str = file_str+"assert(neighbors('"+node_config[k+r][0]+"',`"+node_config[k+r][1]+"`,"+node_config[k+r][2]+","+node_config[k+r][4]+","+node_config[k+r][5]+")),\n";
				
			}
			if(intial_node!=0)
				file_str = file_str+"assert(neighbors('"+node_config[intial_node-1][0]+"',`"+node_config[intial_node-1][1]+"`,"+node_config[intial_node-1][2]+","+node_config[intial_node-1][4]+","+node_config[intial_node-1][5]+")),\n";
			
			file_str = file_str+"platform_start("+node_config[k][2]+"),\n"+
			"start_task_manager("+node_config[k][3]+"),\n"+
		//	"start_queue_manager("+node_config[k][4]+","+node_config[k][5]+"),\n"+
		//	"consult('initiation.pl'),\n"+input_str+"\n"+
			"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
			}
			else
			{
				file_str = "node("+node_config[k][0]+"):-\n"+
				"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"; 
				file_str = file_str+"assert(neighbors('"+node_config[intial_node][0]+"',`"+node_config[intial_node][1]+"`,"+node_config[intial_node][2]+","+node_config[intial_node][4]+","+node_config[intial_node][5]+")),\n";
				file_str = file_str+"platform_start("+node_config[k][2]+"),\n"+
				"start_task_manager("+node_config[k][3]+"),\n"+
			//	"start_queue_manager("+node_config[k][4]+","+node_config[k][5]+"),\n"+
			//	"consult('initiation.pl'),\n"+input_str+"\n"+
				"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
				
			}
			 out.println(file_str);
		        out.flush();
		}//for
	}
	public static void ring_topology(PrintWriter out){
		
		String file_str  = "";
		for(int k=0;k<nodes;k++){
				if(k==0){
					file_str = "node("+node_config[k][0]+"):-\n"+
					"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"+
					"assert(neighbors('"+node_config[k+1][0]+"',`"+node_config[k+1][1]+"`,"+node_config[k+1][2]+","+node_config[k+1][4]+","+node_config[k+1][5]+")),\n"+ 
					"assert(neighbors('"+node_config[nodes-1][0]+"',`"+node_config[nodes-1][1]+"`,"+node_config[nodes-1][2]+","+node_config[nodes-1][4]+","+node_config[nodes-1][5]+")),\n"+ 
					"platform_start("+node_config[k][2]+"),\n"+
					"start_task_manager("+node_config[k][3]+"),\n"+
				//	"start_queue_manager("+node_config[k][4]+","+node_config[k][5]+"),\n"+
				//	"consult('initiation.pl'),\n"+input_str+"\n"+
					"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
				}
				else if(k == (nodes-1)){
					file_str = "node("+node_config[k][0]+"):-\n"+
					"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"+
					"assert(neighbors('"+node_config[0][0]+"',`"+node_config[0][1]+"`,"+node_config[0][2]+","+node_config[0][4]+","+node_config[0][5]+")),\n"+ 
					"assert(neighbors('"+node_config[k-1][0]+"',`"+node_config[k-1][1]+"`,"+node_config[k-1][2]+","+node_config[k-1][4]+","+node_config[k-1][5]+")),\n"+ 
					"platform_start("+node_config[k][2]+"),\n"+
					"start_task_manager("+node_config[k][3]+"),\n"+
				//	"start_queue_manager("+node_config[k][4]+","+node_config[k][5]+"),\n"+
				//	"consult('initiation.pl'),\n"+input_str+"\n"+
					"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
				}
				else{
					file_str = "node("+node_config[k][0]+"):-\n"+
						"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"+
						"assert(neighbors('"+node_config[k+1][0]+"',`"+node_config[k+1][1]+"`,"+node_config[k+1][2]+","+node_config[k+1][4]+","+node_config[k+1][5]+")),\n"+ 
						"assert(neighbors('"+node_config[k-1][0]+"',`"+node_config[k-1][1]+"`,"+node_config[k-1][2]+","+node_config[k-1][4]+","+node_config[k-1][5]+")),\n"+ 
						"platform_start("+node_config[k][2]+"),\n"+
						"start_task_manager("+node_config[k][3]+"),\n"+
				//		"start_queue_manager("+node_config[k][4]+","+node_config[k][5]+"),\n"+
				//		"consult('initiation.pl'),\n"+input_str+"\n"+
						"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
				}
			    out.println(file_str);
		        out.flush();
		}//for	
		
		
	}
	
public static void ring_topology_mod(PrintWriter out){
		
		String file_str  = "";
		for(int k=0;k<nodes;k++){
				if((k-4)<=0){
					int l =0;
					l = 4-k;
					file_str = "node("+node_config[k][0]+"):-\n"+
					"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"+
					"assert(neighbors('"+node_config[l+1][0]+"',`"+node_config[l+1][1]+"`,"+node_config[l+1][2]+",6,0)),\n"+ 
					"assert(neighbors('"+node_config[nodes-k-1][0]+"',`"+node_config[nodes-k-1][1]+"`,"+node_config[nodes-k-1][2]+",6,0)),\n"+ 
					"platform_start("+node_config[k][2]+"),\n"+
					"start_task_manager("+node_config[k][3]+"),\n"+
				//	"consult('initiation.pl'),\n"+input_str+"\n"+
					"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
				}
				else if((k+4) >= (nodes-1)){
					int l =0;
					l = nodes - k;
					file_str = "node("+node_config[k][0]+"):-\n"+
					"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"+
					"assert(neighbors('"+node_config[k-1][0]+"',`"+node_config[k-1][1]+"`,"+node_config[k-1][2]+",6,0)),\n"+ 
					"assert(neighbors('"+node_config[l-1][0]+"',`"+node_config[l-1][1]+"`,"+node_config[l-1][2]+",6,0)),\n"+ 
					"platform_start("+node_config[k][2]+"),\n"+
					"start_task_manager("+node_config[k][3]+"),\n"+
				//	"consult('initiation.pl'),\n"+input_str+"\n"+
					"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
				}
				else{
					
					file_str = "node("+node_config[k][0]+"):-\n"+
						"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n"+
						"assert(neighbors('"+node_config[k+4][0]+"',`"+node_config[k+4][1]+"`,"+node_config[k+4][2]+",6,0)),\n"+ 
						"assert(neighbors('"+node_config[k-4][0]+"',`"+node_config[k-4][1]+"`,"+node_config[k-4][2]+",6,0)),\n"+ 
						"platform_start("+node_config[k][2]+"),\n"+
						"start_task_manager("+node_config[k][3]+"),\n"+
					//	"consult('initiation.pl'),\n"+input_str+"\n"+
						"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
				}
			    out.println(file_str);
		        out.flush();
		}//for	
		
		
	}

public static void binary_tree(PrintWriter out){
	
	String file_str  = "";
	for(int k=0;k<nodes;k++){
			
				file_str = "node("+node_config[k][0]+"):-\n"+
					"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n";
				int l = 0;
				l = (k-1)/2;
				//Parent
				if(l>=0 && k!=0)
				file_str = file_str + "assert(neighbors('"+node_config[l][0]+"',`"+node_config[l][1]+"`,"+node_config[l][2]+","+node_config[l][4]+","+node_config[l][5]+")),\n";
					
				//Child_left
				if((2*k+1)<=(nodes-1) )
					file_str = file_str + "assert(neighbors('"+node_config[2*k+1][0]+"',`"+node_config[2*k+1][1]+"`,"+node_config[2*k+1][2]+","+node_config[2*k+2][4]+","+node_config[2*k+2][5]+")),\n";
				// Child Right
				if((2*k+2)<=(nodes-1))
					file_str = file_str + "assert(neighbors('"+node_config[2*k+2][0]+"',`"+node_config[2*k+2][1]+"`,"+node_config[2*k+2][2]+","+node_config[2*k+2][4]+","+node_config[2*k+2][5]+")),\n";
					
					file_str = file_str +"platform_start("+node_config[k][2]+"),\n"+
					"start_task_manager("+node_config[k][3]+"),\n"+
					//"start_queue_manager("+node_config[k][4]+","+node_config[k][5]+"),\n"+
					//"consult('initiation.pl'),\n"+input_str+"\n"+
					"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
			
		    out.println(file_str);
	        out.flush();
	}//for	
	
	
}

public static void grid_topology(PrintWriter out){
	
	String file_str  = "";
	
	Scanner sc = new Scanner(System.in);
	System.out.println("Enter number of columns in the grid ");
	int col_len = sc.nextInt();
	System.out.println("The grip topology is created: \n");
	int row_len = (nodes/col_len);
	if((nodes%col_len)>0){
		row_len += 1;
	}
	int row =0, col =0;

	for(int k=0;k<nodes;){
		
		for(col =0; (col<col_len) && (k < nodes) ;col++){
			
			file_str = "node("+node_config[k][0]+"):-\n"+
						"assert(node_info('"+node_config[k][0]+"',`"+node_config[k][1]+"`,"+node_config[k][2]+")),\n";
			System.out.print("node "+k);
			if(((row+1)<row_len) && ((row+1)*col_len+col)<nodes){
				int x = ((row+1)*col_len+col);
				file_str = file_str + "assert(neighbors('"+node_config[x][0]+"',`"+node_config[x][1]+"`,"+node_config[x][2]+","+node_config[x][4]+","+node_config[x][5]+")),\n";
			}
			if(((col+1)<col_len) && (row*col_len+(col+1))<nodes){
				int x = (row*col_len+(col+1));
				file_str = file_str + "assert(neighbors('"+node_config[x][0]+"',`"+node_config[x][1]+"`,"+node_config[x][2]+","+node_config[x][4]+","+node_config[x][5]+")),\n";
			}
			if(((row-1)>= 0)){
				int x = ((row-1)*col_len+col);
				file_str = file_str + "assert(neighbors('"+node_config[x][0]+"',`"+node_config[x][1]+"`,"+node_config[x][2]+","+node_config[x][4]+","+node_config[x][5]+")),\n";
			}
			if(((col-1)>= 0)){
				int x = (row*col_len+(col-1));
				file_str = file_str + "assert(neighbors('"+node_config[x][0]+"',`"+node_config[x][1]+"`,"+node_config[x][2]+","+node_config[x][4]+","+node_config[x][5]+")),\n";
			}
			
			file_str = file_str + "platform_start("+node_config[k][2]+"),\n"+
									"start_task_manager("+node_config[k][3]+"),\n"+
									//"consult('initiation.pl'),\n"+input_str+"\n"+
									"write(`~M~JNODE:"+ node_config[k][0] +"~M~J`).\n";
			 out.println(file_str);
		     out.flush();			
			k++;
			if(col != (col_len-1) && k != (nodes))
			System.out.print("----");
		}//for
		System.out.print("\n");
		for(int z=0;z<2;z++){
			for(int h =0; h <= (col-1) ;h++){
			System.out.print("|           ");
			}
			System.out.print("\n");
		}
		
		row++;
		
	}//for	
	
	
}//grid-topology

public static void write_script(String script, PrintWriter out) throws Exception{
		String load_code = "load_config:-\n"+
									"\t\t\tsystem_ip(IP),\n"+
									script+
									"forall(member(X,L),(replaceX(X,Result),exec('pro386w.exe',Result,Y))).\n\n"+
									"replaceX(X,FinalText):- config_file(Config_File), topology_file(Topo_File), portray_clause(\n \t\t(";
									
		InputStream    fis = new FileInputStream("../Config/LoadList.txt");
         BufferedReader br = new BufferedReader(new InputStreamReader(fis, Charset.forName("UTF-8")));
		String to_load = "";
		Vector Load_list = new Vector();
            while ((to_load = br.readLine()) != null) {
				if(!(to_load.equals("")))
				Load_list.add(to_load);
				System.out.println("TO be loaded:"+to_load);
				}//while
			Enumeration load_vector = Load_list.elements();
			String load_str = "";
		while(load_vector.hasMoreElements()){
			//System.out.println("IP:"+ip_vector.nextElement());
			load_str = load_str +"ensure_loaded("+load_vector.nextElement()+"),\n";
		}//while
		load_str = load_str +"consult(Config_File), consult(Topo_File),\n";
		out.println(load_code);
		out.println(load_str);
		out.println("node(concerned_node)\n\t\t	)\n	)~>String,");
		out.println("name(String,ChList),");
		out.println("name(concerned_node,To_be_replaced),");
		out.println("name(X,Xlist),");
		out.println("substitute_all(ChList,To_be_replaced,Xlist,Result),");
		out.println("name(FinalText,Result).\n\n");
		
		out.println("substitute_all(Source, Tag, Content, X) :-");
        out.println("append(Tag, After, TagAndAfter),");
        out.println("append(Before, TagAndAfter, Source),");
        out.println("append(Before, Content, BeforeAndContent),");
        out.println("append(BeforeAndContent, FinalAfter, X),");
        out.println("!,");
        out.println("substitute_all(After, Tag, Content, FinalAfter).");

		out.println("substitute_all(Source, _, _, Source).");

		out.println("nothing.");

		out.println("node_ip(X):- catch(Err,retractall(system_ip(_))),assert(system_ip(X)),!.");
		out.println("\n");
		out.flush();
		
}//write_script


	public static void main(String[] args) throws Exception {
		// TODO Auto-generated method stub
		String ip_input = "";
		Scanner sc = new Scanner(System.in);
	    
	//	System.out.println("Enter the IP addresses of physical nodes [Enter 'q' to quit]");
	//	ip_input = sc.next();
	//	while (!(ip_input.equals("q")) ){
	//		IP.add(ip_input);
	//		ip_input = sc.next();
	//	}
		 InputStream    fis = new FileInputStream("../Config/IPs.txt");
         BufferedReader br = new BufferedReader(new InputStreamReader(fis, Charset.forName("UTF-8")));
		 System.out.println("IP addresseses of physical nodes :");
            while ((ip_input = br.readLine()) != null) {
				IP.add(ip_input);
				//System.out.println(ip_input);
		}
		Enumeration ip_vector = IP.elements();
		while(ip_vector.hasMoreElements()){
			System.out.println("IP:"+ip_vector.nextElement());
		}
		System.out.println("Enter the total number of nodes ");
		nodes = sc.nextInt();
		System.out.println("Total nodes = "+nodes+"|IP Size|"+IP.size());
		//System.out.println("Enter any string to include ");
		//input_str = sc.next();
		
		int moduls = (nodes%IP.size());
		System.out.println("modulas:"+moduls);
		
		int division = (nodes/IP.size());
		System.out.println("Division:"+division);
		
		Enumeration ip_vec = IP.elements();
		String IP_str = null;
		int node_size = 0;
		int remaining_nodes = nodes;
		int remaining_ips = IP.size();
		while(ip_vec.hasMoreElements()){
			// if(moduls>0){
				// int temp = (division+1);
				//System.out.println("temp:"+temp);
				// nodes_per_IP.put(ip_vec.nextElement(),new Integer(temp));
				//System.out.println("For:");
				// moduls--;
			// }
			// else
			// nodes_per_IP.put(ip_vec.nextElement(),new Integer(division));
			remaining_nodes = remaining_nodes - node_size;
			IP_str = (String)ip_vec.nextElement();
			System.out.println("Enter number of nodes for "+IP_str+" out of "+nodes+" currently "+remaining_nodes+" are remaining :");
			System.out.println("Suggestive number of nodes:"+division);
			System.out.println("Number of IPs left :" + remaining_ips);
			node_size = Integer.parseInt(sc.next());
			while(remaining_ips == 1 && node_size<remaining_nodes){
				System.out.println("**[Error!!] Nodes entered is less than the total number of remaining nodes");
				System.out.println("Enter number of nodes for "+IP_str+" out of "+nodes+" currently "+remaining_nodes+" are remaining :");
				System.out.println("Number of IPs left :" + remaining_ips);
				node_size = Integer.parseInt(sc.next());
			}
			nodes_per_IP.put(IP_str,node_size);
			remaining_ips = remaining_ips - 1;
		}
		
		Enumeration number_of_nodes = nodes_per_IP.keys();
		
		while(number_of_nodes.hasMoreElements()){
			String ip = (String)number_of_nodes.nextElement();
			System.out.println("Node IP:"+ip+" | Number of nodes:"+nodes_per_IP.get(ip));
		}
		
		// Creating Topology
		 FileWriter outFile = new FileWriter("../network/network_"+IP.size()+"_"+topo+"_"+nodes+".pl");
		 String net_file = "network_"+IP.size()+"_"+topo+"_"+nodes+".pl";
		 PrintWriter out = new PrintWriter(outFile);
		 
		int counter = 0;
		int platform_port = 25500;
		int taskmanager_port = 50000;
		int enqueue_port =	55000;
		int dequeue_port =	60000;
		String script = "";
		Enumeration ips = IP.elements();
		for(int k=0;k<IP.size();k++){
			String ip = (String)ips.nextElement();
			int node_no = Integer.parseInt(nodes_per_IP.get(ip).toString());
			int temp = counter + node_no;
			String nodes_nm = "";
			for(;counter<temp;){
				node_config[counter][0]="n"+counter;
				node_config[counter][1]=""+ip;
				node_config[counter][2]=""+platform_port;	
				node_config[counter][3]=""+taskmanager_port;
				node_config[counter][4]=""+enqueue_port;
				node_config[counter][5]=""+dequeue_port;
				if(counter == (temp-1))
					nodes_nm = nodes_nm+"n"+counter;
				else
					nodes_nm = nodes_nm+"n"+counter+",";
				counter++;
				platform_port++;
				taskmanager_port++;
				enqueue_port++;
				dequeue_port++;
				
			}
			out.println("node_list"+k+"(["+nodes_nm+"]). \t%"+ ip );
			script = script + "((IP =`"+ip+"`)->node_list"+k+"(L);nothing),\n";
		}
		out.println("\n");
		out.println(":-retractall(config_file(_)),assert(config_file('network\\"+net_file+"')).\n");
		String topology = "topology_"+IP.size()+"_"+topo+"_"+nodes+".pl";
		out.println(":-retractall(topology_file(_)),assert(topology_file('network\\"+topology+"')).\n");
		System.out.println("Node\tIP \tPlatform\tTask");
		FileWriter phero_File = new FileWriter("../network/"+topology);
		PrintWriter phero_out = new PrintWriter(phero_File);
		 phero_out.println("/*"+topo+" Topology */\n\n"+
				 	topo+"_induce_pheros([");
		String node_table = "\nnode_table([\n";
		for(int k=0;k<nodes;k++){
			System.out.println(node_config[k][0]+",\t"+node_config[k][1]+",\t"+node_config[k][2]+",\t"+node_config[k][3]+",\t"+node_config[k][4]+",\t"+node_config[k][5]);
			node_table = node_table + "\n["+node_config[k][0]+",\t`"+node_config[k][1]+"`,\t"+node_config[k][2]+",\t"+node_config[k][3]+",\t"+node_config[k][4]+",\t"+node_config[k][5]+"]";
			phero_out.print("\n[`"+node_config[k][1]+"`,"+node_config[k][3]+"]");
			if(k<(nodes-1)){
				phero_out.print(",");
				node_table = node_table + ",";
				}
			phero_out.flush();
		}
		phero_out.println("\n]).\n\n" +
				topo+"_spawn_agent([\n\n]).\n");
		node_table = node_table + "\n ]).\n\n\n\n";
		
		//write all the ips in the network_ips/1 predicate inside pheros
		Enumeration ip_net = IP.elements();
		String net_ips = "\nnetwork_ips([";
		while(ip_net.hasMoreElements()){
			net_ips = net_ips + "\n[`"+ip_net.nextElement()+"`]";
			if(ip_net.hasMoreElements())
				net_ips = net_ips + ",";
		}
		net_ips = net_ips + "\n\n]).\n";
		
		phero_out.println(net_ips);
		phero_out.println(node_table);
		phero_out.flush();
		phero_out.close();
		//ring_topology(out);
		//star_topology(out);
		//ring_topology_mod(out);
		//binary_tree(out);
		grid_topology(out);
		write_script(script,out);
		out.close();
		//Thread.sleep(1000);
		System.out.println("Enter q to exit");
		ip_input = sc.next();
		while (!(ip_input.equals("q")) ){
		ip_input = sc.next();
		}
	}//main()

}//class()
