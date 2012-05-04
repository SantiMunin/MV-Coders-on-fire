package es.udc.santiago;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import es.udc.santiago.model.DataStore;

/**
 * Provides functions to communicate with clients. Implements singleton.
 * 
 * @author Santiago Munín González
 * 
 */
public class CommunicationsManager {

	private static final int PORT = 9000;
	private static CommunicationsManager instance;
	private ServerSocket serverSocket;

	private CommunicationsManager() {
		try {
			this.serverSocket = new ServerSocket(PORT);
		} catch (IOException e) {
			System.out.println("Port "+PORT+" is being use, please pick another one. Abort.");
			System.exit(-1);
		}
	}

	public static CommunicationsManager getInstance() {
		if (instance == null) {
			instance = new CommunicationsManager();
		}
		return instance;
	}

	/**
	 * Starts the server.
	 */
	public void listen() {
		while (true) {
			Socket clientSocket;
			try {
				clientSocket = serverSocket.accept();
				ClientHandler mH = new ClientHandler(clientSocket);
				mH.start();
			} catch (IOException e) {
				System.out.println("IO problem: " + e.getMessage());
			}
		}
	}

	/**
	 * Send the same message to all clients.
	 * 
	 * @param message
	 *            message content.
	 */
	public void notifyAllClients(String message) {
		Iterator<Entry<String, ClientHandler>> it = DataStore.getInstance()
				.getAllClients().entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<String, ClientHandler> pairs = it.next();
				pairs.getValue().send(message);
		}
	}

	public static void main(String[] args) {
		System.out.println("Starting server...");
		CommunicationsManager cm = CommunicationsManager.getInstance();
		cm.listen();
		System.out.println("Finished");
	}

}
