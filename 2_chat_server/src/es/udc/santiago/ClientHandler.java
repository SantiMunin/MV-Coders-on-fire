package es.udc.santiago;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import es.udc.santiago.model.Codes;
import es.udc.santiago.model.DataStore;

/**
 * Handles all petitions from one client.
 * 
 * @author Santiago Munín González
 * 
 */
public class ClientHandler extends Thread {

	private Socket sckt;
	private String clientNick;
	private boolean loggedIn;

	public ClientHandler(Socket sckt) {
		super();
		this.sckt = sckt;
	}

	public void run() {
		String message;
		while (true) {
			message = this.readLine(this.sckt);
			if (message == null) {
				System.out.println(this.clientNick + " has disconnected, notifiying other clients.");
				this.disconnect();
				break;
			}
			if (message != null && message.length() > 0) {
				System.out.println("Received: " + message);
				this.processMessage(message, this.sckt);
			}
		}
	}

	/**
	 * 
	 * 
	 * @return a line read from the socket.
	 */
	private String readLine(Socket socket) {
		BufferedReader clientInput;
		try {
			clientInput = new BufferedReader(new InputStreamReader(
					socket.getInputStream()));
			return clientInput.readLine();
		} catch (IOException e) {
			this.disconnect();
			return null;
		}
	}

	/**
	 * 
	 * 
	 * @param clientMessage
	 *            Message to be processed.
	 * @param clientSocket
	 *            Socket which will send the answer.
	 */
	private void processMessage(String clientMessage, Socket clientSocket) {
		String response = "";

		if (clientMessage.charAt(0) == 'N') {
			response = this.nickOperation(clientMessage);
			this.send(response);
			if (response == Codes.OK) {
				this.send(this.userList());
			}
			return;
		}
		// If not logged in, don't check anything else and send error.
		if (!loggedIn) {
			response = Codes.E_NOT_LOGGED_IN;
			this.send(response);
			return;
		}
		if (clientMessage.charAt(0) == 'M') {
			response = this.channelMessage(clientMessage);
		}
		if (clientMessage.charAt(0) == 'L') {
			this.send(this.userList());
			return;
		}
		if (clientMessage.charAt(0) == Codes.PRIVATE_MESSAGE) {
			response = this.privateMessage(clientMessage);
		}
		this.send(response);
	}

	/**
	 * Handle nick operation.
	 * 
	 * @param clientMessage
	 * @return response.
	 */
	private String nickOperation(String clientMessage) {
		if (clientMessage.matches("N:.*:.*")) {
			String[] nicks = clientMessage.substring(2).split(":");
			if (nicks.length != 2) {
				return Codes.E_WRONG_PETITION;
			} else {
				return setNewNick(nicks[0], nicks[1]);
			}
		} else {
			return setNick(clientMessage.substring(2));
		}
	}

	/**
	 * Performs the operation of setting a nick.
	 * 
	 * @param nick
	 * @param clientSocket
	 * @return response
	 */
	private String setNick(String nick) {
		if (!this.correctNick(nick)) {
			return Codes.E_INCORRECT_NICK;
		}
		DataStore dataStore = DataStore.getInstance();
		if (dataStore.addNick(nick, this)) {
			this.loggedIn = true;
			this.clientNick = nick;
			return Codes.OK;
		}
		return Codes.E_DUPLICATE_NICK;
	}

	/**
	 * Changes the nick.
	 * 
	 * @param nick
	 * @param newNick
	 * @return response
	 */
	private String setNewNick(String nick, String newNick) {
		if (!this.correctNick(newNick)) {
			return Codes.E_INCORRECT_NICK;
		}
		if (DataStore.getInstance().changeNick(nick, newNick)) {
			this.clientNick = newNick;
			return Codes.OK;
		} else {
			return Codes.E_DUPLICATE_NICK;
		}
	}

	/**
	 * Checks if it's correct.
	 * 
	 * @param nick
	 * @return
	 */
	private boolean correctNick(String nick) {
		return Pattern.matches("^[a-zA-Z0-9]+$", nick);
	}

	/**
	 * Handles the received message
	 * 
	 * @param clientMessage
	 * @return
	 */
	private String channelMessage(String clientMessage) {
		clientMessage = clientMessage.substring(2);
		this.sendToAll(clientMessage);
		return Codes.OK;
	}

	/**
	 * Sends a message to the user
	 * 
	 * @param message
	 */
	public void send(String message) {
		try {
			String to = this.clientNick;
			if (to == null) {
				to = "anonymous.";
			}  
			System.out.println("Sent: " + message+ " to " + to);
			DataOutputStream output = new DataOutputStream(
					this.sckt.getOutputStream());
			output.writeChars(message + "\n");
		} catch (IOException e) {
			System.out.println("IO problem: " + e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
	}

	/**
	 * Sends a message to all except original message sender.
	 * 
	 * @param nickFrom
	 * @param message
	 */
	private void sendToAll(String message) {
		Iterator<Entry<String, ClientHandler>> it = DataStore.getInstance()
				.getAllClients().entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<String, ClientHandler> pairs = it.next();
			if (!pairs.getKey().equals(this.clientNick)) {
				try {
					pairs.getValue().send(
							"M" + ":" + this.clientNick + ":" + message);
				} catch (Exception e) {
					System.out
							.println("Exception sending a message, maybe user is not connected.");
				}
			}
		}
	}

	/**
	 * 
	 * @return a string with all users connected.
	 */
	private String userList() {
		String userList = String.valueOf(Codes.LIST);
		Iterator<Entry<String, ClientHandler>> it = DataStore.getInstance()
				.getAllClients().entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<String, ClientHandler> pairs = it.next();
			userList += ":" + pairs.getKey();
		}
		return userList;
	}

	/**
	 * Sends a message to a user.
	 * 
	 * @param clientMessage
	 * @return response code.
	 */
	private String privateMessage(String clientMessage) {
		String[] fields = clientMessage.substring(2).split(":");
		if (fields.length != 2) {
			return Codes.E_WRONG_PETITION;
		}
		boolean sent = false;
		Iterator<Entry<String, ClientHandler>> it = DataStore.getInstance()
				.getAllClients().entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<String, ClientHandler> pairs = it.next();
			if (pairs.getKey().equals(fields[0])) {
				pairs.getValue().send(
						Codes.PRIVATE_MESSAGE + ":" + fields[0] + ":"
								+ fields[1]);
				sent = true;
			}
		}
		if (sent) {
			return Codes.OK;
		} else {
			return Codes.E_USER_DOESNT_EXIST;
		}
	}

	/**
	 * Disconnects client
	 */
	public void disconnect() {
		DataStore.getInstance().removeNick(this.clientNick);
		CommunicationsManager.getInstance().notifyAllClients(this.clientNick +" has disconnected.");
			
	}
}
