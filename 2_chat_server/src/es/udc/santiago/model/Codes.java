package es.udc.santiago.model;
/**
 * Keeps anwser codes.
 * @author Santiago Munín Gonzáez
 *
 */
public abstract class Codes {
	public static final Character NICK = 'N';
	public static final Character MESSAGE= 'M';
	public static final Character PRIVATE_MESSAGE= 'P';
	public static final Character LIST= 'L';
	public static final String E_DUPLICATE_NICK = "E01";
	public static final String E_INCORRECT_NICK = "E02";
	public static final String E_NOT_LOGGED_IN = "E03";
	public static final String E_USER_DOESNT_EXIST = "E04";
	public static final String E_WRONG_PETITION = "E99";
	public static final String OK = "OK";
	public static final String DISCONNECTED = "DC";
}