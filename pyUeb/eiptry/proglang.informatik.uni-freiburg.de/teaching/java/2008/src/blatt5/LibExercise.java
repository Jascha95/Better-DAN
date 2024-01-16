import java.util.Map;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.htmlparser.Parser;
import org.htmlparser.visitors.NodeVisitor;

/**
 * This is our main class. We will perform every operation from here.
 *
 */
public class LibExercise {

	private static final String s_LOG4JPATTERN = "%m%n";

	private static Logger s_Logger = Logger.getRootLogger();

	private static String s_INPUT_FILENAME = "table.html";
	
	private static String s_OUTPUT_FILENAME = "table_out.html";

	/**
	 * This is our main method.  
	 * This method ... 
	 * - initializes the logging subsystem
	 * - measures the execution time of the whole program
	 * - catches all fatal exceptions that occur during the program execution
	 * - <complete the comment with the things you do in this method>
	 * 
	 * @param args We ignore all parameters.
	 */
	public static void main(String[] args) {
		//begin measuring execution time
		long start = System.currentTimeMillis();

		//define look & feel of output in layout
		PatternLayout layout = new PatternLayout(s_LOG4JPATTERN);

		// create an appender for console output and attach layout to it
		ConsoleAppender consoleAppender = new ConsoleAppender(layout);

		// attach the appender to the log4j subsystem
		s_Logger.addAppender(consoleAppender);

		//print success message 
		s_Logger.info("Ich wurde richtig eingebunden!");

		//set logging level to OFF - this means logging is disabled
		s_Logger.setLevel(Level.OFF);

		try {
			//you should run your visitor from here 
			//runVisitor(s_FILENAME,<your Visitor here>);

		} catch (Exception e) {
			//if something goes wrong, we print the stack trace via the logging subsystem 
			s_Logger.fatal("An exception occured!", e);
		}

		//print total execution time if the right logging level is set
		s_Logger.debug("The program took "
				+ (System.currentTimeMillis() - start) + "ms to execute!");
	}

	/**
	 * This method creates a HTML parser object that will convert any given HTMl
	 * file to a tree structure. It then traverses the tree structure in
	 * depth-first order. While doing so it executes the methods of a
	 * NodeVisitor on every node encountered in the traversal.
	 * 
	 * @param url
	 *            The resource String. May be a valid URL or a valid filename
	 *            containing a HTML file.
	 * @param visitor
	 *            A NodeVisitor object containing methods to read and/or
	 *            manipulate the HTML tree structure.
	 * @throws Exception
	 *             The parser throws exception if the given resource String is
	 *             invalid or the resource contains illegal characters
	 */
	public void runVisitor(String url, NodeVisitor visitor) throws Exception {
		Parser parser = new Parser(url);
		parser.visitAllNodesWith(visitor);
	}

	/**
	 * This method converts a raw type map to a human readable String
	 * representation. It relies on the toString() methods of the keys and
	 * values contained in the map. Every key-value-pair is listed in a separate
	 * line (except if the value itself contains line breaks).
	 * 
	 * The SupressWarnings("unchecked") attribute was inserted to prevent a
	 * compiler warning for using the raw type Map.
	 * 
	 * @param map
	 *            The object we want to convert to a String.
	 * @return The resulting String ready for output.
	 */
	@SuppressWarnings("unchecked")
	public static String convertMapToString(Map map) {
		String newLine = "\n";
		String separator = ":";
		String leftBrace = "[";
		String rightBrace = "]";
		StringBuilder builder = new StringBuilder();

		for (Object o : map.keySet()) {
			builder.append(leftBrace).append(o).append(rightBrace).append(
					separator).append(map.get(o).toString()).append(newLine);
		}
		return builder.toString();
	}
}
