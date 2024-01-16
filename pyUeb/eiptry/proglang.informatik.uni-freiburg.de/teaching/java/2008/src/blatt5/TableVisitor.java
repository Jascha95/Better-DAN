import java.util.Map;
import org.apache.log4j.Logger;
import org.htmlparser.Tag;
import org.htmlparser.Text;
import org.htmlparser.visitors.NodeVisitor;

public class TableVisitor extends NodeVisitor {

	private Logger m_Log;
	
	//you have to initialize this object somewhere
	private Map<Object,Object> m_Container;
	
	//use this object to represent the current state of your visitor
	private State m_State;
	
	//use some or all of this states to distinguish the different states of your visitor
	private enum State {
		DATA, TABLE, FIRSTELEMENT, ENTRY, EXIT, REJECT
	};
	
	public TableVisitor(Logger log) {
		m_Log = log;
		m_State = State.ENTRY;
	}

	public void beginParsing() {
	}

	public void visitTag(Tag tag) {
	}

	public void visitEndTag(Tag tag) {
	}

	public void visitStringNode(Text string) {
	}

	public void finishedParsing() {
	}

	/**
	 * This method should be used to return the created Map object to the main
	 * method of LibExercise. 
	 * 
	 * @return The created Map object. May be null.
	 */
	public Map<Object, Object> getMap() {
		return this.m_Container;
	}

}
