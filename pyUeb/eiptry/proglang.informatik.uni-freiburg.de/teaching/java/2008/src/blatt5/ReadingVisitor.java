import org.htmlparser.Tag;
import org.htmlparser.Text;
import org.htmlparser.visitors.NodeVisitor;

public class ReadingVisitor extends NodeVisitor {
	
	// use the StringBuilder to create Strings efficiently. You have to
	// initialize the object at the right place. Remember that the
	// same visitor could be executed multiple times and you always want a fresh
	// StringBuilder object.
	private StringBuilder m_Builder;
	
	public ReadingVisitor() {
	}

	public void beginParsing() {
	}

	public void visitTag(Tag tag) {
	}

	public void visitEndTag(Tag tag) {
	}

	public void visitStringNode(Text string) {
	}
	
	public void finishedParsing(){
	}
	
	public String toString(){
		if(m_Builder!=null){
			return m_Builder.toString();
		}
		return super.toString();
	}
}
