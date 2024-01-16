import java.util.*;
import org.jdom.*;
import org.jdom.filter.*;

public class XmlHeight {

    int xmlHeight(Element e) {
	List contents = e.getContent();
	Iterator i = contents.iterator();
	int max = 0;
	while (i.hasNext()) {
	    Object c = i.next();
	    int h;
	    if (c instanceof Element)
		h = xmlHeight((Element)c);
	    else
		h = 1;
	    if (h>max)
		max = h;
	}
	return max+1;
    }

    static void doubleSugar(Document d)
	throws DataConversionException {
	Namespace rcp = Namespace.getNamespace("http://www.brics.dk/ixwt/recipes");
	Filter f = new ElementFilter("ingredient", rcp);
	Iterator i = d.getDescendants(f);
	while (i.hasNext()) {
	    Element e = (Element)i.next();
	    if (e.getAttributeValue("name").equals("sugar")) {
		double amount = e.getAttribute("amount").getDoubleValue();
		e.setAttribute("amount", new Double(2*amount).toString());
	    }
	}
    }
}
