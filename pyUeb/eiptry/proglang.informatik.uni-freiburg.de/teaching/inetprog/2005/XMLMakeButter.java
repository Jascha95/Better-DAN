import java.util.*;
import org.jdom.*;

class XMLMakeButter {

    void makeButter(Element e)
	throws DataConversionException {
	Namespace rcp = Namespace.getNamespace("http://www.brics.dk/ixwt/recipes");
	ListIterator i = e.getChildren().listIterator();
	while (i.hasNext()) {
	    Element c = (Element)i.next();
	    if (c.getName().equals("ingredient")
		&& c.getAttributeValue("name").equals("butter")) {
		Element butter = new Element("ingredient", rcp);
		butter.setAttribute("name", "butter");
		Element salt = new Element("ingredient", rcp);
		salt.setAttribute("name", "salt");
		salt.setAttribute("amount", "*");
		butter.addContent(salt);
		Element cream = new Element("ingredient", rcp);
		cream.setAttribute("name", "cream");
		cream.setAttribute("unit", c.getAttributeValue("unit"));
		double amount = c.getAttribute("amount").getDoubleValue();
		cream.setAttribute("amount", new Double(2*amount).toString());
		butter.addContent(cream);
		Element preparation = new Element("preparation", rcp);
		Element churn = new Element("step", rcp);
		churn.addContent("Add salt and churn until the cream turns to butter.");
		preparation.addContent(churn);
		butter.addContent(preparation);
		i.set(butter);
	    } else
		makeButter(c);
	}
    }
}
