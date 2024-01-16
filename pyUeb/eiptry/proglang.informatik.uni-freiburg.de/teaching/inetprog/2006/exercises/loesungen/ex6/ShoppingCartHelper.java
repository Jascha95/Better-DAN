package de.uni_freiburg.informatik.proglang.inetprog2006;

import java.util.Map;
import java.util.Iterator;

public class ShoppingCartHelper {

    public static String formatItems(Map cart) {
        if (cart == null || cart.isEmpty()) {
            return "";
        }
        StringBuffer sb = new StringBuffer();
        sb.append("<table border=1><tr><th>Item</th><th>Amount</th></tr>");
        Iterator i = cart.entrySet().iterator();
        while (i.hasNext()) {
          Map.Entry me = (Map.Entry)i.next();  
          sb.append("<tr><td>" + me.getKey() + 
                    "</td><td align=right>" + me.getValue() + "</td></tr>");
        }
        sb.append("</table>");
        return sb.toString();
    }
}
