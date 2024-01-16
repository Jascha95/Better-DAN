<%@ page import="java.util.*, de.uni_freiburg.informatik.proglang.inetprog2006.*" 
         errorPage="ShoppingCartError.jsp" %>
<html>
  <head>
    <title>Widget Inc.</title>
  </head>
  <body>
    <h1>Widget Inc. Online Shopping</h1> 
    <% Map cart;
       if (session.isNew() || session.getAttribute("cart") == null) {
         cart = new TreeMap();
         session.setAttribute("cart", cart);
       } else {
         cart = (Map)session.getAttribute("cart");
       }
       if (request.getMethod().equals("POST")) {
         String item = request.getParameter("item");
         String amount = request.getParameter("amount");
         if (item!=null) {
    	   addToCart(cart, item, Integer.parseInt(amount));
         }
       }
       String url = request.getRequestURI();
    %>
    <form method=post action="<%= response.encodeURL(url) %>">
      Item: <input type=text name=item size=20>
      Amount: <input type=text name=amount size=5>
      <input type=submit name=submit value="Add to shopping cart">
    </form>
    <p>
    <% if (cart.isEmpty()) { %>
      Your shopping cart is empty.
    <% } else { %>
      Your shopping cart now contains:<p>
      <%= ShoppingCartHelper.formatItems(cart) %>
      <p>
      <a href="<%= response.encodeURL("ShoppingCartBuy.jsp") %>">Proceed to cashier</a>
      </form>
    <% }%>
  </body>
</html>

<%! void addToCart(Map cart, String item, int amount) {
      if (amount<0)
        return;
      Integer a = (Integer)cart.get(item);
      if (a==null) 
        a = new Integer(0);
      cart.put(item, new Integer(a.intValue()+amount));
    }
%>

