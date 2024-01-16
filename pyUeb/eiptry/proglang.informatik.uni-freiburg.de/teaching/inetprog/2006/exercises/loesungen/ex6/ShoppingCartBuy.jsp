<%@ page import="java.util.*, de.uni_freiburg.informatik.proglang.inetprog2006.*" 
         errorPage="ShoppingCartError.jsp" %>
<% 
  Map cart = (Map)session.getAttribute("cart");
%>
<html>
  <head>
    <title>Widget Inc.</title>
  </head>
  <body>
    <h1>Widget Inc. Online Shopping</h1> 
    <p>Your shopping cart contains:</p>
    <%= ShoppingCartHelper.formatItems(cart) %>
    <p>Zahlungsart:
      <form method=post action="<%= response.encodeURL("ShoppingCartBuyConfirm.jsp") %>">
        <input type="radio" name="payment" value="credit" checked="checked">Credit Card<br>
        <input type="radio" name="payment" value="cash">Cash<br>
        <input type=submit name=submit value="Buy!">
      </form>
    </p>
    <a href="<%= response.encodeURL("ShoppingCartNew.jsp") %>">back</a>
  </body>
</html>