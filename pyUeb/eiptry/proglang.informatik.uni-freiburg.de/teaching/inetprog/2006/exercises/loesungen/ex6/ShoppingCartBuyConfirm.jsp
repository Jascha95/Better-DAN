<%@ page import="java.util.*, de.uni_freiburg.informatik.proglang.inetprog2006.*" 
         errorPage="ShoppingCartError.jsp" %>
<% 
  Map cart = (Map)session.getAttribute("cart");
  session.invalidate();
%>
<html>
  <head>
    <title>Widget Inc.</title>
  </head>
  <body>
    <h1>Widget Inc. Online Shopping</h1> 
    <p>You bought the following items by <%= request.getParameter("payment") %>:
     <%= ShoppingCartHelper.formatItems(cart) %>
    </p>
   </body>
 </html>