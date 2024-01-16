<%@ page errorPage="ShoppingCartError.jsp" %>
<%@ taglib prefix="s" tagdir="/WEB-INF/tags/shopping" %> 
<%@ taglib prefix="u" tagdir="/WEB-INF/tags/urls" %> 
<html> 
  <head> 
    <title>Widget Inc.</title> 
  </head> 
  <body> 
    <h1>Widget Inc. Online Shopping</h1>                                        
    <s:cart>                                                                    
      <s:add item="${param.item}" amount="${param.amount}"/>                    
      <u:url target="##self">                                                   
        <form method=post action="${url}">                                      
          Item: <input type="text" name="item" size="20" /> <br />
          Amount: <input type="text" name="amount" size="5" /> <br />           
          <input type="submit" name="submit" value="Add to shopping cart" />    
        </form>                                                                 
      </u:url>                                                                  
      <br />                                                                    
      <s:process>                                                               
        <jsp:attribute name="empty">Your shopping cart is empty.</jsp:attribute>
        <jsp:attribute name="noempty">   
          Your shopping cart now contains:<br />                                
          <table border=1><tr><th>Item</th><th>Amount</th></tr>                 
            <s:loop>                                                            
              <tr><td>${item}</td><td>${amount}</td></tr>                       
            </s:loop>                                                           
          </table>                                                              
          <br />                                                                
          <u:url target="ShoppingCartBuy.jsp">                                                  
            <form method=post action="${url}">                                  
              <input type="submit" name="submit" value="Proceed to cashier" />  
            </form>                                                             
          </u:url>                                                              
        </jsp:attribute>                                              
      </s:process>                                                              
    </s:cart>                                                                   
  </body> 
</html> 
