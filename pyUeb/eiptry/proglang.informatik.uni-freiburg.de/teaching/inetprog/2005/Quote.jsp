<html>
<head>
<title>Stock Quotes</title>
</head>
<body>
<%@ page errorPage="errorPage.jsp" %> 
<form action="quote.jsp"
method="GET"> <p>Enter Symbol: <input size="20" name="symbol"><input
type="submit" value="Submit"></p>
</form>
<%if (request.getParameter("symbol") != null) {
%>
<jsp:useBean id="quotes" scope="page" class="com.jguru.Quotes" />
<jsp:setProperty name="quotes" property="*" />
<table border="1">
<tr>
<th align="left">Symbol</th>
<th align="left">Name</th>
<th align="left">Price</th>
</tr>
<tr>
<td><jsp:getProperty name="quotes" property="symbol" /></td>
<td><jsp:getProperty name="quotes" property="name" /></td>
<td><jsp:getProperty name="quotes" property="price" /></td>
</tr>
</table>
<%
}
%>
</body>
</html> 

<p>


