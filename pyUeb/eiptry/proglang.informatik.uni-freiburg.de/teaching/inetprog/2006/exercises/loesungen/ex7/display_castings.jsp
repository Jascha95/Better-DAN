<%@ page import="java.util.*, de.uni_freiburg.informatik.proglang.inetprog2006.ex7.*" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>

<html>
  <head>
    <title>Display Castings</title>
  </head>
  <body>
    <h1>Castings for <i><%= request.getParameter("movie") %></i></h1> 
    <ul>
      <c:forEach varStatus="i" items="${castings}" var="cast">
        <li>
          ${cast.actor}: ${cast.role}
          <c:if test="${cast.leading}">
            <i>(lead role)</i>
          </c:if>
        </li>
      </c:forEach>
    </ul>
  </body>
</body>