<%@ page import="java.util.*, de.uni_freiburg.informatik.proglang.inetprog2006.ex7.*" %>
<%@ taglib uri="http://java.sun.com/jsp/jstl/core" prefix="c" %>
<%@ taglib prefix="u" tagdir="/WEB-INF/tags/urls" %> 

<html>
  <head>
    <title>All Movies</title>
  </head>
  <body>
    <h1>All Movies</i></h1> 
    <ul>
      <c:forEach varStatus="i" items="${movies}" var="movie">
        <u:url target="display.do?movie=${movie}">
          <li><a href="${url}">${movie}</a></li>  
        </u:url>
      </c:forEach>
    </ul>
  </body>
</body>