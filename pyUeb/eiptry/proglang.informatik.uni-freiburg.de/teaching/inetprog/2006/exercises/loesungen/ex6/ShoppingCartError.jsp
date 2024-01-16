<%@ page isErrorPage="true" %>

<html>
  <head>
    <title>Fehler</title>
  </head>
  <body>
    <h1>Fehler</h1>
    <p>Leider ist bei der Bearbeitung ein Fehler aufgetreten:
       <code><%= exception %></code>
       <pre><% StackTraceElement[] es = exception.getStackTrace();
               for (int i = 0; i < es.length; i++) {
                   out.println(es[i]);
               }
            %></pre>
    </p>
  </body>
</html>