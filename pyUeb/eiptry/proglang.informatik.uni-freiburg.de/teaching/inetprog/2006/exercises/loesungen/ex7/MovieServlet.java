package de.uni_freiburg.informatik.proglang.inetprog2006.ex7;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class MovieServlet extends HttpServlet {
    
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException {
        
        String command = request.getServletPath();
        
        try {
            if (command.equals("/all_movies.do")) {
                MovieModel model = new MovieModel();
                List<String> movies = model.getMovies();
                request.setAttribute("movies", movies);
                getServletContext()
                .getRequestDispatcher("/display_movies.jsp")
                .forward(request, response);            
            } else if (command.equals("/display.do")) {
                String movie = request.getParameter("movie");
                MovieModel model = new MovieModel();
                List<Casting> castings = model.searchCasting(movie);
                request.setAttribute("castings", castings);
                getServletContext()
                  .getRequestDispatcher("/display_castings.jsp")
                  .forward(request, response);
            } else {
                response.setContentType("text/plain");
                response.getWriter().println("Unknown command: " + command);
            }
        } catch (Exception e) {
            response.setContentType("text/plain");
            PrintWriter w = response.getWriter();
            w.println("An exception occurred: " + e);
            StackTraceElement[] es = e.getStackTrace();
            for (int i = 0; i < es.length; i++) {
                w.println(es[i]);
            }                
        }
    }
    
}