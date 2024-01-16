package de.uni_freiburg.informatik.proglang.inetprog2006.ex7;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class MovieModel {

    private Connection conn;
    
    public MovieModel() throws SQLException, ClassNotFoundException {
        Class.forName("org.hsqldb.jdbcDriver");
        conn = DriverManager.getConnection("jdbc:hsqldb:/home/wehr/workspace/InetProg2006Servlets/moviedb",
                                           "sa",                     // username
                                            "");                      // password
    }
  
    private static String escape(String s) {
        return s.replaceAll("'", "''");
    }
    
    public List<String> getMovies() throws SQLException {
        String query = "SELECT name FROM movie";
        Statement st = conn.createStatement();
        ResultSet rs = st.executeQuery(query);
        List<String> result = new ArrayList<String>();
        while (rs.next()) {
            result.add(rs.getString("name"));
        }
        st.close();
        return result;
    }
    
    public List<Casting> searchCasting(String movie) throws SQLException {
        String query = "SELECT actor.name, casting.role, casting.is_leading " +
                       "FROM movie, actor, casting " +
                       "WHERE movie.name = '" + escape(movie) + "' " +
                       "AND casting.movie = movie.id AND casting.actor = actor.id";
        Statement st = conn.createStatement();
        ResultSet rs = st.executeQuery(query);
        List<Casting> result = new ArrayList<Casting>();
        while (rs.next()) {
            String actor = rs.getString("name");
            String role = rs.getString("role");
            boolean leading = rs.getBoolean("is_leading");
            Casting c = new Casting(actor, role, leading);
            result.add(c);
        }
        st.close();
        return result;
    }
    
    public static void main(String[] args) throws Exception {
        String movie = args[0];
        MovieModel model = new MovieModel();
        List<Casting> l = model.searchCasting(movie);
        for (Casting c : l) {
            System.out.println(c);
        }
    }
}
