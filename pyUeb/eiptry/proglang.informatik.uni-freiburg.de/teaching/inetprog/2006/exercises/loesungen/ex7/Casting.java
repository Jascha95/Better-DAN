package de.uni_freiburg.informatik.proglang.inetprog2006.ex7;

public class Casting {

    String actor;
    String role;
    boolean leading;
    
    public Casting(String actor, String role, boolean leading) {
        this.actor = actor;
        this.role = role;
        this.leading = leading;
    }

    public String getActor() {
        return actor;
    }

    public boolean isLeading() {
        return leading;
    }

    public String getRole() {
        return role;
    }

    public String toString() {
        return "Casting(" + actor + ", " + role + ", " + leading + ")";
    }
}
