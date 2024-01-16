package de.uni_freiburg.informatik.proglang.inetprog.j2ee;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.naming.Context;

import proglang.j2ee.ejbs.AuthorValue;
import proglang.j2ee.ejbs.BookManager;
import proglang.j2ee.ejbs.BookManagerUtil;
import proglang.j2ee.ejbs.BookValue;
import proglang.j2ee.ejbs.CategoryValue;

/* Invoke as
 * java -classpath .:/usr/local/workshop/bea90/weblogic90/server/lib/weblogic.jar:../BookstoreEJB/BookstoreEJB.jar de.uni_freiburg.informatik.proglang.inetprog.j2ee.BookManagerClient
 */
public class BookManagerClient {

    private static BufferedReader stdin = 
        new BufferedReader(new InputStreamReader(System.in));
    
    public static String prompt(String msg) throws IOException {
        System.out.print(msg );
        String s = stdin.readLine();
        return s.trim();
    }
    
    public static int promptForInt(String msg) throws IOException {
        String s = prompt(msg);
        return new Integer(s);
    }
    
    public static void main(String[] args) throws Exception {
        System.setProperty(Context.INITIAL_CONTEXT_FACTORY, 
                           "weblogic.jndi.WLInitialContextFactory");
        System.setProperty(Context.PROVIDER_URL, "t3://nonopapa:7001");
        
        BookManager bookManager = BookManagerUtil.getHome().create();
        String title = prompt("Enter title: ");
        int price = promptForInt("Enter price: ");
        String author = prompt("Enter author: ");
        BookValue book = new BookValue(null, title, "", price);
        book.addAuthor(new AuthorValue(null, author));
        book.addCategory(new CategoryValue (null, "Informatik", null));
        bookManager.addBook(book);
    }
}
