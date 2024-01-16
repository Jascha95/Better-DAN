import java.util.*;
import java.util.Map;
import java.util.HashMap;


public class Multipliziere {
	 public static void main(String[] args) {
	
	Map <String,Integer> apply = new HashMap<>();
		apply.put("maldrei",3);
		apply.put("malvier",4);

	System.out.print(apply.get("maldrei")*5*apply.get("malvier"));


}}