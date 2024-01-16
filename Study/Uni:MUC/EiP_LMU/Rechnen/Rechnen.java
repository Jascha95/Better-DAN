public class Rechnen {
public static void main( String [] args ){
		
		final int RADIUS = 3 ;
		
		final int HOEHE = 10 ;
		
		double kreisFlaeche = Math.PI * 2 * RADIUS;

			   
				
		
		double kegelInhalt = 1/3 * kreisFlaeche * HOEHE;
		
		double mantelLinie = Math.sqrt(RADIUS * RADIUS + Math.pow((double) HOEHE,2));
		
		double winkel = Math.asin(RADIUS/mantelLinie);
		

		System.out.println("Kegelinhalt= " + kegelInhalt);
		System.out.println("Winkel Kegelspitze="+winkel);
}}



