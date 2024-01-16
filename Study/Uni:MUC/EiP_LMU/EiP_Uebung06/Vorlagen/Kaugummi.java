/*
 * Vorlage EiP-Übung, Lehrstuhl TCS, LMU München.
 */
 
 
class Mod3Automaton { 
    /* Hilfs-Klassen, welche nicht public sind, können zur benutzenden Klasse in die gleiche Datei hineingeschrieben werden. 
    * Wem dies irritiert, kann Mod3Automaton in eine eigene Datei auslagern.
    */

    /* TODO */

    public Mod3Automaton()          { /* TODO */ }

    public void read ( /* TODO */ ) { /* TODO */ }

    public boolean accepting()      { return false; /* TODO */ }

}


public class Kaugummi {

    public static void test(String input) {

        Mod3Automaton automaton = new Mod3Automaton();
        boolean error = false;
        for (int i=0; i<input.length(); i++) {
            char c = input.charAt(i);
            if      (c == '1') automaton.read( /* TODO */ );
            else if (c == '2') automaton.read( /* TODO */ );
            else if (c == '5') automaton.read( /* TODO */ );
            else {
                System.out.println("Illegal character: " + c);
                error = true;
            }
        } 
        if (error) System.out.println("Faulty input.");
        else if (automaton.accepting()) 
             System.out.println("Digit sum is divisible by 3.");
        else System.out.println("Digit sum is not divisible by 3.");
    } 

    public static void main(String[] args) {
        for (String s : args) {
            System.out.println("Running automaton on input " + s);
            test(s);
        }
    }
}
