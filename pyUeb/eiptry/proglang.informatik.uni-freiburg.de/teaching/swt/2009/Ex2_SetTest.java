import org.junit.*;
import static org.junit.Assert.*;
import java.util.*;

public class Ex2_SetTest {

    private Ex2_Set<String> s, s2;
  
    @Before
    public void setup() {
	s = new Ex2_Set<String>();
	s.add("one"); s.add("two");
	s2 = new Ex2_Set<String>();
	s2.add("two"); s2.add("three");
    }

    private void testset(String[] exp, Ex2_Set<String> s) {
	assertTrue(s.size() == exp.length);
	for (int i = 0; i < s.size(); i++) {
	    assertTrue(s.member(exp[i]));
	}
    }
    
    @Test
    public void test_union_1() {
	s.union(s2);
	String[] exp = {"one", "two", "three"}
	testset(exp, s);
    }
}
