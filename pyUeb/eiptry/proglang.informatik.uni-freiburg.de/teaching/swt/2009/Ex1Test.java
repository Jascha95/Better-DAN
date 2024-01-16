import org.junit.*;
import static org.junit.Assert.*;
import java.util.*;

public class Ex1Test {
    @Test
    public void test_find_min_1() {
	int[] a = {5, 1, 7};
	int res = Ex1.find_min(a);
	assertTrue(res == 1);
    }
    
    @Test
    public void test_insert_1() {
	int[] x = {2, 7};
	int n = 6;
	int[] res = Ex1.insert(x, n);
	int[] expected = {2, 6, 7};
	assertTrue(Array.equals(expected, res));
    }
}
