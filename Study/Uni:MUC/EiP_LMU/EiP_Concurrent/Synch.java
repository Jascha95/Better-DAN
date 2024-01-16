public class Synch extends Thread {
  int val;
  boolean condition = true;
 
   public void run() {
     if (condition)
       while (condition) {
	   if (val%2==0) condition = condition&&val%2==0;
	   val=val+1;
       }
   }
 
   public static void main(String[] args) throws Exception {
     Synch s=new Synch();
     s.start();
     Thread.sleep(10);
     for(int i=0;i<100;i++) s.condition=false;
     System.out.println(s.val);
     System.out.println(s.val);
   }
}
