
class Zustand{
    int a=0;
    int b=0;
    int r1=0;
    int r2=0;
}

class Thread1 extends Thread{
    Thread1(Zustand z){
	this.z=z;
    }
    Zustand z;
    public void run(){
	try{sleep(10);}catch(Exception e){}	

	int r1=z.b;
	for(int i=0;i<1000000;i++)r1=i+z.b-i;
	//try{sleep(1);}catch(Exception e){}
	z.a=1;z.r1=r1;
    }
}
class Thread2 extends Thread{
    Thread2(Zustand z){
	this.z=z;
    }
    Zustand z;
    public void run(){
	try{sleep(10);}catch(Exception e){}	
	int r2=z.a;
	for(int i=0;i<1000000;i++)r2=i+z.a-i;

	z.b=2;z.r2=r2;
    }
}
public class Funny{
    public static void main(String[] args){
	Zustand z = new Zustand();
	
	    while(!(z.r1==2 && z.r2==1)){
		Thread1 th1 = new Thread1(z);
		Thread2 th2 = new Thread2(z);	    
		z.a=0;z.b=0;
		th2.start();		th1.start();

		try {th1.join();th2.join();} catch(InterruptedException e){}
		//	  
	    }  System.out.println("r1 = " + z.r1 + ". r2 = " + z.r2);	    
    }
}
	    
	    

	    
    
