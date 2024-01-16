import draw.*;
import colors.*;
import geometry.*;

class Test1 {
  Canvas c = new Canvas(500,500);
  Test1() {
     c.show();
     c.drawString(new Posn(10,50),"Hallo Welt!");
  }  
}

class Test2 {
  Canvas c = new Canvas(500,500);
  Test2() {
    c.show();
    c.drawCircle(new Posn(250,250),100,new Red());
  }  
}

class Test3 {
  Canvas c = new Canvas(500,500);
  Test3() {
    c.show();
    c.drawRect(new Posn(100,100),100,25,new Red());    
  }  
}

class Test4 {
  Canvas c = new Canvas(500,500);
  Test4() {
    c.show();
    c.drawDisk(new Posn(100,100),25,new Red());    
  }  
}