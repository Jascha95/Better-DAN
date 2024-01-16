// geometrische Figuren
interface IShape {
}

class SuperImp implements IShape {
  IShape bot;
  IShape top;
  SuperImp(IShape bot, IShape top) {
    this.bot = bot;
    this.top = top;
  }
}

class Dot implements IShape {
  Point loc;
  Dot(Point loc) {
    this.loc = loc;
  }
}

class Circle implements IShape {
  Point loc;
  int radius;
  Circle (Point loc, int radius) {
    this.loc = loc;
    this.radius = radius;
  }
}

class Point {
  int x;
  int y;
  Point(int x, int y) {
    this.x = x; 
    this.y = y;
  }
}

class ShapeExample {
  Dot d1 = new Dot(new Point(10, 20));
  Circle c1 = new Circle(new Point(30,20), 5);
  IShape s1 = d1;
  IShape s2 = c1;
}