import java.util.Scanner;

enum TriangleType {
    EQUALATERAL,
    ISOSCELES,
    SCALENE
}

class Triangle{
    double a, b, c;

    public Triangle(double a, double b, double c)
    {
	this.a = a; this.b = b; this.c = c;
    }

    public void ReadTriangle() throws IllegalArgumentException
    {
	Scanner s = new Scanner(System.in);

	System.out.println("Please enter the sides of the triangle:");
	this.a = s.nextDouble();
	this.b = s.nextDouble();
	this.c = s.nextDouble();

	if (!isTriangle())
	    throw new IllegalArgumentException("The entered figure is not a triangle!");
    }
   

    public boolean isTriangle() {
        return this.a > 0 && this.b > 0 && this.c > 0 && 
	       this.a<this.b+this.c && this.b<this.a+this.c && this.c<this.a+this.b;
    }

    public boolean isEqualateral()
    {
	if (this.a == this.b && this.b == this.c)
	    return true;
	return false;
    }

    public boolean isIsosceles()
    {
	if (this.a==this.b || this.b==this.c || this.c==this.a) 
	    return true;
	return false;
    }

    public TriangleType getType()
    {
	if (isEqualateral()) 
	    return TriangleType.EQUALATERAL;
	else if (isIsosceles())
	    return TriangleType.ISOSCELES;
	else
	    return TriangleType.SCALENE;
    }

    public static void printType (TriangleType type)
    {
	if (type==TriangleType.EQUALATERAL) 
	    System.out.println ("The triangle is equalateral.");
	else if (type==TriangleType.ISOSCELES)
	    System.out.println ("The triangle is isosceles.");
	else
	    System.out.println ("The triangle is scalene");
    }

}


class Processing{
    public static void main(String[] args) 
    {
	Triangle t = new Triangle (0,0,0);
	boolean finished = false;
	
	while (!finished) {
	    try {
		t.ReadTriangle();

		Triangle.printType (t.getType());
		
	    } catch (IllegalArgumentException e) {
		System.out.println ("Input invalid. Finishing execution.");

		finished = true;
	    }
	}
	if (t.isEqualateral())
	    System.out.println("The triangle is a equalateral triangle");
    }
}

