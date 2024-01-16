/* A stack with a fixed maximum capacity */
public class Stack<X> {
    
    int topIx;            // index in content of the top element
    final X[] content;    // array that stores the elements of the stack
    
    public Stack(int capacity) {
        this.content = (X[]) new Object[capacity];
        this.topIx = -1;
    }
    
    public X top() {
        return this.content[this.topIx];
    }
    
    public X pop() {
        X res = this.content[this.topIx];
        this.topIx--;
        return res;       
    }
    
    public void push(X x) {
        this.topIx++;
        this.content[this.topIx] = x;
    }
    
    public boolean isEmpty() {
        return this.topIx == -1;
    }
    
    public boolean isFull() {
        return this.topIx == (this.content.length - 1);
    }

    public static void main(String[] args) {
        // some tests
        Stack<String> stack = new Stack<String>(2);
        System.out.println(stack.isEmpty());
        System.out.println(stack.isFull());
        stack.push("1");
        stack.push("2");
        System.out.println(stack.isEmpty());
        System.out.println(stack.isFull());
        System.out.println(stack.pop());
        stack.push("3");
        System.out.println(stack.pop());
        System.out.println(stack.pop());
        System.out.println(stack.isEmpty());
        System.out.println(stack.isFull());
        stack.push("4");
        System.out.println(stack.pop());       
    }
}
