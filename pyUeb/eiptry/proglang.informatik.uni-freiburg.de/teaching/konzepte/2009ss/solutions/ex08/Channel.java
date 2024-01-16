
public class Channel<T> {
    
    private static class Stream<T> {
        private MVar<Item<T>> mvar;
        Stream() {
            this.mvar = new MVar<Item<T>>();
        }
    }
    
    private static class Item<T> {
        private T first;
        private Stream<T> rest;
        Item(T first, Stream<T> rest) {
            this.first = first;
            this.rest = rest;
        }
    }
    
    MVar<Stream<T>> readEnd;
    MVar<Stream<T>> writeEnd;
    
    public Channel() {
        this.readEnd = new MVar<Stream<T>>();
        this.writeEnd = new MVar<Stream<T>>();
        Stream<T> hole = new Stream<T>();
        this.readEnd.put(hole);
        this.writeEnd.put(hole);
    }
    
    void put(T val) {
        Stream<T> newHole = new Stream<T>();
        Stream<T> oldHole = writeEnd.take();
        writeEnd.put(newHole);
        oldHole.mvar.put(new Item<T>(val, newHole));
    }
    
    T take() {
        Stream<T> s = this.readEnd.take();
        Item<T> item = s.mvar.take();
        this.readEnd.put(item.rest);
        return item.first;
    }
}
