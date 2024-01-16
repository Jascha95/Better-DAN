
public class MVar<T> {

    T value;
    Mutex full;
    Mutex empty;
    
    T take() {
        full.mutexWait();
        T x = this.value;
        this.value = null;
        empty.mutexSignal();
        return x;
    }
    
    void put(T val) {
        empty.mutexWait();
        this.value = val;
        full.mutexSignal();
    }
}
