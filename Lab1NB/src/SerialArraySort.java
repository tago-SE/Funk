
import java.util.Arrays;

public class SerialArraySort extends SortStrategy {
    
    private SerialArraySort() {}
    
    /**
     * Singleton instance
     */
    public static final SerialArraySort instance = new SerialArraySort();
    
    @Override
    public long sort(float[] a) {
        System.gc();
        long start = System.nanoTime();
        Arrays.sort(a);
        return System.nanoTime() - start;
    }
   
}
