
import java.util.Arrays;

public class SerialArraySort extends SortStrategy {
    
    private SerialArraySort() {}
    
    /**
     * Singleton instance
     */
    public static final SerialArraySort instance = new SerialArraySort();
    
    @Override
    public long sort(float[] a, int cores, int threshold) {
        System.gc();
        long start = System.nanoTime();
        Arrays.sort(a);
        return System.nanoTime() - start;
    }
   
}
