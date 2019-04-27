
import java.util.Arrays;

public class ParallelSort extends SortStrategy {
    
    private ParallelSort() {}
    
    /**
     * Singleton instance
     */
    public static final ParallelSort instance = new ParallelSort();
    
    @Override
    public long sort(float[] a) {
        System.gc();
        long start = System.nanoTime();
        Arrays.parallelSort(a);
        return System.nanoTime() - start;
    }
 
}
