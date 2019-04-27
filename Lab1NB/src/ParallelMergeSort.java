
import java.util.Arrays;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;

public class ParallelMergeSort extends SortStrategy {

    private ParallelMergeSort() {}
    
    public static final ParallelMergeSort instance = new ParallelMergeSort();
    
           
    
    @Override
    public long sort(float[] a, int cores, int threshold) {
        System.gc();
        long start = System.nanoTime();
        RecursiveAction mainTask = new SortTask(a, 0, a.length - 1, threshold);
        ForkJoinPool pool = new ForkJoinPool(cores);
        pool.invoke(mainTask);
        return System.nanoTime() - start;
    }
    
   
   
    private static class SortTask extends RecursiveAction {
 
        private float[] a;
        private int left, right;
        private int threshold;
        
        SortTask(float[] a, int left, int right, int threshold) {
            this.a = a;
            this.left = left;
            this.right = right;
            this.threshold = threshold;
        }
        
        public  void merge(int mid) {
            // Temporary Sub-arrays to be merged
            int n1 = mid - left + 1;
            int n2 = right - mid;
            float a1[] = new float[n1];
            float a2[] = new float[n2];
            // Fill sub arrays
            for (int i = 0; i< n1; ++i) 
                a1[i] = a[left + i]; 
            for (int j = 0; j < n2; ++j) 
                a2[j] = a[mid + 1 + j]; 
            // Sort and merge
            int l = 0, r = 0, o = left;
            while (l < a1.length && r < a2.length) {
                if (a1[l] <= a2[r])
                    a[o++] = a1[l++];
                else
                    a[o++] = a2[r++];
            }
            // Merge remaining
            while (l < a1.length)
                a[o++] = a1[l++];
            while (r < a2.length)
                a[o++] = a2[r++];
        }
        
        @Override
        protected void compute() {
            if (left < right) {
                if (right - left < threshold) {
                    Arrays.sort(a, left, right + 1);
                } else {
                    int mid = (left + right)/2;
                    invokeAll(
                        new SortTask(a, left, mid, threshold), 
                        new SortTask(a, mid + 1, right, threshold)
                    );
                    merge(mid);
                }
            }
        }
    }
}



