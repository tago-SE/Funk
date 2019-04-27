
import java.util.Arrays;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveAction;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class ParallelMergeSort extends SortStrategy {

    private ParallelMergeSort() {}
    
    /**
     * Singleton instance
     */
    public static final ParallelMergeSort instance = new ParallelMergeSort();
    
    @Override
    public long sort(float[] a) {
        System.gc();
        long start = System.nanoTime();
        RecursiveAction mainTask = new SortTask(a, 0, a.length - 1);
        ForkJoinPool pool = new ForkJoinPool(cores);
        pool.invoke(mainTask);
        return System.nanoTime() - start;
    }
   
    private static class SortTask extends RecursiveAction {
 
        private final int THRESHOLD = 1;
        
        private float[] a;
        private int left, right;
        
        SortTask(float[] a, int left, int right) {
            this.a = a;
            this.left = left;
            this.right = right;
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
                if (right - left <= THRESHOLD) {
                    Arrays.sort(a, left, right);
                } else {
                    int mid = (left + right)/2;
                    invokeAll(
                        new SortTask(a, left, mid), 
                        new SortTask(a, mid + 1, right)
                    );
                    merge(mid);
                }
            }
        }
    }
}



