
import java.util.Arrays;
import java.util.concurrent.ForkJoinPool;
import static java.util.concurrent.ForkJoinTask.invokeAll;
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
public class ParallelQuickSort extends SortStrategy {

    private ParallelQuickSort() {}
    
    /**
     * Singleton instance
     */
    public static final ParallelQuickSort instance = new ParallelQuickSort();
    
    @Override
    public long sort(float[] a, int cores, int threshold) {
        SortTask.threshold = threshold;
        System.gc();
        long start = System.nanoTime();
        RecursiveAction mainTask = new SortTask(a, 0, a.length - 1);
        ForkJoinPool pool = new ForkJoinPool(cores);
        pool.invoke(mainTask);
        return System.nanoTime() - start;
    }
   
    private static class SortTask extends RecursiveAction {
 
        private static int threshold = 8192;
        private float[] a;
        private int first, last;
        
        
        public SortTask(float[] a, int first, int last) {
            this. a = a;
            this.first = first;
            this.last = last;
        }
        
        private int partition() {
            float pivot = a[first], temp;
            int up = first;
            int down = last;
            do {
                while (up < last && pivot >= a[up]) 
                    up++;
                while (pivot < a[down])             
                    down--;
                if (up < down) {
                    temp = a[up];
                    a[up] = a[down];
                    a[down] = temp;
                }
            } while (up < down);
            temp = a[first];
            a[first] = a[down];
            a[down] = temp;
            return down;
        }
        
        @Override
        protected void compute() {
            if (first < last) {
                if (last - first < threshold) {
                    Arrays.sort(a, first, last + 1);
                } else {
                    int pivot = partition();
                    invokeAll(
                           new SortTask(a, first, pivot - 1), 
                           new SortTask(a, pivot + 1, last)
                       );
                }
            }
        }
    }    
}


