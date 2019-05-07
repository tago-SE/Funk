
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
        System.gc();
        long start = System.nanoTime();
        RecursiveAction mainTask = new SortTask(a, 0, a.length - 1);
        SortTask.threshold = threshold;
        ForkJoinPool pool = new ForkJoinPool(cores);
        pool.invoke(mainTask);
        return System.nanoTime() - start;
    }
   
    private static class SortTask extends RecursiveAction {
 
        private static int threshold;
        private float[] a;
        private int first, last;
        
        
        public SortTask(float[] a, int first, int last) {
            this. a = a;
            this.first = first;
            this.last = last;
        }

        @Override
        protected void compute() {
            if (first < last) {
                if ((last - first) < threshold) {
                    Arrays.sort(a, first, last + 1);
                } else {
                    float pivot = a[last];
                    int i = (first - 1); // index of smaller element
                    for (int j = first; j<last; j++) {
                        if (a[j] <= pivot) {
                            i++;
                            float temp = a[i];
                            a[i] = a[j];
                            a[j] = temp;
                        }
                    }
                    float temp = a[i+1];
                    a[i + 1] = a[last];
                    a[last] = temp;
                    int part = i + 1;
                    invokeAll(
                            new SortTask(a, first, part - 1),
                            new SortTask(a, part + 1, last)
                    );
                }
            }
        }
    }    
}


