
import java.util.Arrays;
import java.util.concurrent.TimeUnit;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class Lab1NB {
    
    private static final int QUICK_SORT_ASCENDING   = 0;
    private static final int MERGE_SORT_ASCENDING   = 1;
    private static final int SORT                   = 2;
    private static final int PARALLEL_SORT          = 3;
    
    // Number of test runs for each sort method
    private static final int N                      = 1;    
    private static final String[] LABEL = {
        "QuickSort", 
        "MergeSort", 
        "ArraySort", 
        "ParallelSort"};
    
    private static int ARR_SIZE                     = 10000;
    private static int RANGE                        = 10000;

    
    public static float[] randomArrayFactory() {
        float[] f = new float[ARR_SIZE];
        for (int j = 0; j < ARR_SIZE; j++) {
            f[j] = (float) (Math.random() % RANGE * RANGE);
        }
        return f;
    }
    
    private static int sum;
    
    public static void main(String[] args) {

        System.gc();
        int cores = Runtime.getRuntime().availableProcessors();
        System.out.println("Cores: " + cores);
      
        int x = 1 << 13;
        System.out.println("X: " + x);
        
       
        for (int type = 0; type < 4; type++) {
            System.out.println("Sorting Method: " + LABEL[type]);
            sum = 0;
            for (int i = 0; i < N; i++) {
                float f[] = randomArrayFactory();
                System.gc();
                long startTime = System.nanoTime();
                switch (type) {
                    case QUICK_SORT_ASCENDING:
                        QuickSort.sort(f);
                        break;
                    case MERGE_SORT_ASCENDING:
                        MergeSort.sort(f); // does not work for 10E8
                        break;
                    case SORT:
                        Arrays.sort(f);
                        break;
                    case PARALLEL_SORT:
                        Arrays.parallelSort(f);
                        break;
                    default:
                        return;
                }
                long elapsed = System.nanoTime() - startTime;
                long elapsedMiliseconds = TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS);
                System.out.println("Average " + LABEL[type] + ": " + elapsed + " -- " + TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS));
                sum += elapsed;
            }
            
        }
       
     
    }
    
}
