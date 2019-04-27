
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
    
  
    
    
    private static SortStrategy[] STRATEGIES = {
        SerialQuickSort.instance,
        SerialMergeSort.instance,
        SerialArraySort.instance,
        ParallelSort.instance,
        ParallelMergeSort.instance,
        ParallelQuickSort.instance
    };
    
    private static int E                            = 2;
    private static int RANGE                        = 10000;
    private static final int K                      = 1; // Number of repeat attempts
    
    private static final int NUM_SORT_TYPES         = 5;
    
    public static void main(String[] args) {

        int maxCores = Runtime.getRuntime().availableProcessors();
        int size = (int) Math.pow(10, E);
        long elapsed;
       
        System.out.println("X" + (1 << 13));
        
        System.out.println("Max cores: " + maxCores);
        System.out.println("Array size [" + E + "] = " + size);
        System.out.println("Range = " + RANGE);
      
        // Generates a randomized array of floats and saves it to a file
        //SortStrategy.writeToFile(SortStrategy.randomArray(size, RANGE));
       
        
        float[] a;
        
        SortStrategy strategy = STRATEGIES[1];
         a = SortStrategy.randomArray(size, RANGE);
         
       
       
        strategy.sort(a);
        strategy.print(a);
        System.out.println("sorted = " + strategy.isSorted(a));
       
        a = SortStrategy.randomArray(size, RANGE);
        strategy = STRATEGIES[4];
        strategy.sort(a);
        strategy.print(a);
        System.out.println("sorted = " + strategy.isSorted(a));
          /*
        strategy = STRATEGIES[5];
        strategy.sort(a);
        strategy.print(a);
        System.out.println("sorted = " + strategy.isSorted(a));
        */
        /*

        for (int type = 0; type < NUM_SORT_TYPES; type++) {
            SortStrategy strategy = STRATEGIES[type];
            strategy.setCores(2);
            System.out.println(strategy.getClass().getName());
            for (int i = 0; i < K; i++) {
                //elapsed = strategy.sort(SortStrategy.randomArray(size, RANGE));
               // if (type == 3)
                //    continue;
                float[] a = SortStrategy.randomArray(size, RANGE);
                elapsed = strategy.sort(a);
                System.out.println(elapsed + " -- " + TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + " with " + strategy.cores + " cores.");
                strategy.print(a);
                System.out.println("sorted = " + strategy.isSorted(a));
            }
        } 
        */
    }
}
