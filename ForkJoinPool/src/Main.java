
import com.sun.xml.internal.ws.policy.privateutil.PolicyUtils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
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
public class Main {
    
  
    
    private static int PARALLEL_MERGESORT = 4;
    private static int PARALLEL_QUICKSORT = 5;
    
    private static SortStrategy[] STRATEGIES = {
        SerialQuickSort.instance,
        SerialMergeSort.instance,
        SerialArraySort.instance,
        ParallelSort.instance,
        ParallelMergeSort.instance,
        ParallelQuickSort.instance
    };

    private static int RANGE                        = 1000;
    
    private static String path = "C:/Users/tiago/Desktop/Funk/Lab2Data/";   // Change this
    private static String filename = "data.txt";


    public static void main(String[] args) throws IOException {
        int e = 7;
        int size = (int) Math.pow(10, e);

        int minSize = 10000;

        String file = path + filename;
        float[] a;


        a =  SortStrategy.randomArray(size, RANGE);

        /*
            Hybrid
         */
         //Test.hybrid(path + "hybrid_qs.txt", a, ParallelQuickSort.instance, 2, 4, 10, minSize);

        // Test.hybrid(path + "hybrid_ms.txt", a, ParallelMergeSort.instance, 2 4, 10, minSize);


        /*
            Range
         */
        //Test.range(path + "range_qs.txt", a, ParallelQuickSort.instance, 1, 4, 10, 100000, 5000, minSize);
        //Test.range(path + "range_ms.txt", a, ParallelMergeSort.instance, 1, 4, 10, 100000, 5000, minSize);
        /*
            Test All
         */
       // SortStrategy[] s = {SerialArraySort.instance, SerialQuickSort.instance, SerialMergeSort.instance, ParallelQuickSort.instance, ParallelMergeSort.instance, ParallelSort.instance};
        //Test.strategies(path + "comp.txt", a, s, 1, 4, minSize);


        /*
            Test finalized version
         */
        // Test.single(path + "ms_final.txt", size, RANGE, ParallelMergeSort.instance, 1, 8, 25, 20000);
        // Test.single(path + "qs_final.txt", size, RANGE, ParallelQuickSort.instance, 1, 8, 25, 5000);

        /*
            Finding a reasonable threshold
         */

        Test.allSizes(path + "ms_resonable.txt", size, 100000, RANGE, ParallelMergeSort.instance, 8, 8, 60000, 8, 4000);
        Test.allSizes(path + "qs_resonable.txt", size, 100000, RANGE, ParallelQuickSort.instance, 8, 8, 60000, 8, 4000);

    }
}
