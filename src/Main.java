
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
    
    private static void lab1_MergeSort_treshold()  {
        FileWriter fw = null;
        try {
            File newTextFile = new File(path + filename);
            fw = new FileWriter(newTextFile);


            SortStrategy[] s = {SerialArraySort.instance, SerialMergeSort.instance, SerialArraySort.instance};
            SortStrategy sorter = s[0];
            fw.write("Algo\tSize\tns\tms\tcores\tthreshold\n");
            for (int algo = 0; algo < s.length; algo++) {
                sorter = s[algo];
                System.out.println(sorter.getClass().getName());
                fw.write(sorter.getClass().getName() + "\n");
                for (int j = 0; j < 4; j++) {
                    System.out.println("Run: " + j);
                    fw.write("Run: " + j);
                    for (int i = 5; i <= 8; i++) {
                        int size = (int) Math.pow(10, i);
                        float[] a = SortStrategy.randomArray(size, RANGE);
                        System.out.println("size: 10E" + i);
                        for (int cores = 1; cores <= 1; cores *= 2) {
                            System.out.println("Core: " + cores);
                            for (int tresh = 2500; tresh <= 60000; tresh += 2500) {
                                // Generates a randomized array of floats and saves it to a file
                                //SortStrategy.writeToFile(SortStrategy.randomArray(size, RANGE));
                                System.gc();
                                float[] b = new float[a.length];
                                System.arraycopy(a, 0, b, 0, a.length);
                                long elapsed = sorter.sort(b, cores, tresh);
                                fw.write(/*algo + */ "E" + i + "\t" + elapsed + "\t" +
                                        TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + "\t" + cores + "\t" + tresh + "\n");
                            }
                        }
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (fw != null) { try {
                    fw.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    private static void parallelMergeSortVsArraySort()  {
        FileWriter fw = null;
        try {
            File newTextFile = new File(path + filename);
            fw = new FileWriter(newTextFile);
            SortStrategy sorter = ParallelMergeSort.instance;
            int size = (int) Math.pow(10, 8);
            float[] a = sorter.randomArray(size, RANGE);
            fw.write("Parallel Mergesort - " + RANGE + "\n");
            for (int cores = 2; cores <= 4; cores *= 2) {
                for (int t = 10000000; t <= size; t += 5000000) {
                    System.out.println("threshold: " + t + " cores: " + cores);
                    long elapsed = sorter.sort(a.clone(),  cores, t);
                    System.out.println("elapsed = " + elapsed);
                    fw.write("E" + 8 + "\t" + elapsed + "\t" +
                            TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + "\t" + cores +"\t" + t + "\n");
                }
            }
            sorter = SerialArraySort.instance;
            fw.write("ArraySort - " + RANGE + "\n");
            long elapsed = sorter.sort(a.clone(), 8, 0);
            fw.write("E" + 8 + "\t" + elapsed + "\t" +
                    TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + "\t" + 1 + "\t" + 0 + "\n");
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (fw != null) {
                try {
                    fw.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
    
    public static void main(String[] args) throws IOException {

        parallelMergeSortVsArraySort();


        System.out.println("Done");
    }
}
