import java.util.Arrays;
import java.util.concurrent.TimeUnit;

public class Main {

    private static final int QUICK_SORT_ASCENDING   = 0;
    //private static final int QUICK_SORT_DESCENDING  = 1;
    private static final int MERGE_SORT_ASCENDING   = 1;
    //private static final int MERGE_SORT_DESCENDING  = 3;
    private static final int SORT                   = 2;
    private static final int PARALLEL_SORT          = 3;

    public static void main(String[] args) {

        int cores = Runtime.getRuntime().availableProcessors();
        System.out.println("Cores: " + cores);

        int n = 3;

        int type = 0;

        int arraySize = 10000000;
        int range = arraySize;
        // Allocate source array
        float[] source = new float[arraySize];
        for (int j = 0; j < arraySize; j++) {
            source[j] = (float) (Math.random() % range * range);
        }

        for (int k = 0; k < 4; k++) {
            System.out.println("Sorting Method: " + k);
            int sum = 0;
            for (int i = 0; i < n; i++) {
                // copy source array
                float f[] = new float[arraySize];
                System.arraycopy(source, 0, f, 0, source.length);

                // Start sorting
                long startTime = System.nanoTime();
                switch (type) {
                    case QUICK_SORT_ASCENDING:
                        QuickSort.sort(f, false);
                        break;
                    case MERGE_SORT_ASCENDING:
                        MergeSort.sort(f, false);
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
                System.out.println("Time (nano): " + elapsed);
                System.out.println("Time (mili): " + elapsedMiliseconds);
            }
        }
    }
}
