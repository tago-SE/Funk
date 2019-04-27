/**
 * This is a singleton class used for benchmarking serial QuickSort.
 * 
 * Note: a[] is an array inherited from the SortManager class
 */
public class SerialQuickSort extends SortStrategy {
    
    private SerialQuickSort() {}
    
    /**
     * Singleton instance
     */
    public static final SerialQuickSort instance = new SerialQuickSort();
    
    
    private static void quicksort(float[] b, int first, int last) {
        if (first < last) { // there is data to be sorted
            int pivot = partition(b, first, last);
            quicksort(b, first, pivot - 1); // sort left half
            quicksort(b, pivot + 1, last);  // sort right half
        }
    }
    
    private static int partition(float[] b, int first, int last) {
        float pivot = b[first], temp;
        int up = first;
        int down = last;
        do {
            while (up < last && pivot >= b[up]) up++;
            while (pivot < b[down])             down--;
            if (up < down) {
                temp = b[up];
                b[up] = b[down];
                b[down] = temp;
            }
        } while (up < down);
        temp = b[first];
        b[first] = b[down];
        b[down] = temp;
        return down;
    }
    
    @Override
    public long sort(float[] a) {
        System.gc();
        long start = System.nanoTime();
        quicksort(a, 0, a.length - 1);
        return System.nanoTime() - start;
    }
}
