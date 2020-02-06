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


    /*
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
    */

    private static void swap(float[] b, int i, int j) {
        float temp = b[i];
        b[i] = b[j];
        b[j] = temp;
    }

    private static int partition(float[] a, int first, int last) {
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
        return i + 1;
    }

    private static void quicksort(float[] b, int first, int last) {
        if (first < last) { // there is data to be sorted
            int pivot = partition(b, first, last);
            quicksort(b, first, pivot - 1); // sort left half
            quicksort(b, pivot + 1, last);  // sort right half
        }
    }
    
    @Override
    public long sort(float[] a, int cores, int threshold) {
        System.gc();
        long start = System.nanoTime();
        quicksort(a, 0, a.length - 1);
        return System.nanoTime() - start;
    }
}
