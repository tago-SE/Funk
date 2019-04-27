/**
 * Note: a[] is an array inherited from the SortManager class
 */
public class SerialMergeSort extends SortStrategy {

    private SerialMergeSort() {}
    
    /**
     * Singleton instance
     */
    public static final SerialMergeSort instance = new SerialMergeSort();
    
    
    public static void merge(float[] b, int left, int mid, int right) {
        // Temporary Sub-arrays to be merged
        int n1 = mid - left + 1;
        int n2 = right - mid;
        float a1[] = new float[n1];
        float a2[] = new float[n2];
        // Fill sub arrays
        for (int i = 0; i< n1; ++i) 
            a1[i] = b[left + i]; 
        for (int j = 0; j < n2; ++j) 
            a2[j] = b[mid + 1 + j]; 
        // Sort and merge
        int l = 0, r = 0, o = left;
        while (l < a1.length && r < a2.length) {
            if (a1[l] <= a2[r])
                b[o++] = a1[l++];
            else
                b[o++] = a2[r++];
        }
        // Merge remaining
        while (l < a1.length)
            b[o++] = a1[l++];
        while (r < a2.length)
            b[o++] = a2[r++];
    }
    
    public static void mergesort(float[] b, int left, int right) {
        if (left < right) {
            int mid = (left + right)/2;
            mergesort(b, left, mid);
            mergesort(b, mid + 1, right);
            merge(b, left, mid, right);
        }
    }

    @Override
    public long sort(float[] a) {
        System.gc();
        long start = System.nanoTime();
        mergesort(a, 0, a.length - 1);
        return System.nanoTime() - start;
    }

}
