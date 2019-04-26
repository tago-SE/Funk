public class MergeSort {

    private static void merge(float[] a, int left, int mid, int right) {
        // Temporary Sub-arrays to be merged
        float a1[] = new float[mid - left + 1];
        float a2[] = new float[right - mid];
        // Fill sub arrays
        System.arraycopy(a, left, a1, 0, a1.length);
        System.arraycopy(a, mid + 1, a2, 0, a2.length);
        // Sort and merge
        int l = 0, r = 0, o = left;
        while (l < a1.length && r < a2.length) {
            if (a1[l] <= a2[r])
                a[o++] = a1[l++];
            else
                a[o++] = a2[r++];
        }
        // Merge remaining
        while (l < a1.length)
            a[o++] = a1[l++];
        while (r < a2.length)
            a[o++] = a2[r++];
    }
    
    private static void mergesort(float[] a, int left, int right) {
        if (left < right) {
            int mid = (left + right)/2;
            mergesort(a, left, mid);
            mergesort(a, mid + 1, right);
            merge(a, left, mid, right);
        }
    }

    public static void sort(float[] values) {
        mergesort(values, 0, values.length - 1);
    }

}
