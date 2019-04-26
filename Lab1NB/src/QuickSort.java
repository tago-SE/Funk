public class QuickSort {

    private static void swap(float[] a, int i, int j) {
        float temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    private static int partition(float[] a, int first, int last) {
        float pivot = a[first];
        float temp;
        int up = first;
        int down = last;
        do {
            while (up < last && pivot >= a[up])
                up++;
            while (pivot < a[down])
                down--;
            if (up < down) {
                temp = a[up];
                a[up] = a[down];
                a[down] = temp;
            }
        } while (up < down);
        temp = a[up];
        a[first] = a[down];
        a[down] = temp;
        return down;
    }

    private static void quicksort(float[] a, int first, int last) {
        if (first < last) { // there is data to be sorted
            int pivot = partition(a, first, last);
            quicksort(a, first, pivot - 1); // sort left half
            quicksort(a, pivot + 1, last);  // sort right half
        }
    }

    public static void sort(float[] values) {
        quicksort(values, 0, values.length - 1);
    }
}
