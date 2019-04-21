public class QuickSort {

    private static void swap(float[] a, int i, int j) {
        float temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    private static void quicksort(float[] a, int first, int last) {
        if (first < last) { // there is data to be sorted
            int pivot = partition(a, first, last);
            quicksort(a, first, pivot - 1); // sort left half
            quicksort(a, pivot + 1, last);  // sort right half
        }
    }

    private static int partition(float[] a, int first, int last) {
        float pivot = a[first];
        int up = first;
        int down = last;
        do {
            while (up < last && pivot >= a[up])
                up++;
            while (pivot < a[down])
                down--;
            if (up < down)
                swap(a, up, down);
        } while (up < down);
        swap(a, first, down);
        return down;
    }

    public static void sort(float[] a) {
        quicksort(a, 0, a.length - 1); // first, last
    }
}
