public class QuickSort {

    private static void swap(float[] a, int i, int j) {
        float temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    private static int partitionAsc(float[] a, int first, int last) {
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

    private static int partitionDesc(float[] a, int first, int last) {
        float pivot = a[first];
        int up = first;
        int down = last;
        do {
            while (up < last && pivot <= a[up])
                up++;
            while (pivot > a[down])
                down--;
            if (up < down)
                swap(a, up, down);
        } while (up < down);
        swap(a, first, down);
        return down;
    }

    private static void quicksortAsc(float[] a, int first, int last) {
        if (first < last) { // there is data to be sorted
            int pivot = partitionAsc(a, first, last);
            quicksortAsc(a, first, pivot - 1); // sort left half
            quicksortAsc(a, pivot + 1, last);  // sort right half
        }
    }

    private static void quicksortDesc(float[] a, int first, int last) {
        if (first < last) { // there is data to be sorted
            int pivot = partitionDesc(a, first, last);
            quicksortDesc(a, first, pivot - 1); // sort left half
            quicksortDesc(a, pivot + 1, last);  // sort right half
        }
    }

    public static void sort(float[] values, boolean descendingOrder) {
        if (descendingOrder)
            quicksortDesc(values, 0, values.length - 1);
        else
            quicksortAsc(values, 0, values.length - 1);
    }
}
