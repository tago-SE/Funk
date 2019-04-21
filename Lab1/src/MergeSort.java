public class MergeSort {

    private static void mergeAsc(float[] left, float[] right, float[] out) {
        int l = 0, r = 0, o = 0;
        while (l < left.length && r < right.length) {
            if (left[l] < right[r])
                out[o++] = left[l++];
            else
                out[o++] = right[r++];
        }
        while (l <left.length)
            out[o++] = left[l++];
        while (r <right.length)
            out[o++] = right[r++];
    }

    private static void mergesortAsc(float[] a) {
        if (a.length == 1)
            return;
        float[] left = new float[a.length/2];
        float[] right = new float[a.length/2 + a.length % 2];
        System.arraycopy(a, 0, left, 0, left.length);
        System.arraycopy(a, left.length, right, 0, right.length);
        mergesortAsc(left);
        mergesortAsc(right);
        mergeAsc(left, right, a);
    }

    private static void mergeDesc(float[] left, float[] right, float[] out) {
        int l = 0, r = 0, o = 0;
        while (l < left.length && r < right.length) {
            if (left[l] < right[r])
                out[o++] = left[l++];
            else
                out[o++] = right[r++];
        }
        while (l <left.length)
            out[o++] = left[l++];
        while (r <right.length)
            out[o++] = right[r++];
    }

    private static void mergesortDesc(float[] a) {
        if (a.length == 1)
            return;
        float[] left = new float[a.length/2];
        float[] right = new float[a.length/2 + a.length % 2];
        System.arraycopy(a, 0, left, 0, left.length);
        System.arraycopy(a, left.length, right, 0, right.length);
        mergesortDesc(left);
        mergesortDesc(right);
        mergeDesc(left, right, a);
    }

    public static void sort(float[] values, boolean descendingOrder) {
        if (descendingOrder)
            mergesortDesc(values);
        else
            mergesortAsc(values);
    }

}
