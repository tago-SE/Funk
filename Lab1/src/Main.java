public class Main {
    public static void main(String[] args) {

        int n = 100;

        float f[] = new float[n];
        for (int i = 0; i < n; i++) {
            f[i] = (float) (Math.random()%n*n);
        }

       // MergeSort.sort(f, true);
        QuickSort.sort(f);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < n; i++) {
            sb.append(f[i]).append("\n");
        }
        System.out.println(sb.toString());

        for (int i = 1; i < 100; i++) {
            if (f[i - 1] > f[i]){
                System.out.println("ERROR: " + i);
                break;
            }
        }

    }
}
