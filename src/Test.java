import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.TimeUnit;

public class Test {



    private static void work(FileWriter fw, SortStrategy s, float[] a, int minC, int maxC, int t) throws IOException {
        fw.write(t + "\t");
        for (int cores = maxC; cores >= minC; cores /= 2) {
            if (a.length == t && cores != maxC) {

            } else {
                long elapsed = s.sort(a.clone(), cores, t);
                fw.write(TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + "\t");
            }
        }
        fw.write("\n");
    }

    public static void expTen(String filename, float[] a, SortStrategy s, int minC, int maxC, int minSize) {
        FileWriter fw = null;
        try {
            File newTextFile = new File(filename);
            fw = new FileWriter(newTextFile);
            fw.write(s.getClass().getName() + "\n");
            for (int size = a.length; size >= minSize; size /= 10) {
                a = SortStrategy.trunc(a, size);
                System.out.println("size: " + a.length);
                fw.write("size\t" + size + "\nthresh\t");
                for (int cores = maxC; cores >= minC; cores /= 2) {
                    fw.write("C" + cores +"\t");
                }
                fw.write("\n");
                for (int t = 1; t <= size; t *= 10) {
                    work(fw, s, a, minC, maxC, t);
                }
            }
        } catch (Exception e) {

        } finally {
            if (fw != null) try {
                fw.close();
            } catch (IOException e2) {
                e2.printStackTrace();
            }
        }
    }

    public static void linear(String filename, float[] a, SortStrategy s, int minC, int maxC, int k, int minSize) {
        FileWriter fw = null;
        try {
            File newTextFile = new File(filename);
            fw = new FileWriter(newTextFile);
            fw.write(s.getClass().getName() + "\n");
            for (int size = a.length; size >= minSize; size /= 10) {
                a = SortStrategy.trunc(a, size);
                System.out.println("size: " + a.length);
                fw.write("size\t" + size + "\nthresh\t");
                for (int cores = maxC; cores >= minC; cores /= 2) {
                    fw.write("C" + cores +"\t");
                }
                fw.write("\n");
                for (int t = size/k; t <= size; t += size/k - 1) {
                    work(fw, s, a, minC, maxC, t);
                }
            }
        } catch (Exception e) {
        } finally {
            if (fw != null) try {
                fw.close();
            } catch (IOException e2) {
                e2.printStackTrace();
            }
        }
    }

    private static ArrayList<Integer> thresholds = new ArrayList<>();


    public static void hybrid(String filename, float[] a, SortStrategy s, int minC, int maxC, int k, int minSize) {
        FileWriter fw = null;
        try {
            File newTextFile = new File(filename);
            fw = new FileWriter(newTextFile);
            fw.write(s.getClass().getName() + "\n");
            for (int size = a.length; size >= minSize; size /= 10) {
                thresholds.clear();
                a = SortStrategy.trunc(a, size);
                System.out.println("size: " + a.length);
                fw.write("size\t" + size + "\nthresh\t");
                for (int cores = maxC; cores >= minC; cores /= 2) {
                    fw.write("C" + cores +"\t");
                    if (minC != 1)
                        thresholds.add((size - 1 + cores)/cores);
                }
                fw.write("\n");
                // Setup threshold
                for (int t = 1; t < size/10; t *= 10)
                    thresholds.add(t);
                for (int t = size/k; t < size; t += size/k - 1)
                    thresholds.add(t);
                thresholds.add(size);
                Collections.sort(thresholds);

                for (Integer t : thresholds) {
                    fw.write(t + "\t");
                    for (int cores = maxC; cores >= minC; cores /= 2) {
                        long elapsed = s.sort(a.clone(), cores, t);
                        fw.write(TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + "\t");
                    }
                    fw.write("\n");
                }
                fw.write("\n");
                fw.write("\n");
            }
        } catch (Exception e) {
            //
        } finally {
            if (fw != null) try {
                fw.close();
            } catch (IOException e2) {
                e2.printStackTrace();
            }
        }
    }

    public static void range(String filename, float[] a, SortStrategy s, int minC, int maxC, int start, int end, int k, int minSize) {
        FileWriter fw = null;
        try {
            File newTextFile = new File(filename);
            fw = new FileWriter(newTextFile);
            fw.write(s.getClass().getName() + "\n");

            for (int size = a.length; size >= minSize; size /= 10) {
                a = SortStrategy.trunc(a, size);
                System.out.println("size: " + a.length);
                thresholds.clear();
                for (int t = start; t < end; t += k) {
                    thresholds.add(t);
                }
                fw.write("size\t" + a.length + "\nthresh\t");
                for (int cores = maxC; cores >= minC; cores /= 2) {
                    fw.write("C" + cores + "\t");
                }
                fw.write("\n");
                for (Integer t : thresholds) {
                    fw.write(t + "\t");
                    for (int cores = maxC; cores >= minC; cores /= 2) {
                        long elapsed = s.sort(a.clone(), cores, t);
                        fw.write(TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + "\t");
                    }
                    fw.write("\n");
                }
                fw.write("\n");
                fw.write("\n");
            }


        } catch (Exception e) {
            //
        } finally {
            if (fw != null) try {
                fw.close();
            } catch (IOException e2) {
                e2.printStackTrace();
            }
        }
    }

    static void strategies(String filename, float[] a, SortStrategy[] strategies, int minC, int maxC, int minSize) {
        FileWriter fw = null;
        try {
            File newTextFile = new File(filename);
            fw = new FileWriter(newTextFile);
            fw.write("size\tcores\t");
            for (SortStrategy s : strategies) {
                fw.write(s.getClass().getName() + "\t");
            }
            fw.write("\n");
            for (int size = a.length; size >= minSize; size /= 10) {
                a = SortStrategy.trunc(a, size);
                System.out.println("size: " + a.length);
                for (int cores = minC; cores <= maxC; cores++) {
                    System.out.println("Cores: " + cores);
                    fw.write(size + "\t" + cores + "\t");
                    for (SortStrategy s: strategies) {
                        long elapsed = s.sort(a.clone(), cores, (size + cores - 1)/cores);
                        fw.write(TimeUnit.MILLISECONDS.convert(elapsed, TimeUnit.NANOSECONDS) + "\t");
                    }
                    fw.write("\n");
                }
            }
        } catch (Exception e) {
        } finally {
            if (fw != null) try {
                fw.close();
            } catch (IOException e2) {
                e2.printStackTrace();
            }
        }
    }
}
