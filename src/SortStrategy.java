
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;




public class SortStrategy {
    
    
    /**
     * Populates a random array of floats with a given size and range starting 
     * at 0.0 and ending at 0.0 + range.
     **/
    public static float[] randomArray(int size, int range) {
        float[] a = new float[size];
          for (int j = 0; j < size; j++) {
            a[j] = (float) (Math.random() % range * range);
        }
        return a;
    }

    public static float[] trunc(float[] a, int size) {
        float[] b = new float[size];
        System.arraycopy(a, 0, b, 0, size);
        return b;
    }

    public static int getE(int size) {
        int count = 0;
        for (int i = size; i > 1; i /=10) {
            count++;
        }
        return count;
    }

    public static void writeToFile(String filename, float[] a)  {
        try {
            FileOutputStream fos = new FileOutputStream(filename);
            DataOutputStream dos = new DataOutputStream(fos);
            for (int i = 0; i < a.length; i++) {
                dos.writeFloat(a[i]); 
            }
            dos.close();
        } catch (FileNotFoundException ex) {
            Logger.getLogger(SortStrategy.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(SortStrategy.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public static float[] readFromFile(String filename, int size) {
        float[] a = new float[size];
        try {
            FileInputStream fin = new FileInputStream(filename);
            DataInputStream din = new DataInputStream(fin);
            for (int i = 0; i < size; i++) {
                a[i] = din.readFloat();
            }
            din.close();
        } catch (FileNotFoundException ex) {
            Logger.getLogger(SortStrategy.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IOException ex) {
            Logger.getLogger(SortStrategy.class.getName()).log(Level.SEVERE, null, ex);
        }
        return a;
    }
    
    public void print(float[] a) {
        for (int i = 0; i < a.length; i++) {
            System.out.println(a[i]);
        }
    }
    
    public boolean isSorted(float[] a) {
        for (int i = 0; i < a.length - 1; i++) {
            if (a[i + 1] < a[i])
                return false;
        }
        return true;
    }
    
    public long sort(float[] a, int cores, int threshold) {
        throw new IllegalStateException("Not implemented.");
    }
}
