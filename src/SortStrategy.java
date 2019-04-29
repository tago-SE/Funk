
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;




public class SortStrategy {

    private static final String FILE_NAME = "rdm_arr.txt";
    
    
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
    
    public static float[] copyArray(float[] srcs) {
        float[] copy = new float[srcs.length];
        System.arraycopy(srcs, 0, copy, 0, srcs.length);
        return copy;
    }

    public static void writeToFile(float[] a)  {
        try {
            FileOutputStream fos = new FileOutputStream(FILE_NAME);
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
    
    public static float[] readFromFile(int size) {
        float[] a = new float[size];
        try {
            FileInputStream fin = new FileInputStream(FILE_NAME);
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
