
import java.util.concurrent.RecursiveAction;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author tiago
 */
public class ParallelQuickSort extends SortStrategy {

    private ParallelQuickSort() {}
    
    /**
     * Singleton instance
     */
    public static final ParallelQuickSort instance = new ParallelQuickSort();
    
    
    private static class SortTask extends RecursiveAction {

        @Override
        protected void compute() {
            
        }
        
    }
    
}
