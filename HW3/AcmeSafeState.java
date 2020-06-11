/* Homework 3: Java Shared Memory Performance Races
   Grade Received: 100%
   If you found this file helpful, please consider reaching out to me:
   Website: faithtwardzik.com
   Instagram: @faithtwardzik
*/

import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
    private AtomicLongArray arr; 
    
    AcmeSafeState(int length) {
	arr = new AtomicLongArray(length);
    }

    public int size() { return arr.length(); }

    public long[] current() {
	int arr_length = arr.length();
	long[] value = new long[arr_length];
	for (int i = 0; i < arr_length; i++) {
	    value[i] = arr.get(i);
	}
	return value;
    }

    public void swap(int i, int j) {
	
	arr.decrementAndGet(i);
	arr.incrementAndGet(j);
	
    }
}

