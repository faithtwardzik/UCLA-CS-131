/* Homework 3: Java Shared Memory Performance Races
   Grade Received: 100%
   If you found this file helpful, please consider reaching out to me:
   Website: faithtwardzik.com
   Instagram: @faithtwardzik
*/

class SynchronizedState implements State {
    private long[] value;

    SynchronizedState(int length) { value = new long[length]; }

    public int size() { return value.length; }

    public long[] current() { return value; }

    public synchronized void swap(int i, int j) {
	value[i]--;
	value[j]++;
    }
}
