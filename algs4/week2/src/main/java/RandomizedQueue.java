import java.util.Iterator;
import java.util.NoSuchElementException;
import edu.princeton.cs.algs4.StdRandom;

public class RandomizedQueue<Item> implements Iterable<Item> {

    private Item[] array;
    private int N;
    
    // construct an empty randomized queue
    public RandomizedQueue() {
        array = (Item[]) new Object[10];
        N = 0;
    }

    // is the randomized queue empty?
    public boolean isEmpty() {
        return N == 0;
    }

    // return the number of items on the randomized queue
    public int size() {
        return N;
    }

    private void resize(int newSize) {
        Item[] newArray = (Item[]) new Object[newSize];
        for (int i = 0; i < N; i++)
            newArray[i] = array[i];
        array = newArray;
    }

    // add the item
    public void enqueue(Item item) {
        if (item == null)
            throw new IllegalArgumentException();

        if (N == array.length)
            resize(array.length * 2);

        N++;
        int index = StdRandom.uniform(N);
        array[N - 1] = array[index];
        array[index] = item;
    }

    // remove and return a random item
    public Item dequeue() {
        if (isEmpty())
            throw new NoSuchElementException();

        Item result = array[--N];
        array[N] = null;

        if (N < array.length / 2)
            resize(array.length / 2);

        return result;
    }

    // return a random item (but do not remove it)
    public Item sample() {
        if (isEmpty())
            throw new NoSuchElementException();
        
        return array[StdRandom.uniform(N)];
    }

    private final class RandomizedIterator implements Iterator<Item> {
        private Item[] arr;
        private int id;

        public RandomizedIterator(Item[] a) {
            arr = a;
            id = arr.length - 1;
        }

        public boolean hasNext() {
            if (id < 0)
                return false;
            else
                return true;
        }
        
        public Item next() {
            if (! hasNext())
                throw new NoSuchElementException();
            
            Item item = arr[id];
            arr[id--] = null;
            return item;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    private static void shuffle(Object[] a) {
        if (a.length < 2)
            return;
        
        for (int i = 1; i < a.length; i++) {
            int r = StdRandom.uniform(i);
            Object tmp = a[r];
            a[r] = a[i];
            a[i] = tmp;
        }
    }

    // return an independent iterator over items in random order
    public Iterator<Item> iterator() {
        Item[] newArray = (Item[]) new Object[N];

        for (int i = 0; i < N; i++)
            newArray[i] = array[i];

        shuffle(newArray);
            
        return new RandomizedIterator(newArray);
    }

    // unit testing (required)
    public static void main(String[] args) {
        System.out.println("+ Creating a queue +");
        RandomizedQueue<String> queue = new RandomizedQueue<String>();
        queue.enqueue("Zero");
        queue.enqueue("One");
        queue.enqueue("Two");
        queue.enqueue("Three");
        queue.enqueue("Four");
        System.out.println("+ Printing random 1 +");
        for (String el : queue)
            System.out.println(el);
        System.out.println("+ Printing random 2 +");
        for (String el : queue)
            System.out.println(el);
        System.out.println("+ Random sample: " + queue.sample() + " +");
        System.out.println("+ Removing elements +");
        while (! queue.isEmpty())
            System.out.println(queue.dequeue());
        System.out.println("+ Queue size is "
                           + queue.size()
                           + " empty? "
                           + queue.isEmpty()
                           + " +");
    }

}
