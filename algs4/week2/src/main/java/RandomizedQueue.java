import java.util.Iterator;
import java.util.NoSuchElementException;
import edu.princeton.cs.algs4.StdRandom;

public class RandomizedQueue<Item> implements Iterable<Item> {

    private Deque<Item> deque;
    // construct an empty randomized queue
    public RandomizedQueue() {
        deque = new Deque<Item>();
    }

    // is the randomized queue empty?
    public boolean isEmpty() {
        return deque.isEmpty();
    }

    // return the number of items on the randomized queue
    public int size() {
        return deque.size();
    }

    // add the item
    public void enqueue(Item item) {
        if (StdRandom.uniform(2) == 0) {
            deque.addFirst(item);
        } else {
            deque.addLast(item);
        }
    }

    // remove and return a random item
    public Item dequeue() {
        if (StdRandom.uniform(2) == 0) {
            return deque.removeFirst();
        } else {
            return deque.removeLast();
        }
    }

    // return a random item (but do not remove it)
    public Item sample() {
        Item item = dequeue();
        enqueue(item);
        return item;
    }

    private final class RandomizedIterator implements Iterator<Item> {
        private Item[] arr;
        private int id;

        public RandomizedIterator(Deque<Item> d) {
            arr = (Item[]) new Object[d.size()];
            id = d.size() - 1;
            
            for (int i = 0; i < arr.length; i++) {
                Item el = d.removeFirst();
                arr[i] = el;
            }
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

    // return an independent iterator over items in random order
    public Iterator<Item> iterator() {
        Deque<Item> oldDeque = deque;
        deque = new Deque<Item>();

        for (Item el : oldDeque) {
            enqueue(el);
        }
            
        return new RandomizedIterator(oldDeque);
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
