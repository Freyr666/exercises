import java.util.Iterator;
import java.util.NoSuchElementException;

public class Deque<Item> implements Iterable<Item> {
    
    private final class Node {
        Node prev;
        Node next;
        Item item;
    }

    private Node first;
    private Node last;
    private int size;
    
    // construct an empty deque
    public Deque() {
        first = null;
        last = null;
        size = 0;
    }

    // is the deque empty?
    public boolean isEmpty() {
        return size == 0;
    }

    // return the number of items on the deque
    public int size() {
        return size;
    }

    // add the item to the front
    public void addFirst(Item item) {
        if (item == null)
            throw new IllegalArgumentException();
        
        Node node = new Node ();
        node.item = item;
        node.prev = null;

        if (first == null && last == null) {
            node.next = null;
            first = node;
            last = node;
        } else {
            node.next = first;
            first.prev = node;
            first = node;
        }            
        
        size++;
    }

    // add the item to the back
    public void addLast(Item item) {
        if (item == null)
            throw new IllegalArgumentException();
        
        Node node = new Node ();
        node.item = item;
        node.next = null;
        
        if (last == null && first == null) {
            node.prev = null;
            first = node;
            last = node;
        } else {
            node.prev = last;
            last.next = node;
            last = node;
        }
        
        size++;
    }

    // remove and return the item from the front
    public Item removeFirst() {
        if (isEmpty())
            throw new NoSuchElementException();
        
        Item result = null;
        if (first != null) {
            result = first.item;
            first = first.next;
            size--;
        }

        if (first == null)
            last = null;
        else
            first.prev = null;
        return result;
    }

    // remove and return the item from the back
    public Item removeLast() {
        if (isEmpty())
            throw new NoSuchElementException();
        
        Item result = null;
        if (last != null) {
            result = last.item;
            last = last.prev;
            size--;
        }

        if (last == null)
            first = null;
        else
            last.next = null;
        return result;
    }

    private final class DequeIterator implements Iterator<Item> {
        Node next;

        public DequeIterator(Node init) {
            next = init;
        }
        
        public boolean hasNext() {
            return next != null;
        }

        public Item next() {
            if (! hasNext())
                throw new NoSuchElementException();
            
            Item item = next.item;
            next = next.next;
            return item;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    // return an iterator over items in order from front to back
    public Iterator<Item> iterator() {
        return new DequeIterator(first);
    }

    // unit testing (required)
    public static void main(String[] args) {
        System.out.println("Creating a deque");
        Deque<String> test = new Deque<String>();
        System.out.println("Test addition");
        test.addFirst("One");
        System.out.println("Test removal");
        if (test.removeLast().equals("One"))
            System.out.println("Removal works fine");
        System.out.println("Test multiple addition");
        test.addFirst("Two");
        test.addLast("Three");
        test.addFirst("One");
        test.addFirst("Zero");
        test.addLast("Four");
        System.out.println("Deque of size " + test.size() + " contains:");
        for (String s : test) {
            System.out.println(s);
        }
        System.out.println("Removing elements...");
        while (test.size() > 0) {
            System.out.println(test.removeLast());
        }
        System.out.println("Deque is now of size "
                           + test.size()
                           + " empty? "
                           + test.isEmpty());
    }

}
