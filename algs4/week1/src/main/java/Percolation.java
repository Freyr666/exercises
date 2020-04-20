import java.util.Arrays;
import java.util.Optional;
import edu.princeton.cs.algs4.WeightedQuickUnionUF;

public class Percolation {

    WeightedQuickUnionUF conns;
    boolean opened[];
    int openedNum;
    int virtualTop;
    int virtualBottom;
    int size;
    
    public Percolation(int n) {
        if (n <= 0)
            throw new IllegalArgumentException();

        size = n;
        virtualTop = n * n;
        virtualBottom = n * n + 1;

        opened = new boolean[n*n];
        openedNum = 0;
        Arrays.fill(opened, false);

        conns = new WeightedQuickUnionUF(n * n + 2);
    }

    boolean opened(int id) {
        if (id == virtualTop || id == virtualBottom)
            return true;
        else
            return opened[id];
    }

    int upper(int id) {
        int res = id - size;
        if (res < 0)
            return virtualTop;
        else
            return res;
    }

    int lower(int id) {
        int res = id + size;
        if (res > size * size)
            return virtualBottom;
        else
            return res;
    }

    Optional<Integer> left(int id) {
        if (id % size == 0)
            return Optional.empty();
        else
            return Optional.of(id - 1);
    }

    Optional<Integer> right(int id) {
        if (id % size == size - 1)
            return Optional.empty();
        else
            return Optional.of(id + 1);
    }
    
    public void open(int row, int col) {
        if (row <= 0
            || row > size
            || col <= 0
            || col > size)
            throw new IllegalArgumentException();

        int id = (row - 1) * size + (col - 1);
        if (opened[id])
            return;

        openedNum++;
        opened[id] = true;

        int upper = upper(id);
        if (opened(upper))
            conns.union(id, upper);

        int lower = lower(id);
        if (opened(lower))
            conns.union(id, lower);

        Optional<Integer> left = left(id);
        if (left.isPresent()) {
            int l = left.get();
            if (opened(l))
                conns.union(id, l);
        }

        Optional<Integer> right = right(id);
        if (right.isPresent()) {
            int r = right.get();
            if (opened(r))
                conns.union(id, r);
        }
                     
    }

    public boolean isOpen(int row, int col) {
        if (row <= 0
            || row > size
            || col <= 0
            || col > size)
            throw new IllegalArgumentException();

        int id = (row - 1) * size + (col - 1);
        return opened(id);
    }

    public boolean isFull(int row, int col) {
        if (row <= 0
            || row > size
            || col <= 0
            || col > size)
            throw new IllegalArgumentException();

        int id = (row - 1) * size + (col - 1);
        return conns.find(id) == conns.find(virtualTop);
    }

    // returns the number of open sites
    public int numberOfOpenSites() {
        return openedNum;
    }

    // does the system percolate?
    public boolean percolates() {
        return conns.find(virtualTop) == conns.find(virtualBottom);
    }

    public static void main(String[] args) {
        System.out.println("Test 1");
        Percolation p1 = new Percolation(5);
        for(int i = 1; i <= 5; i++)
            p1.open(i, 3);
        System.out.println("Result: " + p1.percolates());

        System.out.println("Test 2");
        Percolation p2 = new Percolation(5);
        for(int i = 1; i <= 4; i++)
            p2.open(i, 3);
        System.out.println("Result: " + p2.percolates());
    }
    
}
