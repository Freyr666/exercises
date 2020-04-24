import edu.princeton.cs.algs4.WeightedQuickUnionUF;

public class Percolation {

    private final WeightedQuickUnionUF conns;
    private final boolean[] opened;
    private int openedNum;
    private final int virtualTop;
    private final int virtualBottom;
    private final int size;
    
    public Percolation(int n) {
        if (n <= 0)
            throw new IllegalArgumentException();

        size = n;
        virtualTop = n * n;
        virtualBottom = n * n + 1;

        opened = new boolean[n*n];
        openedNum = 0;

        for (int i = 0; i < n*n; i++)
            opened[i] = false;

        conns = new WeightedQuickUnionUF(n * n + 2);
    }

    private void checkArgs(int row, int col) {
        if (row <= 0
            || row > size
            || col <= 0
            || col > size)
            throw new IllegalArgumentException();
    }

    private int toId(int row, int col) {
        if (row == 0) {
            return virtualTop;
        } else if (row == size + 1) {
            return virtualBottom;
        } else {
            int id = (row - 1) * size + (col - 1);
            return id;
        }
    }
    
    private void connect(int id, int row, int col) {
        if (row < 0
            || row > size + 1
            || col <= 0
            || col > size)
            return;

        int dest = toId(row, col);

        try {
            if (row > 0 && row <= size) {
                if (isOpen(row, col))
                    conns.union(id, dest);
            } else {
                conns.union(id, dest);
            }
        }
        catch (IllegalArgumentException e) {
            return;
        }
    }
    
    public void open(int row, int col) {
        checkArgs(row, col);

        if (isOpen(row, col))
            return;

        int id = toId(row, col);

        openedNum++;
        opened[id] = true;

        connect(id, row - 1, col);
        connect(id, row + 1, col);
        connect(id, row, col - 1);
        connect(id, row, col + 1);
    }

    public boolean isOpen(int row, int col) {
        checkArgs(row, col);

        int id = toId(row, col);
        return id == virtualTop
            || id == virtualBottom
            || opened[id];
    }

    public boolean isFull(int row, int col) {
        checkArgs(row, col);

        int id = toId(row, col);
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
        for (int i = 1; i <= 5; i++)
            p1.open(i, 3);
        System.out.println("Result: " + p1.percolates());

        System.out.println("Test 2");
        Percolation p2 = new Percolation(5);
        for (int i = 1; i <= 4; i++)
            p2.open(i, 3);
        System.out.println("Result: " + p2.percolates());
    }
    
}
