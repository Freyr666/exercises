import java.util.ArrayDeque;
import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.MinPQ;

public class Solver {

    private class Node implements Comparable<Node> {
        private final int priority;
        private final int moves;
        private final Board board;
        private final Node previous;

        public Node(Board board, Node prev, int moves) {
            this.board = board;
            this.previous = prev;
            this.moves = moves;
            this.priority = board.manhattan() + moves;
        }

        public int compareTo(Node that) {
            return Integer.compare(priority, that.priority);
        }
    }

    private Node node = null;

    // find a solution to the initial board (using the A* algorithm)
    public Solver(Board initial) {
        if (initial == null)
            throw new IllegalArgumentException();

        int moves = 0;
        MinPQ<Node> queue = new MinPQ<Node>();
        queue.insert(new Node(initial, null, moves));

        while (true) {
            node = queue.delMin();

            if (node == null || node.board.isGoal())
                break;

            moves++;
            for (Board neighbour : node.board.neighbors()) {
                if (node.previous == null
                    || !neighbour.equals(node.previous.board))
                    queue.insert(new Node(neighbour, node, moves));
            }
        }
    }

    // is the initial board solvable? (see below)
    public boolean isSolvable() {
        return (node != null);
    }

    // min number of moves to solve initial board
    public int moves() {
        if (node == null)
            return Integer.MAX_VALUE;
        else
            return node.moves;
    }

    // sequence of boards in a shortest solution
    public Iterable<Board> solution() {
        ArrayDeque<Board> result = new ArrayDeque<Board>();

        Node point = node;
        while (point != null) {
            result.push(point.board);
            point = point.previous;
        }
        return result;
    }

    // test client (see below) 
    public static void main(String[] args) {
        // create initial board from file
        In in = new In(args[0]);
        int n = in.readInt();
        int[][] tiles = new int [n][n];
        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
                tiles[i][j] = in.readInt();
        Board initial = new Board(tiles);

        // solve the puzzle
        Solver solver = new Solver(initial);

        // print solution to standard output
        if (!solver.isSolvable())
            StdOut.println("No solution possible");
        else {
            StdOut.println("Minimum number of moves = " + solver.moves());
            for (Board board : solver.solution())
                StdOut.println(board);
        }
    }

}
