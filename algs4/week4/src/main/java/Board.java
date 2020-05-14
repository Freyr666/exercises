import java.util.Stack;
import edu.princeton.cs.algs4.StdRandom;

public class Board {

    private final int[][] tiles;
    private final int side;
    
    // create a board from an n-by-n array of tiles,
    // where tiles[row][col] = tile at (row, col)
    public Board(int[][] tiles) {
        assert (tiles.length == tiles[0].length);

        this.side = tiles.length;

        this.tiles = new int[side][side];

        for (int row = 0; row < side; row++)
            for (int col = 0; col < side; col++)
                this.tiles[row][col] = tiles[row][col];
    }
                                           
    // string representation of this board
    public String toString() {
        String res = String.valueOf(side);
        res += "\n";
        for (int row = 0; row < side; row++) {
            for (int col = 0; col < side; col++) {
                res += " " + String.valueOf(tiles[row][col]);
            }
            res += "\n";
        }
        return res;
    }

    // board dimension n
    public int dimension() {
        return side;
    }

    // number of tiles out of place
    public int hamming() {
        int result = 0;
        
        for (int row = 0; row < side; row++) {
            for (int col = 0; col < side; col++) {
                if (tiles[row][col] != row * side + col + 1
                    && tiles[row][col] != 0)
                    result++;
            }
        }
        return result;
    }

    // sum of Manhattan distances between tiles and goal
    public int manhattan() {
        int result = 0;
        
        for (int row = 0; row < side; row++) {
            for (int col = 0; col < side; col++) {
                if (tiles[row][col] != row * side + col + 1
                        && tiles[row][col] != 0) {
                    int rowGoal = (tiles[row][col] - 1) / side;
                    int colGoal = (tiles[row][col] - 1) % side;
                    result += Math.abs(row - rowGoal) + Math.abs(col - colGoal);
                }
            }
        }
        return result;
    }

    // is this board the goal board?
    public boolean isGoal() {
        for (int row = 0; row < side; row++) {
            for (int col = 0; col < side; col++) {
                if (tiles[row][col] != row * side + col + 1
                    && tiles[row][col] != 0)
                    return false;
            }
        }
        return true;
    }

    // does this board equal y?
    public boolean equals(Object y) {
        if (y == this)
            return true;

        if (y == null)
            return false;

        if (y.getClass() != this.getClass())
            return false;

        Board that = (Board) y;

        if (that.side != this.side)
            return false;
        
        for (int row = 0; row < side; row++) {
            for (int col = 0; col < side; col++) {
                if (tiles[row][col] != that.tiles[row][col])
                    return false;
            }
        }
        return true;
    }

    private Board swapAndCreate(int arow, int acol, int brow, int bcol) {
        if (arow < 0 || arow >= side
            || acol < 0 || acol >= side
            || brow < 0 || brow >= side
            || bcol < 0 || bcol >= side)
            return null;

        int tmp = tiles[arow][acol];
        tiles[arow][acol] = tiles[brow][bcol];
        tiles[brow][bcol] = tmp;

        Board res = new Board(tiles);

        tiles[brow][bcol] = tiles[arow][acol];
        tiles[arow][acol] = tmp;

        return res;
    }
    
    // all neighboring boards
    public Iterable<Board> neighbors() {
        Stack<Board> result = new Stack<Board>();

        int zeroRow = 0;
        int zeroCol = 0;
        
        for (int row = 0; row < side; row++)
            for (int col = 0; col < side; col++)
                if (tiles[row][col] == 0) {
                    zeroRow = row;
                    zeroCol = col;
                }

        Board top = swapAndCreate(zeroRow, zeroCol, zeroRow - 1, zeroCol);
        if (top != null)
            result.push(top);
        Board bottom = swapAndCreate(zeroRow, zeroCol, zeroRow + 1, zeroCol);
        if (bottom != null)
            result.push(bottom);
        Board left = swapAndCreate(zeroRow, zeroCol, zeroRow, zeroCol - 1);
        if (left != null)
            result.push(left);
        Board right = swapAndCreate(zeroRow, zeroCol, zeroRow, zeroCol + 1);
        if (right != null)
            result.push(right);
        
        return result;
    }

    // a board that is obtained by exchanging any pair of tiles
    public Board twin() {
        int rowZero = 0;
        int colZero = 0;

        for (int row = 0; row < side; row++)
            for (int col = 0; col < side; col++)
                if (tiles[row][col] == 0) {
                    rowZero = row;
                    colZero = col;
                    break;
                }

        int rowA = (rowZero + 1) % side;
        int colA = colZero;
        int rowB = rowZero;
        int colB = (colZero + 1) % side;
        return swapAndCreate(rowA, colA, rowB, colB);
    }

    // unit testing (not graded)
    public static void main(String[] args) {
        int n = 3;
        int [] data = new int[n*n];
        for (int i = 0; i < n*n; i++)
            data[i] = i;
        StdRandom.shuffle(data);

        int[][] tiles = new int[n][n];
        for (int row = 0; row < n; row++)
            for (int col = 0; col < n; col++)
                tiles[row][col] = data[row * n + col];

        Board initial = new Board(tiles);
        System.out.print(initial);

        for (Board b : initial.neighbors()) {
            System.out.println("Neighbour: ");
            System.out.print(b);
            System.out.println("Manhattan: " + b.manhattan());
            System.out.println("Hamming: " + b.hamming());
            System.out.println("Dims: " + b.dimension());
        }
    }
    
}
