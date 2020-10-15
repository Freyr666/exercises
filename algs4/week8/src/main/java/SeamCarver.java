import edu.princeton.cs.algs4.AcyclicSP;
import edu.princeton.cs.algs4.DirectedEdge;
import edu.princeton.cs.algs4.EdgeWeightedDigraph;
import edu.princeton.cs.algs4.Picture;
import java.awt.Color;

public class SeamCarver {

    private Picture picture;

    private static void require(boolean exp) {
        if (!exp)
            throw new IllegalArgumentException();
    }
    
    // create a seam carver object based on the given picture
    public SeamCarver(Picture picture) {
        require(picture != null);
        
        this.picture = picture;
    }

    // current picture
    public Picture picture() {
        return picture;
    }

    // width of current picture
    public int width() {
        return picture.width();
    }

    // height of current picture
    public int height() {
        return picture.height();
    }

    private boolean borderPixel(int x, int y) {
        return x == 0 || x == width() - 1 || y == 0 || y == height() - 1;
    }

    private static double sqr(int v) {
        return (double) (v * v);
    }
    
    // energy of pixel at column x and row y
    public double energy(int x, int y) {
        require(x >= 0 && x < width());
        require(y >= 0 && y < height());

        if (borderPixel(x, y))
            return 1000.0;
        
        Color cnx = picture.get(x - 1, y);
        Color cpx = picture.get(x + 1, y);
        Color cny = picture.get(x, y - 1);
        Color cpy = picture.get(x, y + 1);

        int rx = cnx.getRed() - cpx.getRed();
        int gx = cnx.getGreen() - cpx.getGreen();
        int bx = cnx.getBlue() - cpx.getBlue();

        int ry = cny.getRed() - cpy.getRed();
        int gy = cny.getGreen() - cpy.getGreen();
        int by = cny.getBlue() - cpy.getBlue();

        double deltaX = sqr(rx) + sqr(gx) + sqr(bx);
        double deltaY = sqr(ry) + sqr(gy) + sqr(by);

        return Math.sqrt(deltaX + deltaY);
    }

    // sequence of indices for vertical seam
    public int[] findVerticalSeam() {
        EdgeWeightedDigraph eg = new EdgeWeightedDigraph(width() * height() + 2);
        int top = width() * height();
        int bottom = width() * height() + 1;

        for (int y = 0; y < height() - 1; y++)
            for (int x = 0; x < width(); x++)
                for (int otherX = x == 0 ? 0 : x - 1;
                     otherX <= (x == width() - 1 ? width() - 1 : x + 1);
                     otherX++) {
                    int self = y * width() + x;
                    int other = (y + 1) * width() + otherX;
                    double energy = energy(otherX, y + 1);
                    DirectedEdge edge = new DirectedEdge(self, other, energy);
                    eg.addEdge(edge);
                }

        for (int x = 0; x < width(); x++) {
            int bottomOff = (height() - 1) * width();
            DirectedEdge topEdge = new DirectedEdge(top, x, energy(x, 0));
            DirectedEdge botEdge = new DirectedEdge(x + bottomOff, bottom, energy(x, height() - 1));
            eg.addEdge(topEdge);
            eg.addEdge(botEdge);
        }

        AcyclicSP sp = new AcyclicSP(eg, top);

        int[] res = new int[height()];
        int pos = 0;
        for (DirectedEdge edge : sp.pathTo(bottom)) {
            if (edge.from() == top)
                continue;
            res[pos++] = edge.from() % width();
        }

        return res;
    }

    // sequence of indices for horizontal seam
    public int[] findHorizontalSeam() {
        EdgeWeightedDigraph eg = new EdgeWeightedDigraph(width() * height() + 2);
        int left = width() * height();
        int right = width() * height() + 1;

        for (int x = 0; x < width() - 1; x++)
            for (int y = 0; y < height(); y++)
                for (int otherY = (y == 0 ? 0 : y - 1);
                     otherY <= (y == height() - 1 ? height() - 1 : y + 1);
                     otherY++) {
                    int self = x * height() + y;
                    int other = (x + 1) * height() + otherY;
                    double energy = energy(x + 1, otherY);
                    DirectedEdge edge = new DirectedEdge(self, other, energy);
                    eg.addEdge(edge);
                }

        for (int y = 0; y < height(); y++) {
            int rightOff = (width() - 1) * height();
            DirectedEdge leftEdge = new DirectedEdge(left, y, energy(0, y));
            DirectedEdge rightEdge = new DirectedEdge(y + rightOff, right, energy(width() - 1, y));
            eg.addEdge(leftEdge);
            eg.addEdge(rightEdge);
        }

        AcyclicSP sp = new AcyclicSP(eg, left);

        int[] res = new int[width()];
        int pos = 0;
        for (DirectedEdge edge : sp.pathTo(right)) {
            if (edge.from() == left)
                continue;
            res[pos++] = edge.from() % height();
        }

        return res;
    }

    private static void checkSeamConstraints(int[] seam, int min, int max) {
        for (int i = 0; i < seam.length; i++) {
            if (i > 0)
                require(Math.abs(seam[i] - seam[i-1]) < 2);
            require(seam[i] >= min);
            require(seam[i] <= max);
        }
    }

    // remove horizontal seam from current picture
    public void removeHorizontalSeam(int[] seam) {
        require(seam != null);
        require(height() > 1);
        require(seam.length == width());
        checkSeamConstraints(seam, 0, height() - 1);
        
        Picture np = new Picture(width(), height() - 1);

        for (int x = 0; x < width(); x++) {
            for (int y = 0; y < seam[x]; y++)
                np.set(x, y, picture.get(x, y));
            for (int y = seam[x] + 1; y < height(); y++)
                np.set(x, y - 1, picture.get(x, y));
        }

        picture = np;
    }

    // remove vertical seam from current picture
    public void removeVerticalSeam(int[] seam) {
        require(seam != null);
        require(width() > 1);
        require(seam.length == height());
        checkSeamConstraints(seam, 0, width() - 1);
        
        Picture np = new Picture(width() - 1, height());

        for (int y = 0; y < height(); y++) {
            for (int x = 0; x < seam[y]; x++)
                np.set(x, y, picture.get(x, y));
            for (int x = seam[y] + 1; x < width(); x++)
                np.set(x - 1, y, picture.get(x, y));
        }

        picture = np;
    }

    public static void main(String[] args) {
        Picture p = new Picture(args[0]);
        SeamCarver sm = new SeamCarver(p);

        System.out.println("Testing energy");
        System.out.println("energy(2,1) (expected 151.02) " + sm.energy(2, 1));
        System.out.println("energy(4,3) (expected 194.50) " + sm.energy(4, 3));
        
        System.out.println("Testing horizontal seam");
        for (int i : sm.findHorizontalSeam())
            System.out.print(" " + i);

        System.out.println();
        System.out.println("Testing vertical seam");
        for (int i : sm.findVerticalSeam())
            System.out.print(" " + i);

        System.out.println();
        System.out.println("Testing seam removal");
        sm.removeHorizontalSeam(sm.findHorizontalSeam());
        //sm.picture().show();
    }
}
