import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.Draw;
import edu.princeton.cs.algs4.Point2D;
import edu.princeton.cs.algs4.RectHV;
import java.util.TreeSet;
import java.util.Stack;

public class PointSET {
    private final TreeSet<Point2D> set;
    
    // construct an empty set of points 
    public PointSET() {
        set = new TreeSet<Point2D>();
    }

    // is the set empty? 
    public boolean isEmpty() {
        return set.isEmpty();
    }

    // number of points in the set 
    public int size() {
        return set.size();
    }

    // add the point to the set (if it is not already in the set)
    public void insert(Point2D p) {
        if (p == null)
            throw new IllegalArgumentException();

        set.add(p);
    }

    // does the set contain point p? 
    public boolean contains(Point2D p) {
        if (p == null)
            throw new IllegalArgumentException();

        return set.contains(p);
    }

    // draw all points to standard draw 
    public void draw() {
        StdDraw.setPenColor(Draw.BLACK);
        StdDraw.setPenRadius(0.01);
        for (Point2D p : set)
            p.draw();
    }

    // all points that are inside the rectangle (or on the boundary) 
    public Iterable<Point2D> range(RectHV rect) {
        Stack<Point2D> stack = new Stack<Point2D>();

        if (rect == null)
            throw new IllegalArgumentException();
        
        for (Point2D p : set)
            if (rect.contains(p))
                stack.push(p);

        return stack;
    }

    // a nearest neighbor in the set to point p; null if the set is empty 
    public Point2D nearest(Point2D p) {
        double min = Double.MAX_VALUE;
        Point2D res = null;

        if (p == null)
            throw new IllegalArgumentException();
        
        for (Point2D test : set) {
            double dist = test.distanceTo(p);
            if (dist < min) {
                min = dist;
                res = test;
            }
        }
        return res;
    }

    public static void main(String[] args) {
        
    }
}
