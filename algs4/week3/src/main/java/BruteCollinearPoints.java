import java.util.ArrayList;
import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.StdDraw;

public class BruteCollinearPoints {
    private final ArrayList<LineSegment> segments = new ArrayList<>();
    
    // finds all line segments containing 4 points
    public BruteCollinearPoints(Point[] points) {

        if (points == null)
            throw new IllegalArgumentException();

        for (int i = 0; i < points.length; i++) {
            if (points[i] == null)
                throw new IllegalArgumentException();
            for (int j = i + 1; j < points.length; j++) {
                if (points[i].compareTo(points[j]) == 0)
                    throw new IllegalArgumentException();
            }
        }
        
        for (int i = 0; i < points.length; i++)
            for (int j = i + 1; j < points.length; j++)
                for (int k = j + 1; k < points.length; k++)
                    for (int g = k + 1; g < points.length; g++) {
                        if (points[i].slopeOrder().compare(points[j], points[k]) == 0
                            && points[i].slopeOrder().compare(points[k], points[g]) == 0) {
                            segments.add(new LineSegment(minPoint(points[i], points[j], points[k], points[g]),
                                    maxPoint(points[i], points[j], points[k], points[g])));
                        }
                    }
    }

    private static Point minPoint(Point p1, Point p2, Point p3, Point p4) {
        Point s1 = p1.compareTo(p2) < 0 ? p1 : p2;
        Point s2 = p3.compareTo(p4) < 0 ? p3 : p4;
        return s1.compareTo(s2) < 0 ? s1 : s2;
    }

    private static Point maxPoint(Point p1, Point p2, Point p3, Point p4) {
        Point s1 = p1.compareTo(p2) > 0 ? p1 : p2;
        Point s2 = p3.compareTo(p4) > 0 ? p3 : p4;
        return s1.compareTo(s2) > 0 ? s1 : s2;
    }

    // the number of line segments
    public           int numberOfSegments() {
        return segments.size();
    }

    // the line segments
    public LineSegment[] segments() {
        return segments.toArray(new LineSegment[numberOfSegments()]);
    }

    public static void main(String[] args) {

        // read the n points from a file
        In in = new In(args[0]);
        int n = in.readInt();
        Point[] points = new Point[n];
        for (int i = 0; i < n; i++) {
            int x = in.readInt();
            int y = in.readInt();
            points[i] = new Point(x, y);
        }

        // draw the points
        StdDraw.enableDoubleBuffering();
        StdDraw.setXscale(0, 32768);
        StdDraw.setYscale(0, 32768);
        for (Point p : points) {
            p.draw();
        }
        StdDraw.show();

        // print and draw the line segments
        BruteCollinearPoints collinear = new BruteCollinearPoints(points);
        for (LineSegment segment : collinear.segments()) {
            StdOut.println(segment);
            segment.draw();
        }
        StdDraw.show();
    }
}
