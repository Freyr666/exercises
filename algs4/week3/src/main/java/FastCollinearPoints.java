import java.util.Arrays;
import java.util.ArrayList;
import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.StdDraw;

public class FastCollinearPoints {
    private final ArrayList<LineSegment> segments = new ArrayList<>();
    
    // finds all line segments containing 4 or more points
    public FastCollinearPoints(Point[] points) {
        
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

        Point[] copy = Arrays.copyOf(points, points.length);
        Arrays.sort(points);
        
        for (int i = 0; i < copy.length - 3; i++) {
            Arrays.sort(copy);
            Arrays.sort(copy, copy[i].slopeOrder());

            for (int first = 1, last = 1; last < copy.length; last++) {
                while (last < copy.length
                       && copy[0].slopeOrder().compare(copy[first], copy[last]) == 0)
                    last++;

                if (last - first >= 3
                    && copy[0].compareTo(copy[first]) < 0)
                    segments.add(new LineSegment(copy[0], copy[last - 1]));

                first = last;
            }
        }
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
        FastCollinearPoints collinear = new FastCollinearPoints(points);
        for (LineSegment segment : collinear.segments()) {
            StdOut.println(segment);
            segment.draw();
        }
        StdDraw.show();
    }
}
