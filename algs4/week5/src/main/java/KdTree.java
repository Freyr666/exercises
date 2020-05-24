import edu.princeton.cs.algs4.Draw;
import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.Point2D;
import edu.princeton.cs.algs4.RectHV;
import java.util.Stack;

public class KdTree {
    private class VNode {
        private HNode left = null;
        private HNode right = null;
        private final Point2D point;
        private final RectHV area;

        private VNode(Point2D p) {
            point = p;
            area = new RectHV(0, 0, 1, 1);
        }

        private VNode(Point2D p, RectHV a) {
            point = p;
            area = a;
        }

        private void insert(Point2D p) {
            if (p.x() < point.x()) {
                if (left == null)
                    left = new HNode(p, new RectHV(area.xmin(),
                                                   area.ymin(),
                                                   point.x(),
                                                   area.ymax()));
                else
                    left.insert(p);
            } else {
                if (right == null)
                    right = new HNode(p, new RectHV(point.x(),
                                                    area.ymin(),
                                                    area.xmax(),
                                                    area.ymax()));
                else
                    right.insert(p);
            }
        }

        private int size() {
            int l = 0, r = 0;
            if (left != null)
                l = left.size();
            if (right != null)
                r = right.size();
            return 1 + l + r;
        }

        private boolean contains(Point2D p) {
            if (p.equals(point))
                return true;
            else if (p.x() < point.x() && left != null)
                return left.contains(p);
            else if (p.x() >= point.x() && right != null)
                return right.contains(p);
            else
                return false;
        }

        private void draw() {
            StdDraw.setPenColor(Draw.BLACK);
            StdDraw.setPenRadius(0.01);
            point.draw();
            StdDraw.setPenColor(Draw.RED);
            StdDraw.setPenRadius();
            StdDraw.line(point.x(), area.ymin(), point.x(), area.ymax());
            if (left != null)
                left.draw();
            if (right != null)
                right.draw();
        }

        private Point2D nearest(Point2D search) {
            return nearest(search, null);
        }

        private Point2D nearest(Point2D search, Point2D cur) {
            Point2D closest;
            
            if (cur != null
                && area.distanceSquaredTo(search) > cur.distanceSquaredTo(search))
                return cur;

            if (cur == null
                || cur.distanceSquaredTo(search) > point.distanceSquaredTo(search))
                closest = point;
            else
                closest = cur;

            if (left != null) {
                Point2D l = left.nearest(search, closest);
                if (l.distanceSquaredTo(search) < closest.distanceSquaredTo(search))
                    closest = l;
            }

            if (right != null) {
                Point2D r = right.nearest(search, closest);
                if (r.distanceSquaredTo(search) < closest.distanceSquaredTo(search))
                    closest = r;
            }

            return closest;
        }

        private Stack<Point2D> range(RectHV rect) {
            Stack<Point2D> stack = new Stack<Point2D>();

            range(rect, stack);
            
            return stack;
        }

        private void range(RectHV rect, Stack<Point2D> stack) {
            if (!area.intersects(rect))
                return;

            if (rect.contains(point))
                stack.push(point);

            if (left != null)
                left.range(rect, stack);

            if (right != null)
                right.range(rect, stack);
        }
    }

    private class HNode {
        private VNode top = null;
        private VNode bottom = null;
        private final Point2D point;
        private final RectHV area;

        private HNode(Point2D p, RectHV a) {
            point = p;
            area = a;
        }

        private void insert(Point2D p) {
            if (p.y() < point.y()) {
                if (bottom == null)
                    bottom = new VNode(p, new RectHV(area.xmin(),
                                                     area.ymin(),
                                                     area.xmax(),
                                                     point.y()));
                else
                    bottom.insert(p);
            } else {
                if (top == null)
                    top = new VNode(p, new RectHV(area.xmin(),
                                                  point.y(),
                                                  area.xmax(),
                                                  area.ymax()));
                else
                    top.insert(p);
            }
        }

        private int size() {
            int t = 0, b = 0;
            if (top != null)
                t = top.size();
            if (bottom != null)
                b = bottom.size();
            return 1 + t + b;
        }

        private boolean contains(Point2D p) {
            if (p.equals(point))
                return true;
            else if (p.y() >= point.y() && top != null)
                return top.contains(p);
            else if (p.y() < point.y() && bottom != null)
                return bottom.contains(p);
            else
                return false;
        }

        private void draw() {
            StdDraw.setPenColor(Draw.BLACK);
            StdDraw.setPenRadius(0.01);
            point.draw();
            StdDraw.setPenColor(Draw.BLUE);
            StdDraw.setPenRadius();
            StdDraw.line(area.xmin(), point.y(), area.xmax(), point.y());
            if (top != null)
                top.draw();
            if (bottom != null)
                bottom.draw();
        }

        private Point2D nearest(Point2D search, Point2D cur) {
            Point2D closest;
            
            if (cur != null
                && area.distanceSquaredTo(search) > cur.distanceSquaredTo(search))
                return cur;

            if (cur == null
                || cur.distanceSquaredTo(search) > point.distanceSquaredTo(search))
                closest = point;
            else
                closest = cur;

            if (top != null) {
                Point2D t = top.nearest(search, closest);
                if (t.distanceSquaredTo(search) < closest.distanceSquaredTo(search))
                    closest = t;
            }

            if (bottom != null) {
                Point2D b = bottom.nearest(search, closest);
                if (b.distanceSquaredTo(search) < closest.distanceSquaredTo(search))
                    closest = b;
            }

            return closest;
        }

        private void range(RectHV rect, Stack<Point2D> stack) {
            if (!area.intersects(rect))
                return;

            if (rect.contains(point))
                stack.push(point);

            if (top != null)
                top.range(rect, stack);

            if (bottom != null)
                bottom.range(rect, stack);
        }
    }
    
    private VNode root = null;
    
    // construct an empty set of points 
    public KdTree() {
        return;
    }

    // is the set empty? 
    public boolean isEmpty() {
        return root == null;
    }

    // number of points in the set 
    public int size() {
        if (root == null)
            return 0;
        else
            return root.size();
    }

    // add the point to the set (if it is not already in the set)
    public void insert(Point2D p) {
        if (p == null)
            throw new IllegalArgumentException();

        if (root == null)
            root = new VNode(p);
        else
            root.insert(p);
    }

    // does the set contain point p? 
    public boolean contains(Point2D p) {
        if (p == null)
            throw new IllegalArgumentException();

        if (root == null)
            return false;
        else
            return root.contains(p);
    }

    // draw all points to standard draw 
    public void draw() {
        if (root != null)
            root.draw();
    }

    // all points that are inside the rectangle (or on the boundary) 
    public Iterable<Point2D> range(RectHV rect) {
        if (rect == null)
            throw new IllegalArgumentException();
        
        if (root != null)
            return root.range(rect);
        else
            return null;
    }

    // a nearest neighbor in the set to point p; null if the set is empty 
    public Point2D nearest(Point2D p) {
        if (p == null)
            throw new IllegalArgumentException();
        
        if (root != null)
            return root.nearest(p);
        else
            return null;
    }

    public static void main(String[] args) {
        
    }
}
