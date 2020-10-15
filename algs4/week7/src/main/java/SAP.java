import edu.princeton.cs.algs4.BreadthFirstDirectedPaths;
import edu.princeton.cs.algs4.Digraph;

public class SAP {
    
    private final Digraph graph;

    // constructor takes a digraph (not necessarily a DAG)
    public SAP(Digraph G) {
        if (G == null)
            throw new IllegalArgumentException();

        graph = new Digraph(G);
    }

    private void validate(int v) {
        if (v < 0 || v >= graph.V())
            throw new IllegalArgumentException();
    }

    private void validate(Iterable<Integer> v) {
        if (v == null)
            throw new IllegalArgumentException();

        for (var vert : v) {
            if (vert == null)
                throw new IllegalArgumentException();
            
            validate(vert);
        }
    }

    private final int Length = 0;
    private final int Vertex = 1;
    
    private int[] shortestPath(int v, int w) {
        validate(v);
        validate(w);

        int[] result = new int[2];

        result[Length] = Integer.MAX_VALUE;
        result[Vertex] = -1;

        var vPaths = new BreadthFirstDirectedPaths(graph, v);
        var wPaths = new BreadthFirstDirectedPaths(graph, w);

        for (int com = 0; com < graph.V(); com++) {
            if (vPaths.hasPathTo(com) && wPaths.hasPathTo(com)) {
                int len = vPaths.distTo(com) + wPaths.distTo(com);
                if (len < result[Length]) {
                    result[Length] = len;
                    result[Vertex] = com;
                }
            }
        }

        if (result[Vertex] == -1)
            result[Length] = -1;
        
        return result;
    }

    private int[] shortestPath(Iterable<Integer> v, Iterable<Integer> w) {
        validate(v);
        validate(w);

        int[] result = new int[2];

        result[Length] = Integer.MAX_VALUE;
        result[Vertex] = -1;

        var vPaths = new BreadthFirstDirectedPaths(graph, v);
        var wPaths = new BreadthFirstDirectedPaths(graph, w);

        for (int com = 0; com < graph.V(); com++) {
            if (vPaths.hasPathTo(com) && wPaths.hasPathTo(com)) {
                int len = vPaths.distTo(com) + wPaths.distTo(com);
                if (len < result[Length]) {
                    result[Length] = len;
                    result[Vertex] = com;
                }
            }
        }

        if (result[Vertex] == -1)
            result[Length] = -1;
        
        return result;
    }

    // length of shortest ancestral path between v and w; -1 if no such path
    public int length(int v, int w) {
        int[] result = shortestPath(v, w);
        return result[Length];
    }

    // a common ancestor of v and w that participates in a shortest ancestral path; -1 if no such path
    public int ancestor(int v, int w) {
        int[] result = shortestPath(v, w);
        return result[Vertex];
    }

    // length of shortest ancestral path between any vertex in v and any vertex in w; -1 if no such path
    public int length(Iterable<Integer> v, Iterable<Integer> w) {
        int[] result = shortestPath(v, w);
        return result[Length];
    }

    // a common ancestor that participates in shortest ancestral path; -1 if no such path
    public int ancestor(Iterable<Integer> v, Iterable<Integer> w) {
        int[] result = shortestPath(v, w);
        return result[Vertex];
    }

    // do unit testing of this class
    public static void main(String[] args) {

    }
}
