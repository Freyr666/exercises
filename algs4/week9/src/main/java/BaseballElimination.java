import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.ArrayList;
import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.FlowEdge;
import edu.princeton.cs.algs4.FlowNetwork;
import edu.princeton.cs.algs4.FordFulkerson;
import edu.princeton.cs.algs4.In;

public class BaseballElimination {

    private final int teams;
    private final String name[];
    private final HashMap<String, Integer> ids;
    private final int w[]; // Wins
    private final int l[]; // Loss
    private final int r[]; // Left
    private final int g[][]; // Games left against other team

    private final boolean[] isEliminated;
    private final ArrayList<HashSet<String>> eliminatedBy;

    private static void expect(boolean exp) {
        if (!exp)
            throw new IllegalArgumentException();
    }

    private boolean triviallyEliminated(int id) {
        for (int i = 0; i < teams; i++) {
            if (i == id) continue;
            
            if (w[id] + r[id] < w[i]) { // Trivially eliminated
                isEliminated[id] = true;
                HashSet<String> set = new HashSet<>(1);
                set.add(name[i]);
                eliminatedBy.add(id, set);
                return true;
            }
        }
        return false;
    }

    private void nontrivialElimination(int id) {
        int total = teams * (teams + 1) + teams + 2;
        int s = total - 2;
        int t = total - 1;

        double fullCapacity = 0;
        FlowNetwork fn = new FlowNetwork(total);

        int gId = teams;
        for (int i = 0; i < teams; i++) {
            if (i == id) continue;
            
            double sinkCap = Math.max(0, w[id] + r[id] - w[i]);

            fn.addEdge(new FlowEdge(i, t, sinkCap));
            
            for (int j = i + 1; j < teams; j++, gId++) {
                if (j == id) continue;
                
                double cap = g[i][j];
                
                fn.addEdge(new FlowEdge(s, gId, cap));
                fn.addEdge(new FlowEdge(gId, i, Double.POSITIVE_INFINITY));
                fn.addEdge(new FlowEdge(gId, j, Double.POSITIVE_INFINITY));

                fullCapacity += cap;
            }
        }
        FordFulkerson ff = new FordFulkerson(fn, s, t);
        if (ff.value() < fullCapacity) { // Eliminated
            isEliminated[id] = true;
            HashSet<String> set = new HashSet<>();
            for (int i = 0; i < teams; i++) {
                if (i == id) continue;

                if (ff.inCut(i)) set.add(name[i]);
            }
            eliminatedBy.add(id, set);
        }
    }
    
    private void computeEliminated() {
        for (int id = 0; id < teams; id++)
            if (!triviallyEliminated(id))
                nontrivialElimination(id);
    }

    // create a baseball division from given filename in format specified below
    public BaseballElimination(String filename) {
        expect(filename != null);
        
        In in = new In(filename);
        try {
            teams = in.readInt();
            
            name = new String[teams];
            ids = new HashMap<>(teams);
            w = new int[teams];
            l = new int[teams];
            r = new int[teams];
            g = new int[teams][teams];

            isEliminated = new boolean[teams];
            eliminatedBy = new ArrayList<HashSet<String>>(teams);
            
            for (int i = 0; i < teams; i++) {
                name[i] = in.readString();
                ids.put(name[i], i);
                w[i] = in.readInt();
                l[i] = in.readInt();
                r[i] = in.readInt();
                eliminatedBy.add(null);
                for (int j = 0; j < teams; j++)
                    g[i][j] = in.readInt();
            }

            computeEliminated();
        } finally {
            in.close();
        }
    }

    // number of teams
    public int numberOfTeams() {
        return teams;
    }

    // all teams
    public Iterable<String> teams() {
        return Arrays.asList(name);
    }

    // number of wins for given team
    public int wins(String team) {
        expect(team != null);
        try {
            return w[ids.get(team)];
        } catch (IndexOutOfBoundsException e) {
            throw new IllegalArgumentException();
        }
    }

    // number of losses for given team
    public int losses(String team) {
        expect(team != null);
        try {
            return l[ids.get(team)];
        } catch (IndexOutOfBoundsException e) {
            throw new IllegalArgumentException();
        }
    }

    // number of remaining games for given team
    public int remaining(String team) {
        expect(team != null);
        try {
            return r[ids.get(team)];
        } catch (IndexOutOfBoundsException e) {
            throw new IllegalArgumentException();
        }
    }

    // number of remaining games between team1 and team2
    public int against(String team1, String team2) {
        expect(team1 != null);
        expect(team2 != null);
        try {
            return g[ids.get(team1)][ids.get(team2)];
        } catch (IndexOutOfBoundsException e) {
            throw new IllegalArgumentException();
        }
    }

    
    // is given team eliminated?
    public boolean isEliminated(String team) {
        expect(team != null);
        try {
            return isEliminated[ids.get(team)];
        } catch (IndexOutOfBoundsException e) {
            throw new IllegalArgumentException();
        }
    }

    // subset R of teams that eliminates given team; null if not eliminated
    public Iterable<String> certificateOfElimination(String team) {
        expect(team != null);
        try {
            return eliminatedBy.get(ids.get(team));
        } catch (IndexOutOfBoundsException e) {
            throw new IllegalArgumentException();
        }
    }

    public static void main(String[] args) {
        BaseballElimination division = new BaseballElimination(args[0]);
        for (String team : division.teams()) {
            if (division.isEliminated(team)) {
                StdOut.print(team + " is eliminated by the subset R = { ");
                for (String t : division.certificateOfElimination(team)) {
                    StdOut.print(t + " ");
                }
                StdOut.println("}");
            }
            else {
                StdOut.println(team + " is not eliminated");
            }
        }
}

}
