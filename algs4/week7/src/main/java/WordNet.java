import java.util.HashMap;
import java.util.Scanner;
import java.util.HashSet;
import java.util.Set;
import java.io.File;
import java.io.FileNotFoundException;

import edu.princeton.cs.algs4.Digraph;
import edu.princeton.cs.algs4.DirectedCycle;

public class WordNet {

    private final HashMap<String,Set<Integer>> nouns;
    private final HashMap<Integer,String> meanings;
    private final SAP sap;

    // constructor takes the name of the two input files
    public WordNet(String synsets, String hypernyms) {
        if (synsets == null || hypernyms == null)
            throw new IllegalArgumentException();

        int size = 0;
        nouns = new HashMap<>();
        meanings = new HashMap<>();
        
        try (Scanner synIn = new Scanner(new File(synsets))) {
            while (synIn.hasNext()) {
                String line = synIn.nextLine();
                String[] args = line.split(",");
                int id = Integer.parseInt(args[0]);
                size = Integer.max(size, id + 1);
                meanings.put(id, args[1]);
                for (String noun : args[1].split(" +")) {
                    nouns.compute(noun, (k, v) -> {
                            if (v == null)
                                v = new HashSet<Integer>();
                            v.add(id);
                            return v;
                        });
                }
            }
        } catch (FileNotFoundException e) {
            throw new IllegalArgumentException();
        }

        Digraph graph = new Digraph(size);

        try (Scanner hypIn = new Scanner(new File(hypernyms))) {
            while (hypIn.hasNext()) {
                String line = hypIn.nextLine();
                String[] args = line.split(",");
                int id = Integer.parseInt(args[0]);
                for (int pos = 1; pos < args.length; pos++) {
                    graph.addEdge(id, Integer.parseInt(args[pos]));
                }
            }
        } catch (FileNotFoundException e) {
            throw new IllegalArgumentException();
        }

        DirectedCycle dc = new DirectedCycle(graph);
        if (dc.hasCycle())
            throw new IllegalArgumentException();

        int rootsNum = 0;
        for (int v = 0; v < graph.V(); v++) {
            if (graph.outdegree(v) == 0) {
                rootsNum++;
            }
        }

        if (rootsNum != 1)
            throw new IllegalArgumentException();

        sap = new SAP(graph);
    }

    // returns all WordNet nouns
    public Iterable<String> nouns() {
        return nouns.keySet();
    }

    // is the word a WordNet noun?
    public boolean isNoun(String word) {
        if (word == null)
            throw new IllegalArgumentException();
        
        return nouns.containsKey(word);
    }

    // distance between nounA and nounB (defined below)
    public int distance(String nounA, String nounB) {
        if (!isNoun(nounA) || !isNoun(nounB))
            throw new IllegalArgumentException();
        
        return sap.length(nouns.get(nounA), nouns.get(nounB));
    }

    // a synset (second field of synsets.txt) that is the common ancestor of nounA and nounB
    // in a shortest ancestral path (defined below)
    public String sap(String nounA, String nounB) {
        if (!isNoun(nounA) || !isNoun(nounB))
            throw new IllegalArgumentException();
        
        return meanings.get(sap.ancestor(nouns.get(nounA), nouns.get(nounB)));
    }

    // do unit testing of this class
    public static void main(String[] args) {
        var wn = new WordNet(args[0], args[1]);
        System.out.println(wn.sap("pagan", "dualism"));
    }
}
