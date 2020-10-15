
public class Outcast {

    private final WordNet wn;
    
    // constructor takes a WordNet object
    public Outcast(WordNet wordnet) {
        if (wordnet == null)
            throw new IllegalArgumentException();
        
        wn = wordnet;
    }

    // given an array of WordNet nouns, return an outcast
    public String outcast(String[] nouns) {
        if (nouns == null)
            throw new IllegalArgumentException();
        
        int maxDist = -1;
        int maxId = 0;

        for (int id = 0; id < nouns.length; id++) {
            int dist = 0;
            for (String other : nouns) {
                dist += wn.distance(nouns[id], other);
            }
            if (dist > maxDist) {
                maxDist = dist;
                maxId = id;
            }
        }

        return nouns[maxId];
    }

    // see test client below
    public static void main(String[] args) {
        var outcast = new Outcast(new WordNet(args[0], args[1]));
        System.out.println(outcast.outcast(new String[]{"horse", "zebra", "cat", "bear", "table"}));
    }
}
