import java.util.HashSet;
import java.util.Set;

import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.StdOut;

public class BoggleSolver
{
    private static void requires (boolean exp) {
        if (! exp)
            throw new IllegalArgumentException();
    }

    private final Node root = new Node();
    
    // Initializes the data structure using the given array of strings as the dictionary.
    // (You can assume each word in the dictionary contains only the uppercase letters A through Z.)
    public BoggleSolver(String[] dictionary) {
        requires(dictionary != null);
        for (String word : dictionary) {
            requires(word != null);
            addWord(word);
        }
    }
    
    // Returns the set of all valid words in the given Boggle board, as an Iterable.
    public Iterable<String> getAllValidWords(BoggleBoard board) {
        requires(board != null);

        boolean [][] marked = new boolean[board.rows()][board.cols()];
        Set<String> words = new HashSet<>();

        for (int i = 0; i < board.rows(); i++)
            for (int j = 0; j < board.cols(); j++) {
                collectWords(i, j, words, marked, board);
            }
        return words;
    }

    // Returns the score of the given word if it is in the dictionary, zero otherwise.
    // (You can assume the word contains only the uppercase letters A through Z.)
    public int scoreOf(String word) {
        requires(word != null);
        if (!contains(word) || word.length() < 3)
            return 0;
        else if (word.length() < 5)
            return 1;
        else if (word.length() == 5)
            return 2;
        else if (word.length() == 6)
            return 3;
        else if (word.length() == 7)
            return 5;
        else
            return 11;
    }
    
    private static class Node {
        private Node next[] = new Node[26];
        private boolean isValue = false;
    }
    
    private void addWord(String word) {
        assert root != null;
        
        Node node = root;
        for (int i = 0; i < word.length(); i++) {
            char c = word.charAt(i);
            if (node.next[c - 'A'] == null)
                node.next[c - 'A'] = new Node();
            node = node.next[c - 'A'];
        }
        node.isValue = true;
    }

    private void printDict(Node n, String prefix) {
        if (n == null) return;
        if (n.isValue)
            StdOut.println(prefix);
        for (int c = 0; c < 26; c++)
            printDict(n.next[c], prefix + (char)(c + 'A'));
    }
    
    private boolean contains(String word) {
        Node node = root;

        for (int i = 0; i < word.length(); i++) {
            node = node.next[word.charAt(i) - 'A'];
            if (node == null) return false;
        }

        return node.isValue;
    }
    
    private void collectWords(int i, int j,
                              Set<String> words,
                              boolean[][] marked,
                              BoggleBoard board) {
        Node node = root;
        String prefix = "";
        char c = board.getLetter(i, j);
        
        node = node.next[c - 'A'];
        prefix += c;
        
        if (c == 'Q') {
            if (node == null) return;
            node = node.next['U' - 'A'];
            prefix += 'U';
        }

        dfs(i, j, node, prefix, marked, words, board);
    }

    private void dfs(int i, int j,
                     Node n,
                     String prefix,
                     boolean[][] marked,
                     Set<String> words,
                     BoggleBoard board) {
        if (n == null) return;
        
        marked[i][j] = true;
        if (prefix.length() > 2 && n.isValue)
            words.add(prefix);

        for (int x = i - 1; x <= i + 1; x++) {
            for (int y = j - 1; y <= j + 1; y++) {
                if (x >= 0 && x < board.rows()
                    && y >= 0 && y < board.cols()
                    && !marked[x][y]) {
                    
                    Node node = n;
                    char c = board.getLetter(x, y);
                    node = node.next[c - 'A'];
                    String newPrefix = prefix + c;
                    if (c == 'Q') {
                        if (node == null) continue;
                        node = node.next['U' - 'A'];
                        newPrefix += 'U';
                    }
                    dfs(x, y, node, newPrefix, marked, words, board);
                }
            }
        }
        
        marked[i][j] = false;
    }

    public static void main(String[] args) {
        In in = new In(args[0]);
        String[] dictionary = in.readAllStrings();
        BoggleSolver solver = new BoggleSolver(dictionary);
        BoggleBoard board = new BoggleBoard(args[1]);
        int score = 0;
        for (String word : solver.getAllValidWords(board)) {
            StdOut.println(word);
            score += solver.scoreOf(word);
        }
        StdOut.println("Score = " + score);
    }

}
