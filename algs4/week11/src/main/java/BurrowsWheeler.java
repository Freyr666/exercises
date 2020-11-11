import edu.princeton.cs.algs4.BinaryStdIn;
import edu.princeton.cs.algs4.BinaryStdOut;

public class BurrowsWheeler {

    // apply Burrows-Wheeler transform,
    // reading from standard input and writing to standard output 
    public static void transform() {
        String input = BinaryStdIn.readString();
        CircularSuffixArray csa = new CircularSuffixArray(input);

        int first = 0;

        for (int i = 0; i < csa.length(); i++) {
            if (csa.index(i) == 0)
                first = i;
        }
        BinaryStdOut.write(first);

        for (int i = 0; i < csa.length(); i++) {
            char c = input.charAt(Math.floorMod(csa.index(i) - 1, input.length()));
            BinaryStdOut.write(c);
        }

        BinaryStdOut.close();
    }

    // apply Burrows-Wheeler inverse transform,
    // reading from standard input and writing to standard output
    public static void inverseTransform() {
        int first = BinaryStdIn.readInt();
        String t = BinaryStdIn.readString();

        char[] s = new char[t.length()];
        int[] next = new int[t.length()];
        int[] count = new int[257];

        for (int i = 0; i < t.length(); i++) {
            count[t.charAt(i) + 1]++;
        }
        for (int i = 0; i < 256; i++) {
            count[i + 1] += count[i];
        }
        for (int i = 0; i < t.length(); i++) {
            int posi = count[t.charAt(i)]++;
            s[posi] = t.charAt(i);
            next[posi] = i;
        }

        for (int i = 0; i < t.length(); i++) {
            BinaryStdOut.write(s[first]);
            first = next[first];
        }

        BinaryStdOut.close();
    }

    // if args[0] is "-", apply Burrows-Wheeler transform
    // if args[0] is "+", apply Burrows-Wheeler inverse transform
    public static void main(String[] args) {
        if (args[0].equals("-")) {
            transform();
        } else if (args[0].equals("+")) {
            inverseTransform();
        }
    }

}
