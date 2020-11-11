import java.util.NoSuchElementException;

import edu.princeton.cs.algs4.BinaryStdIn;
import edu.princeton.cs.algs4.BinaryStdOut;

public class MoveToFront {

    private static char[] allocateArray() {
        char[] seq = new char[256];
        for (char i = 0; i < 256; i++)
            seq[i] = i;
        return seq;
    }

    private static void moveToFront(char[] arr, int ind) {
        char ch = arr[ind];
        for (int i = ind; i >= 1; i--)
            arr[i] = arr[i - 1];
        arr[0] = ch;
    }

    private static int index(char[] arr, char ch) {
        for (int i = 0; i < 256; i++) {
            if (arr[i] == ch) return i;
        }
        throw new NoSuchElementException();
    }
    
    // apply move-to-front encoding, reading from standard input and writing to standard output
    public static void encode() {
        char[] seq = allocateArray();

        while (!BinaryStdIn.isEmpty()) {
            char c = BinaryStdIn.readChar();
            int index = index(seq, c);
            moveToFront(seq, index);
            BinaryStdOut.write(index, 8);
        }

        BinaryStdOut.close();
    }

    // apply move-to-front decoding, reading from standard input and writing to standard output
    public static void decode() {
        char[] seq = allocateArray();

        while (!BinaryStdIn.isEmpty()) {
            int index = BinaryStdIn.readInt(8);
            char ch = seq[index];
            moveToFront(seq, index);
            BinaryStdOut.write(ch);
        }

        BinaryStdOut.close();
    }

    // if args[0] is "-", apply move-to-front encoding
    // if args[0] is "+", apply move-to-front decoding
    public static void main(String[] args) {
        if (args[0].equals("-")) {
            encode();
        } else if (args[0].equals("+")) {
            decode();
        }
    }

}
