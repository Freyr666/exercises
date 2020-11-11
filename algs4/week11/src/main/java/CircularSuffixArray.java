import edu.princeton.cs.algs4.StdOut;

public class CircularSuffixArray {

    private final int[] index;

    private static void swap(int[] arr, int a, int b) {
        int tmp = arr[a];
        arr[a] = arr[b];
        arr[b] = tmp;
    }
    
    private void sort(String s, int off, int start, int end) {
        if (end <= start || off == s.length())
            return;

        int lt = start, gt = end;
        char pivot = s.charAt((index[start] + off) % s.length());
        int i = start + 1;

        while (i <= gt) {
            int t = s.charAt((index[i] + off) % s.length());
            if (t < pivot)  swap(index, lt++, i++);
            else if (t > pivot) swap(index, i, gt--);
            else i++;
        }

        sort(s, off, start, lt-1);
        sort(s, off+1, lt, gt); // Pivotal
        sort(s, off, gt+1, end);
    }
    
    // circular suffix array of s
    public CircularSuffixArray(String s) {
        if (s == null)
            throw new IllegalArgumentException();

        index = new int[s.length()];
        for (int i = 0; i < index.length; i++)
            index[i] = i;

        sort(s, 0, 0, s.length() - 1);
    }

    // length of s
    public int length() {
        return index.length;
    }

    // returns index of ith sorted suffix
    public int index(int i) {
        if (i >= index.length || i < 0)
            throw new IllegalArgumentException();

        return index[i];
    }

    // unit testing (required)
    public static void main(String[] args) {
        String test = "ABRACADABRA!";
        CircularSuffixArray csa = new CircularSuffixArray(test);
        for (int i = 0; i < csa.length(); i++) {
            StdOut.println("index[" + i + "] = " + csa.index(i));
        }
    }

}
