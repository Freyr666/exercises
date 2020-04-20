import java.lang.Math;
import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdStats;

public class PercolationStats {

    double sqrtTrials;
    double results[];
    Double mean = null;
    Double stddev = null;
    Double lo = null;
    Double hi = null;
       
    // perform independent trials on an n-by-n grid
    public PercolationStats(int n, int trials) {
        if (n <= 0 || trials <= 0)
            throw new IllegalArgumentException();

        results = new double[trials];
        sqrtTrials = Math.sqrt((double) trials);

        for (int iter = 0; iter < trials; iter++) {
            Percolation p = new Percolation(n);
            while (!p.percolates()) {
                p.open(StdRandom.uniform(n) + 1,
                       StdRandom.uniform(n) + 1);
            }
            results[iter] =
                (double)p.numberOfOpenSites()
                / (double) (n * n);
        }
    }

    // sample mean of percolation threshold
    public double mean() {
        if (mean == null)
            mean = StdStats.mean(results);
        return mean;
    }

    // sample standard deviation of percolation threshold
    public double stddev() {
        if (stddev == null)
            stddev = StdStats.stddev(results);
        return stddev;
    }

    // low endpoint of 95% confidence interval
    public double confidenceLo() {
        if (lo == null) {
            lo = mean() - 1.96 *  Math.sqrt(stddev()) / sqrtTrials;
        }
        return lo;
    }

    // high endpoint of 95% confidence interval
    public double confidenceHi() {
        if (hi == null) {
            hi = mean() + 1.96 *  Math.sqrt(stddev()) / sqrtTrials;
        }
        return hi;
    }

   // test client (see below)
    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: [bin] size trials");
            return;
        }

        int n = Integer.parseInt(args[0]);
        int trials = Integer.parseInt(args[1]);

        PercolationStats ps = new PercolationStats(n, trials);

        System.out.println("mean\t= " + ps.mean());
        System.out.println("stddev\t= " + ps.stddev());
        System.out.println("95% confidence interval\t= ["
                           + ps.confidenceLo() + ", "
                           + ps.confidenceHi() + "]");
    }

}
