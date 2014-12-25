package scalan.performance;

import java.util.Arrays;

/**
 * Created by zotov on 12/4/14.
 */
public class MVM
{
    static {
        System.loadLibrary("ddmvm"); // hello.dll (Windows) or libhello.so (Unixes)
    }

    // Native method declaration
    public native double[] ddmvm( double[][] M, double[] v );

    // Test Driver
    public static void main(String[] args) throws Exception {
        double[][] M = { {0.0, 11.0, 2.0}
                       , {0.0, 1.0, 2.0}
                       , {0.0, 1.0, 22.0} };
        double[] v = {1.0, 2.0, 3.0};

        double[] res = new MVM().ddmvm(M, v);  // Invoke native method
        System.out.println( Arrays.toString(res) );

//        double[][] M1  = new double[10000][10000];
//        double[] v1 = new double [10000];
//
//        for( int i = 0; i < 10; ++i ) {
//            System.out.print("" + i + ": ");
//            new MVM().ddmvm(M1, v1);  // Invoke native method
//
////            Thread.sleep(1000);
//        }

    }

}
