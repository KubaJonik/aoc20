package com.visma.omed.solver.factory;

public class Part2 {

    static int size = 1000000;

    static int[] input = new int[] {2,1,9,7,4,8,3,6,5};

    static int[] arr = new int[size+1];

    static int steps = 10000000;

    public static void main(String[] args) {
        init();
        int current = input[0];
        for (int i = 0; i < steps; i++) {
            current = step(current);
        }
        long n1 = next(1);
        long n2 = next((int)n1);
        System.out.println(n1 * n2);
    }

    private static int step(int current) {
        int n1 = next(current);
        int n2 = next(n1);
        int n3 = next(n2);

        int d = findDestination(current, n1, n2, n3);

        int dn = next(d);

        setNext(current, next(n3));
        setNext(d, n1);

        setNext(n3, dn);

        return next(current);
    }

    private static int findDestination(int current, int n1, int n2, int n3) {
        for (int d = decD(current);;d = decD(d)) {
            if (d != n1 && d != n2 && d != n3) {
                return d;
            }
        }
    }

    private static int decD(int d) {
        return (d == 1) ? size : d - 1;
    }

    private static void init() {
        for (int i = 1; i < size; i++) {
            setNext(i, i+1);
        }

        setNext(size, input[0]);
        for (int i = 0; i < input.length - 1; i++) {
            setNext(input[i], input[i+1]);
        }

        setNext(input[input.length - 1], input.length + 1);
    }

    private static int next(int nr) {
        return arr[nr - 1];
    }

    private static void setNext(int nr, int next) {
        arr[nr - 1] = next;
    }

}
