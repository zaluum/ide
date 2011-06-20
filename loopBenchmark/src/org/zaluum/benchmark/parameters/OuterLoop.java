package org.zaluum.benchmark.parameters;

import org.zaluum.benchmark.Benchmark;

public class OuterLoop extends Loop{
  public Lt lt = new Lt();
  public Sum sum = new Sum();
  public Const one = new Const(1);
  public double shift;
  public InnerLoop loop=new InnerLoop();
  @Override
  public void contents() {
    one.apply();
    sum.apply(shift,one.a);
    shift = sum.s;
    lt.apply(sum.s, Benchmark.outer);
    loop.shift=0;
    loop.apply();
  }

  @Override
  public boolean cond() {
    return lt.out;
  }
  public static void main(String[] args) {
    new OuterLoop().apply();
  }
}
