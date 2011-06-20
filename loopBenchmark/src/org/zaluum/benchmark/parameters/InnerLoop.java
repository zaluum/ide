package org.zaluum.benchmark.parameters;

import org.zaluum.benchmark.Benchmark;

public class InnerLoop extends Loop{
  public Lt lt = new Lt();
  public Sum sum = new Sum();
  public Const one = new Const(1);
  public double shift; 
  @Override
  public void contents() {
    one.apply();
    sum.apply(shift, one.a);
    shift = sum.s;
    lt.apply(sum.s, Benchmark.inner);
  }

  @Override
  public boolean cond() {
    return lt.out;
  }
}
