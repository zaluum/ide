package org.zaluum.benchmark.allobjects;

import org.zaluum.benchmark.Benchmark;

public class InnerLoop extends Loop{
  public Lt lt = new Lt();
  public Sum sum = new Sum();
  public Const one = new Const(1);
  public double shift; 
  @Override
  public void contents() {
    one.apply();
    sum.a=shift;
    sum.b=one.a;
    sum.apply();
    shift = sum.s;
    lt.a=sum.s;
    lt.b=Benchmark.inner;
    lt.apply();
  }

  @Override
  public boolean cond() {
    return lt.out;
  }
}
