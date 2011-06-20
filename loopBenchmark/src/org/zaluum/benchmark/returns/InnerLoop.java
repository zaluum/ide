package org.zaluum.benchmark.returns;

import org.zaluum.benchmark.Benchmark;

public class InnerLoop extends Loop{
  public Lt lt = new Lt();
  public Sum sum = new Sum();
  public Const one = new Const(1);
  public double shift;
  private boolean ret; 
  @Override
  public void contents() {
    double sumRes = sum.apply(shift, one.apply());
    shift = sumRes;
    ret = lt.apply(sumRes, Benchmark.inner);
  }

  @Override
  public boolean cond() {
    return ret;
  }
}
