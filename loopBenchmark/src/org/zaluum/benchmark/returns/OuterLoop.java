package org.zaluum.benchmark.returns;

import org.zaluum.benchmark.Benchmark;

public class OuterLoop extends Loop{
  public Lt lt = new Lt();
  public Sum sum = new Sum();
  public Const one = new Const(1);
  public double shift;
  public InnerLoop loop=new InnerLoop();
  private boolean ret;
  @Override
  public void contents() {
    one.apply();
    double sumRes = sum.apply(shift,one.apply());
    shift=sumRes;
    ret = lt.apply(sumRes, Benchmark.outer);
    loop.shift=0;
    loop.apply();
  }

  @Override
  public boolean cond() {
    return ret;
  }
  public static void main(String[] args) {
    new OuterLoop().apply();
  }
}
