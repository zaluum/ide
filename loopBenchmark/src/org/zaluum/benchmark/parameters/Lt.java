package org.zaluum.benchmark.parameters;

public class Lt {
  public boolean out;
  public void apply(double a, double b) {
    out = a<b;
  }
}
