package org.zaluum.nide.java;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.annotation.ElementType;
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface In {
  int x();
  int y();
}
