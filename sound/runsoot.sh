if [ -e "$SOOT_JAR" ]; then
  java -Xmx512m -jar $SOOT_JAR -cp $JAVA_HOME/lib/rt.jar:$JAVA_HOME/lib/jsse.jar:$JAVA_HOME/lib/jce.jar:/home/frede/devel/zaluum/runtime/bin:. --app -W $1
else
  echo "cannot find the soot jar on SOOT_JAR = $SOOT_JAR"
fi