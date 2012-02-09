#!/bin/sh
DIR=`pwd`
fetch()
{
  COMMAND=$@
  $ECLIPSE -nosplash -verbose  -application org.eclipse.equinox.p2.artifact.repository.mirrorApplication $COMMAND
  $ECLIPSE -nosplash -verbose  -application org.eclipse.equinox.p2.metadata.repository.mirrorApplication $COMMAND
}

echo "scala tools"
fetch -source http://download.scala-ide.org/scala-eclipse-toolchain-osgi-2.9.1.final -destination file:$DIR/scalaToolsMirror-2.9.1/

