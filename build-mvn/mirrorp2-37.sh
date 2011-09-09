#!/bin/sh
ECLIPSE=/home/frede/devel/eclipse3.7/eclipse
fetch()
{
  COMMAND=$@
  $ECLIPSE -nosplash -verbose  -application org.eclipse.equinox.p2.artifact.repository.mirrorApplication $COMMAND
  $ECLIPSE -nosplash -verbose  -application org.eclipse.equinox.p2.metadata.repository.mirrorApplication $COMMAND
}

echo "scala tools"
fetch -source http://download.scala-ide.org/scala-eclipse-toolchain-osgi-2.9.1.final -destination file:/home/frede/devel/zaluum-third/scalaToolsMirror-2.9.1/

echo "3.7 updates"
fetch -source http://download.eclipse.org/eclipse/updates/3.7/ -destination file:/home/frede/devel/zaluum-third/3.7UpdatesMirror/ 

echo "indigo"
fetch -source http://download.eclipse.org/releases/indigo/ -destination file:/home/frede/devel/zaluum-third/indigoMirror/ 

echo "gef"
fetch -source http://download.eclipse.org/tools/gef/updates/releases/ -destination file:/home/frede/devel/zaluum-third/gefMirror/ 


