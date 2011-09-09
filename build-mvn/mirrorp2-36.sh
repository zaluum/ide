#!/bin/sh

# scala tools
echo "scala tools"
/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose  -application org.eclipse.equinox.p2.metadata.repository.mirrorApplication -source http://download.scala-ide.org/scala-eclipse-toolchain-osgi-2.9.0.final -destination file:/home/frede/devel/zaluum-third/scalaToolsMirror/

/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose -application org.eclipse.equinox.p2.artifact.repository.mirrorApplication -source http://download.scala-ide.org/scala-eclipse-toolchain-osgi-2.9.0.final -destination file:/home/frede/devel/zaluum-third/scalaToolsMirror/ 

# 3.6 updates
echo "3.6 updates"
/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose  -application org.eclipse.equinox.p2.metadata.repository.mirrorApplication -source http://download.eclipse.org/eclipse/updates/3.6/ -destination file:/home/frede/devel/zaluum-third/3.6UpdatesMirror/ 

/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose -application org.eclipse.equinox.p2.artifact.repository.mirrorApplication -source http://download.eclipse.org/eclipse/updates/3.6/ -destination file:/home/frede/devel/zaluum-third/3.6UpdatesMirror/ 

# helios
echo "helios"
/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose  -application org.eclipse.equinox.p2.metadata.repository.mirrorApplication -source http://download.eclipse.org/releases/helios/ -destination file:/home/frede/devel/zaluum-third/heliosMirror/ 

/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose -application org.eclipse.equinox.p2.artifact.repository.mirrorApplication -source http://download.eclipse.org/releases/helios/ -destination file:/home/frede/devel/zaluum-third/heliosMirror/

# gef
echo "gef"
/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose  -application org.eclipse.equinox.p2.metadata.repository.mirrorApplication -source http://download.eclipse.org/tools/gef/updates/releases/ -destination file:/home/frede/devel/zaluum-third/gefMirror/ 

/home/frede/devel/eclipse3.6/eclipse -nosplash -verbose -application org.eclipse.equinox.p2.artifact.repository.mirrorApplication -source http://download.eclipse.org/tools/gef/updates/releases/ -destination file:/home/frede/devel/zaluum-third/gefMirror/


