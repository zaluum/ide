#!/bin/sh
DIRECTORY=$(cd `dirname $0` && pwd)
/home/frede/devel/eclipse3.6/eclipse -debug -consolelog -nosplash -verbose -application \
  org.eclipse.equinox.p2.publisher.FeaturesAndBundlesPublisher \
  -metadataRepository file:$DIRECTORY/repo \
  -artifactRepository file:$DIRECTORY/repo \
  -source $DIRECTORY -compress -publishArtifacts