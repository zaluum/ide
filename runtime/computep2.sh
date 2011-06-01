#!/bin/sh
/home/frede/devel/eclipse3.6/eclipse -debug -consolelog -nosplash -verbose -application \
  org.eclipse.equinox.p2.publisher.FeaturesAndBundlesPublisher \
  -metadataRepository file:./repo
  -artifactRepository file:./repo
  -source . -compress -publishArtifacts