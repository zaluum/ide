STEPS
1) on targetPlatform run computep2 to generate p2 repo for dependencies
2) link 127.0.0.1/repo to targetPlatform/repo
3) set up nexus on 127.0.0.1:8081/nexus
4) go to updateSite and run buildProduct.py

COMMENTS

* jdt-feature patches p2content.xml to allow the feature patch to work with multiple jdt versions
  see http://contraptionsforprogramming.blogspot.com/2009/09/groovy-eclipse-plugin-now-supports.html
* updateSite zaluum.p2.inf adds update sites 
* buildProduct.py uses a vanilla product with no dependencies and installs one by one the other features. It allows the features to be updated.

TODO
-> investigate P2 Profile
-> use target.platform
PROBLEMS
Failed to execute goal org.sonatype.tycho:maven-osgi-compiler-plugin:0.9.0:compile (default-compile) on project org.zaluum.runtime: Compilation failure: Compilation failure:
[ERROR] /home/frede/devel/zaluum/runtime/src/org/zaluum/widget/MyRangePolicy.java (at line 3):[-1,-1]
[ERROR] import info.monitorenter.gui.chart.rangepolicies.ARangePolicy;
tycho 0.9 - 0.12.0 -> 
https://bugs.eclipse.org/bugs/show_bug.cgi?id=348624
Bundle-ClassPath : ., etc etc !! solution
