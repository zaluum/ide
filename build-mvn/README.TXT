!!
It's not working properly. Tycho nightmare won't find feature patch dependencies
Much easier to do in Eclipse, export, deployable features
* installing nexus to speed up

STEPS
1) targetPlatform run computep2
2) link 127.0.0.1/repo to targetPlatform/repo

PROBLEMS
Failed to execute goal org.sonatype.tycho:maven-osgi-compiler-plugin:0.9.0:compile (default-compile) on project org.zaluum.runtime: Compilation failure: Compilation failure:
[ERROR] /home/frede/devel/zaluum/runtime/src/org/zaluum/widget/MyRangePolicy.java (at line 3):[-1,-1]
[ERROR] import info.monitorenter.gui.chart.rangepolicies.ARangePolicy;
tycho 0.9 - 0.12.0 -> 
https://bugs.eclipse.org/bugs/show_bug.cgi?id=348624
Bundle-ClassPath : ., etc etc !! solution
