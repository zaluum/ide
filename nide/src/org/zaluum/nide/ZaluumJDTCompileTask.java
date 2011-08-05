package org.zaluum.nide;

import java.io.File;

import org.apache.tools.ant.taskdefs.Javac;
import org.apache.tools.ant.util.GlobPatternMapper;
import org.apache.tools.ant.util.SourceFileScanner;

/**
 * The ant task that calls the JDT Compiler for groovy.
 * <p>
 * This task takes the same arguments as the Javac task
 * 
 * 
 * @author Andrew Eisenberg
 * @created Jul 6, 2009
 * 
 */
public class ZaluumJDTCompileTask extends Javac {
	public ZaluumJDTCompileTask() {
	}

	protected void scanDir(File srcDir, File destDir, String[] files) {
		GlobPatternMapper m = new GlobPatternMapper();
		m.setFrom("*.java"); //$NON-NLS-1$
		m.setTo("*.class"); //$NON-NLS-1$
		SourceFileScanner sfs = new SourceFileScanner(this);
		File[] newJavaFiles = sfs.restrictAsFiles(files, srcDir, destDir, m);

		m.setFrom("*.zaluum"); //$NON-NLS-1$
		File[] newZaluumFiles = sfs.restrictAsFiles(files, srcDir, destDir, m);

		if (newJavaFiles.length > 0 || newZaluumFiles.length > 0) {

			File[] newCompileList = new File[compileList.length
					+ newJavaFiles.length + newZaluumFiles.length];

			System.arraycopy(compileList, 0, newCompileList, 0,
					compileList.length);
			System.arraycopy(newJavaFiles, 0, newCompileList,
					compileList.length, newJavaFiles.length);
			System.arraycopy(newZaluumFiles, 0, newCompileList,
					compileList.length + newJavaFiles.length,
					newZaluumFiles.length);
			compileList = newCompileList;
		}

	}
}
