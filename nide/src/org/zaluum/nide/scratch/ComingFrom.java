package org.zaluum.nide.scratch;

public class ComingFrom {

	public static boolean comingFromRefactoring() {
		try {
			throw new Exception();
		} catch (Exception e) {
			StackTraceElement[] stackTrace = e.getStackTrace();
			for (int i = 0; i < stackTrace.length; i++) {
				String stname = stackTrace[i].getClassName();
				if (stname
						.startsWith("org.eclipse.jdt.internal.corext.refactoring") || //$NON-NLS-1$
						stname.startsWith("org.eclipse.ltk")) { //$NON-NLS-1$
					System.out.println("Util: isJavaLikeFileName"); //$NON-NLS-1$
					return true;
				}
			}
		}
		return false;
	}

}
