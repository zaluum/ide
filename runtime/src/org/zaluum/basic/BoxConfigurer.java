package org.zaluum.basic;

import java.util.Map;

import org.eclipse.swt.widgets.Shell;

public interface BoxConfigurer {
	Map<String,String> configure(Shell shell, Map<String,String> values);
}
