package org.zaluum.basic;

import java.util.List;
import java.util.Map;

import org.eclipse.swt.widgets.Shell;

public interface BoxConfigurer {
	Map<String,List<String>> configure(Shell shell, Map<String,List<String>> values);
}
