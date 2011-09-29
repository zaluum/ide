package org.zaluum.widget;

import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Shell;
import org.zaluum.basic.BoxConfigurer;

public class ChartConfigurer implements BoxConfigurer{

	@Override
	public Map<String, List<String>> configure(Shell shell, Map<String, List<String> > values) {
		Dialog d = new Dialog(shell) {
			
		};
		d.open();
		System.out.println("conf");
		return values;
	}

}
