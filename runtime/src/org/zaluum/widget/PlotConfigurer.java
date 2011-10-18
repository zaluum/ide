package org.zaluum.widget;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.zaluum.basic.BoxConfigurer;
import org.zaluum.widget.plot.PlotDialog;

public class PlotConfigurer implements BoxConfigurer {

	@Override
	public Map<String, List<String>> configure(Shell shell,
			Map<String, List<String>> values) {
		List<String> param = values.get("#Script");
		String script = null;
		if (param != null && param.size() == 1) {
			script = param.get(0);
		}
		PlotDialog p = new PlotDialog(shell, script);
		if (p.open() == Window.OK) {
			ArrayList<String> list = new ArrayList<String>(1);
			list.add(p.getScript());
			values.put("#Script", list);
			return values;
		}else return null;
	}

}
