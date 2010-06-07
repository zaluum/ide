package org.zaluum.ide

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

class BoxPerspective extends IPerspectiveFactory {

	var factory : IPageLayout = _
	val ID = "zaluum.BoxPerspective";

	def createInitialLayout(factory : IPageLayout) = {
		this.factory = factory;
		addViews()
		addActionSets()
		addNewWizardShortcuts()
		addViewShortcuts()
	}
	
	def addViews() = {
		
		val bottom = factory.createFolder("bottomLeft", IPageLayout.BOTTOM, 0.5f, factory.getEditorArea())
		bottom.addView(IPageLayout.ID_PROBLEM_VIEW)
		
		val bottomRight = factory.createFolder("bottomRight", IPageLayout.RIGHT, 0.5F, "bottomLeft")
		bottomRight.addView(IPageLayout.ID_PROP_SHEET)
		
		val topLeft = factory.createFolder("topLeft", IPageLayout.LEFT, 0.3f, factory.getEditorArea())
		topLeft.addView(IPageLayout.ID_RES_NAV)
		//topLeft.addView(ServerView.ID)
		
		val topLeftDown = factory.createFolder("topLeftDown", IPageLayout.BOTTOM, 0.6f, "topLeft");
		topLeftDown.addView(IPageLayout.ID_OUTLINE);
		factory.addFastView(BoxFaceView.ID)
	}
	
	def addActionSets() = {
		factory.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);
	}
	
	def addNewWizardShortcuts() = {
		factory.addNewWizardShortcut("org.zaluum.ide.editor.boxprojectwizard");
		factory.addNewWizardShortcut("org.zaluum.ide.editor.boxwizard");
		factory.addNewWizardShortcut("org.zaluum.ide.editor.groovywizard");
		factory.addNewWizardShortcut("org.zaluum.ide.editor.testwizard");
		factory.addNewWizardShortcut("org.zaluum.ide.editor.typeswizard");
	}
	
	def addViewShortcuts() = {
		factory.addShowViewShortcut("org.zaluum.ide.editor.runtime.server");
		factory.addShowViewShortcut("org.zaluum.ide.editor.ports");
		factory.addShowViewShortcut("org.zaluum.ide.editor.boxface");
		factory.addShowViewShortcut(IPageLayout.ID_RES_NAV);
		factory.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
		factory.addShowViewShortcut(IPageLayout.ID_OUTLINE);
		factory.addShowViewShortcut(IPageLayout.ID_PROP_SHEET);
	}
	
}