package org.zaluum.nide.eclipse
import org.eclipse.ui.{ IPerspectiveFactory, IPageLayout, IFolderLayout }

class BoxPerspective extends IPerspectiveFactory {
  var factory: IPageLayout = _
  val ID = "zaluum.BoxPerspective";
  def createInitialLayout(factory: IPageLayout) = {
    this.factory = factory;
    addViews()
    addActionSets()
    addNewWizardShortcuts()
    addViewShortcuts()
  }
  def addViews() = {
    factory.createFolder("bottomLeft", IPageLayout.BOTTOM, 0.5f, factory.getEditorArea()).addView(IPageLayout.ID_PROBLEM_VIEW)
    factory.createFolder("bottomRight", IPageLayout.RIGHT, 0.5F, "bottomLeft").addView(IPageLayout.ID_PROP_SHEET)
    factory.createFolder("topLeft", IPageLayout.LEFT, 0.3f, factory.getEditorArea()).addView(IPageLayout.ID_RES_NAV)
    factory.createFolder("topLeftDown", IPageLayout.BOTTOM, 0.6f, "topLeft").addView(IPageLayout.ID_OUTLINE)
  }
  def addActionSets() = {
    factory.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET);
  }
  def addNewWizardShortcuts() = {
    factory.addNewWizardShortcut("org.zaluum.ide.editor.boxprojectwizard");
    factory.addNewWizardShortcut("org.zaluum.ide.editor.boxwizard");
  }
  def addViewShortcuts() = {
    factory.addShowViewShortcut(IPageLayout.ID_RES_NAV);
    factory.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
    factory.addShowViewShortcut(IPageLayout.ID_OUTLINE);
    factory.addShowViewShortcut(IPageLayout.ID_PROP_SHEET);
  }
}

class DebugPerspective extends IPerspectiveFactory {
  var factory: IPageLayout = _
  val ID = "zaluum.DebugPerspective"
  def createInitialLayout(factory: IPageLayout) = {
    this.factory = factory
    addViews()
    addActionSets()
  }
  def addViews() = {
    factory.createFolder("topLeft", IPageLayout.LEFT, 0.3f, factory.getEditorArea()).addView(IPageLayout.ID_RES_NAV)
    factory.createFolder("topLeftDown", IPageLayout.BOTTOM, 0.6f, "topLeft").addView(IPageLayout.ID_OUTLINE)
  }
  def addActionSets() = {
    factory.addActionSet(IPageLayout.ID_NAVIGATE_ACTION_SET)
  }
}
