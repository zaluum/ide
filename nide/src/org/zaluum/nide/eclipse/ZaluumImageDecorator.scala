package org.zaluum.nide.eclipse;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.viewsupport.ImageDescriptorRegistry;
import org.eclipse.jdt.internal.ui.viewsupport.JavaElementImageProvider;
import org.eclipse.jdt.internal.ui.viewsupport.TreeHierarchyLayoutProblemsDecorator;
import org.eclipse.jdt.ui.JavaElementImageDescriptor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.internal.decorators.DecoratorManager;

class ZaluumImageDecorator extends ILabelDecorator {
    private var fRegistry : ImageDescriptorRegistry =_ 
    private val problemsDecorator = new TreeHierarchyLayoutProblemsDecorator();
    private var decman = WorkbenchPlugin.getDefault().getDecoratorManager();
    private var preventRecursion= false;


    val ZALUUM_NATURE = "org.zaluum.nide.zaluumNature"; //$NON-NLS-1$

    def decorateImage(origimage : Image , element: Object ) : Image = {
        if (preventRecursion) {
            return null;
        }
        var image:Image = null
        var isApplicable = false;
        element match {
          case cu : ICompilationUnit =>
            val r = cu.getResource
            if (r.getName().endsWith(".zaluum")) {
                image = getJavaElementImageDescriptor(origimage, r);
                isApplicable = true;
            }
          case f : IFile if (f.getName.endsWith(".zaluum")) =>
            image = getJavaElementImageDescriptor(image, f);
            isApplicable = true;
          case s:String =>
            image = getImageLabel(new JavaElementImageDescriptor(ZaluumPluginImages.DESC_ZALUUM_FILE, 0, JavaElementImageProvider.SMALL_SIZE));
            isApplicable = true;
          case _ =>
        }   

        if (isApplicable) {
            preventRecursion = true;
            try {
                //the Java ProblemsDecorator is not registered in the official
                //decorator list of eclipse, so we need it to call ourself.
                //problem: if jdt includes more decorators, we won't know it.
                image = problemsDecorator.decorateImage(image, element);

                //apply standard decorators (eg cvs)
                image = decman.decorateImage(image, element);
            } finally {
                preventRecursion = false;
            }
            return image;
        }
        return null;
    }
    private def getJavaElementImageDescriptor(image: Image , resource:IResource ) : Image = {

        var flags = 0
        if (image != null) {
            val rect = image.getBounds();
            flags = if (rect.width == 16) JavaElementImageProvider.SMALL_ICONS else 0;
        } else {
            flags = JavaElementImageProvider.SMALL_ICONS;
        }
        val size= if (useSmallSize(flags)) JavaElementImageProvider.SMALL_SIZE else JavaElementImageProvider.BIG_SIZE;
        var desc : ImageDescriptor = null
        try {
            if (resource.getProject().hasNature(ZALUUM_NATURE)) {
               desc = ZaluumPluginImages.DESC_ZALUUM_FILE;
            } else {
                desc = ZaluumPluginImages.DESC_ZALUUM_FILE_NO_BUILD;
            }
        } catch { case e:CoreException =>
          desc = ZaluumPluginImages.DESC_ZALUUM_FILE_NO_BUILD;
        }
        return getImageLabel(new JavaElementImageDescriptor(desc, 0, size));
    }
    private def useSmallSize(flags : Int ) = (flags & JavaElementImageProvider.SMALL_ICONS) != 0;

    private def getImageLabel(descriptor : ImageDescriptor ) :Image  ={
        if (descriptor == null)
            return null;
        return getRegistry().get(descriptor);
    }

    private def  getRegistry() : ImageDescriptorRegistry ={
        if (fRegistry == null) {
            fRegistry= JavaPlugin.getImageDescriptorRegistry();
        }
        return fRegistry;
    }


    def addListener(listener:ILabelProviderListener ) {}

    def dispose()  {}

    def isLabelProperty(element : Object , property: String )  = false

    def removeListener( listener : ILabelProviderListener)  {
    }

    def decorateText(text : String , element : Object ) = null
}