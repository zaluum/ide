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

@SuppressWarnings("restriction")
public class ZaluumImageDecorator implements ILabelDecorator {

    private ImageDescriptorRegistry fRegistry;
    private TreeHierarchyLayoutProblemsDecorator problemsDecorator;
    private DecoratorManager decman;
    private boolean preventRecursion = false;


    private static final String ZALUUM_NATURE = "org.zaluum.nide.zaluumNature"; //$NON-NLS-1$


    public ZaluumImageDecorator() {
        problemsDecorator = new TreeHierarchyLayoutProblemsDecorator();
        decman = WorkbenchPlugin.getDefault().getDecoratorManager();
    }

    public Image decorateImage(Image image, Object element) {
        if (preventRecursion) {
            return null;
        }

        boolean isApplicable = false;
        if (element instanceof ICompilationUnit) {
            IResource r = ((ICompilationUnit) element).getResource();
            if (r.getName().endsWith(".zaluum")) {
                image = getJavaElementImageDescriptor(image, r);
                isApplicable = true;
            }
        } else if (element instanceof IFile && ((IResource) element).getName().endsWith(".zaluum")) {
            image = getJavaElementImageDescriptor(image, (IResource) element);
            isApplicable = true;
        } else if (element instanceof String) {
            // a request where an IResource cannot be found (probably from opening an svn or cvs file)
            image = getImageLabel(new JavaElementImageDescriptor(ZaluumPluginImages.DESC_ZALUUM_FILE, 0, JavaElementImageProvider.SMALL_SIZE));
            isApplicable = true;
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
    private Image getJavaElementImageDescriptor(Image image, IResource resource) {

        int flags;
        if (image != null) {
            Rectangle rect = image.getBounds();
            flags = (rect.width == 16) ? JavaElementImageProvider.SMALL_ICONS : 0;
        } else {
            flags = JavaElementImageProvider.SMALL_ICONS;
        }
        Point size= useSmallSize(flags) ? JavaElementImageProvider.SMALL_SIZE : JavaElementImageProvider.BIG_SIZE;
        ImageDescriptor desc;
        try {
            if (resource.getProject().hasNature(ZALUUM_NATURE)) {
               desc = ZaluumPluginImages.DESC_ZALUUM_FILE;
            } else {
                desc = ZaluumPluginImages.DESC_ZALUUM_FILE_NO_BUILD;
            }
        } catch (CoreException e) {
          desc = ZaluumPluginImages.DESC_ZALUUM_FILE_NO_BUILD;
        }
        return getImageLabel(new JavaElementImageDescriptor(desc, 0, size));
    }
    private static boolean useSmallSize(int flags) {
        return (flags & JavaElementImageProvider.SMALL_ICONS) != 0;
    }

    private Image getImageLabel(ImageDescriptor descriptor){
        if (descriptor == null)
            return null;
        return getRegistry().get(descriptor);
    }

    private ImageDescriptorRegistry getRegistry() {
        if (fRegistry == null) {
            fRegistry= JavaPlugin.getImageDescriptorRegistry();
        }
        return fRegistry;
    }


    public void addListener(ILabelProviderListener listener) {
    }

    public void dispose()  {
    }

    public boolean isLabelProperty(Object element, String property)  {
        return false;
    }

    public void removeListener(ILabelProviderListener listener)  {
    }

    public String decorateText(String text, Object element) {
        return null;
    }
}
