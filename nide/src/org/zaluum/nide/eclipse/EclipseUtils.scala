package org.zaluum.nide.eclipse
import org.eclipse.core.resources.{ IFile, IResourceVisitor, IResource, IContainer }
import org.eclipse.jdt.core.{ IJavaProject, IClasspathEntry }
trait EclipseUtils {
  def jproject: IJavaProject
  def project = jproject.getProject
  def visitSourceZaluums(body: IFile ⇒ Unit) {
    project.accept(
      new IResourceVisitor {
        def visit(resource: IResource) = resource match {
          case f: IFile if ("zaluum" == f.getFileExtension && isSource(f)) ⇒
            body(f)
            false
          case c: IContainer ⇒ true
          case _ ⇒ true
        }
      })
  }
  def isSourceCP(e: IClasspathEntry) = e.getEntryKind == IClasspathEntry.CPE_SOURCE
  def sourcePaths = jproject.getResolvedClasspath(true) collect { case p if isSourceCP(p) ⇒ p.getPath }
  def isSource(file: IFile) = { sourcePaths exists (p ⇒ p.isPrefixOf(file.getFullPath)) }

}