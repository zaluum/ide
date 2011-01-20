package org.zaluum.nide.eclipse;

import org.zaluum.nide.compiler.CompilationException
import org.eclipse.core.runtime.IPath
import java.io.InputStream
import scala.collection.mutable.Buffer
import org.eclipse.core.resources.IContainer
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.search.SearchMatch
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.TypeNameMatch
import org.eclipse.jdt.core.search.TypeNameMatchRequestor
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.internal.core.JavaModelManager
import org.eclipse.core.runtime.Path;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import java.util.{ Map ⇒ JMap }
import java.io.ByteArrayInputStream
import org.zaluum.nide.model.ProtoModel
import org.zaluum.nide.compiler.{ Compiler, ByteCodeGen }

object ZaluumBuilder {
  val BUILDER_ID = "org.zaluum.nide.zaluumBuilder";
  val MARKER_TYPE = "org.zaluum.nide.zaluumProblem";
}
class ZaluumBuilder extends IncrementalProjectBuilder with EclipseUtils {

  class SampleDeltaVisitor extends IResourceDeltaVisitor {
    def visit(delta: IResourceDelta) = {
      val resource = delta.getResource
      delta.getKind match {
        case IResourceDelta.ADDED ⇒
        // handle added resource
        case IResourceDelta.REMOVED ⇒
        // handle removed resource
        case IResourceDelta.CHANGED ⇒
        // handle changed resource
        case _ ⇒
      }
      //return true to continue visiting children.
      true;
    }
  }

  private def addMarker(file: IFile, message: String, lineNumber: Int,
    severity: Int) {
    var line = lineNumber
    val marker = file.createMarker(ZaluumBuilder.MARKER_TYPE);
    marker.setAttribute(IMarker.MESSAGE, message);
    marker.setAttribute(IMarker.SEVERITY, severity);
    if (lineNumber == -1) {
      line = 1;
    }
    marker.setAttribute(IMarker.LINE_NUMBER, line);
  }

  protected def build(kind: Int, args: JMap[_, _], monitor: IProgressMonitor): Array[IProject] = {
    /*if (kind == IncrementalProjectBuilder.FULL_BUILD) {
      fullBuild(monitor);
    } else {
      val delta = getDelta(getProject());
      if (delta == null) {
        fullBuild(monitor);
      } else {
        incrementalBuild(delta, monitor);
      }
    }*/
    fullBuild(monitor)
    return null;
  }

  private def deleteMarkers(file: IFile) {
    file.deleteMarkers(ZaluumBuilder.MARKER_TYPE, false, IResource.DEPTH_ZERO);
  }
  def jmodel = JavaModelManager.getJavaModelManager.getJavaModel
  def jproject = jmodel.getJavaProject(getProject);
  def defaultOutputFolder = { jproject.getOutputLocation }
  def toRelativePathClass(className: String) = new Path(className.replace(".", "/") + ".class")
  def root = getProject.getWorkspace.getRoot
  def compile(f: IFile, cl: EclipseBoxClasspath) = {
    try {
      cl.toClassName(f) foreach { className ⇒
        val model = ProtoModel.read(f.getContents, className)
        val compiler = new Compiler(model, cl)
        val compiled = compiler.compile
        val outputPath = defaultOutputFolder.append(toRelativePathClass(className))
        writeFile(outputPath, ByteCodeGen.dump(compiled))
      }
    } catch {
      case e: CompilationException ⇒ println(e.compiler.reporter.errors)
    }
  }
  def writeFile(path: IPath, bytes: Array[Byte]) {
    val is = new ByteArrayInputStream(bytes)
    val outputFile = root.getFile(path)
    if (outputFile.exists)
      outputFile.setContents(is, true, false, null)
    else
      outputFile.create(is, true, null)
  }
  protected def fullBuild(monitor: IProgressMonitor) {
    val cl = new EclipseBoxClasspath(getProject)
    cl.update()
    visitSourceZaluums{ f=> compile(f,cl) }
  }

  protected def incrementalBuild(delta: IResourceDelta,
    monitor: IProgressMonitor) {
    // the visitor does the work.
    delta.accept(new SampleDeltaVisitor());
  }
}
