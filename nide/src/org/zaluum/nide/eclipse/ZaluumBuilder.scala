package org.zaluum.nide.eclipse;

import java.io.{ByteArrayInputStream, InputStream, File, DataOutputStream, BufferedOutputStream, FileOutputStream, FileInputStream, DataInputStream, EOFException, FileNotFoundException}
import java.util.{Map => JMap}
import org.eclipse.core.resources.{IncrementalProjectBuilder, IResourceDeltaVisitor, IResourceDelta, IResource, IProject, IMarker, IFile, IContainer}
import org.eclipse.core.runtime.{IProgressMonitor, Path, IPath}
import org.eclipse.jdt.internal.core.JavaModelManager
import org.zaluum.nide.compiler._
import org.zaluum.nide.protobuf.BoxFileProtos
import org.zaluum.nide.{Activator, Cached}
import scala.collection.mutable.Buffer

object ZaluumBuilder {
  val BUILDER_ID = "org.zaluum.nide.zaluumBuilder";
  val MARKER_TYPE = "org.zaluum.nide.zaluumProblem";
  def readTree(i: InputStream, className: Name) = {
    val proto = BoxFileProtos.BoxClassDef.parseFrom(i)
    Parser.parse(proto, Some(className))
  }
}

class ZaluumBuilder extends IncrementalProjectBuilder with EclipseUtils {
  var notifier: BuildNotifier = _
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

  private def addMarker(file: IFile, message: String, blame: Option[Location],
    severity: Int) {
    val marker = file.createMarker(ZaluumBuilder.MARKER_TYPE);
    marker.setAttribute(IMarker.MESSAGE, message + blame.toString);
    marker.setAttribute(IMarker.SEVERITY, severity);
    marker.setAttribute("BLAME", blame.map { _.toString } getOrElse (Location(List()).toString));
  }
  private def addProjectMarker(message: String, severity: Int) {
    val marker = getProject.createMarker(ZaluumBuilder.MARKER_TYPE);
    marker.setAttribute(IMarker.MESSAGE, message);
    marker.setAttribute(IMarker.SEVERITY, severity);
  }
  protected def build(kind: Int, args: JMap[_, _], monitor: IProgressMonitor): Array[IProject] = {
    // imitate JavaBuilder
    val currentProject = getProject
    if (currentProject == null || !currentProject.isAccessible) return Array()
    notifier = new BuildNotifier(monitor)
    notifier.begin()
    notifier.checkCancel()
    fullBuild(monitor)
    notifier.done()

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
    return null;
  }

  private def deleteMarkers(file: IFile) {
    file.deleteMarkers(ZaluumBuilder.MARKER_TYPE, false, IResource.DEPTH_INFINITE);
  }
  def jmodel = JavaModelManager.getJavaModelManager.getJavaModel
  def jProject = jmodel.getJavaProject(getProject);
  def defaultOutputFolder = { jProject.getOutputLocation }
  def root = getProject.getWorkspace.getRoot

  def compile(f: IFile, pr: ZaluumProject) = {
    val reporter = new Reporter
    try {
      pr.toClassName(f) foreach { className ⇒
        val tree = ZaluumBuilder.readTree(f.getContents(true), className)
        val scope = new FakeGlobalScope(pr)
        val analyzedTree = new Analyzer(reporter, tree, scope).compile()
        def generate(tree: Tree) {
          val sym = tree.symbol.asInstanceOf[BoxTypeSymbol]
          /*println("/*** analyzedTree")
          PrettyPrinter.print(analyzedTree,0)
          println("***/")*/
          val classTree = new TreeToClass(tree, scope).run()
          val outputPath = defaultOutputFolder.append(new Path(sym.fqName.toRelativePathClass))
          writeFile(outputPath, ByteCodeGen.dump(classTree))
          State.addPath(outputPath)
          tree match {
            case b: BoxDef ⇒ b.defs foreach { case c: BoxDef ⇒ generate(c) }
          }
        }
        val bd = analyzedTree.asInstanceOf[BoxDef]
        reporter.check()
        generate(analyzedTree)
      }
    } catch {
      case e: CompilationException ⇒
        for (err ← reporter.errors) {
          addMarker(f, err.msg, err.mark, IMarker.SEVERITY_ERROR)
        }
      case e: FileNotFoundException ⇒
        addProjectMarker("Project is out of sync", IMarker.SEVERITY_ERROR)
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
  def serializationFile: Option[File] = {
    if (!project.exists()) None
    else Some(project.getWorkingLocation(Activator.PLUGIN_ID).append("state.dat").toFile())
  }
  override def clean(monitor: IProgressMonitor) {
    for (written ← State.lastState(); path ← written) {
      val f = workspaceRoot.getFile(path)
      f.delete(true, false, null)
    }
    getProject.deleteMarkers(ZaluumBuilder.MARKER_TYPE, true, IResource.DEPTH_INFINITE);
    State.discard()
  }
  protected def fullBuild(monitor: IProgressMonitor) {
    notifier.checkCancel()
    notifier.subtask("Cleaning output")
    clean(monitor)
    notifier.update(0.05)
    notifier.subtask("Preparing build " + getProject.getName)
    ZaluumModelMananger.getOrCreate(getProject) foreach { zp ⇒
      notifier.update(0.05)
      zp.reset()
      val sources = allSourceZaluums
      val inc = 0.90 / (if (sources.size <= 0) 1 else sources.size)
      for (f ← sources) {
        notifier.checkCancel()
        notifier.subtask("Building: " + f.getName)
        compile(f, zp)
        notifier.update(inc)
      }
      // save generated files list
      State.flip();
      notifier.done()
    }
  }
  protected def incrementalBuild(delta: IResourceDelta,
    monitor: IProgressMonitor) {
    // the visitor does the work.
    delta.accept(new SampleDeltaVisitor());
  }
  protected object State {
    private val currentState = Buffer[IPath]()
    val lastState = new Cached[List[IPath]](readState)
    def addPath(p: IPath) {
      currentState += p
    }
    def discard() {
      writeState()
      lastState.reset()
      serializationFile foreach { _.delete }
    }
    def flip() {
      lastState.replace(currentState.toList)
      currentState.clear()
    }
    private def writeState() {
      serializationFile foreach { file ⇒
        val out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)));
        try {
          out.writeUTF(Activator.PLUGIN_ID)
          for (p ← currentState) {
            out.writeUTF(p.toPortableString)
          }
          out.flush()
        } finally {
          out.close()
        }
      }
    }
    private def readState(): Option[List[IPath]] = {
      serializationFile flatMap { file ⇒
        try {
          val in = new DataInputStream(new FileInputStream(file))
          try {
            if (in.readUTF == Activator.PLUGIN_ID) {
              var l = List[IPath]()
              try {
                while (true) {
                  l ::= Path.fromPortableString(in.readUTF)
                }
              } catch { case e: EOFException ⇒ }
              Some(l)
            } else None
          } finally { in.close }
        } catch { case e: FileNotFoundException ⇒ None }
      }
    }
  }
}
