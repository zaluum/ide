package org.zaluum.nide.eclipse.integration.model
import org.zaluum.nide.compiler.BoxDef
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.CompilationUnit

class ZaluumDomCompilationUnit(ast : AST, val tree:BoxDef) extends CompilationUnit(ast)

