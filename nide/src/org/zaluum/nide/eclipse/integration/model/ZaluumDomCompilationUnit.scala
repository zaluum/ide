package org.zaluum.nide.eclipse.integration.model
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.dom.CompilationUnit
import org.zaluum.nide.compiler.BoxDef

class ZaluumDomCompilationUnit(ast: AST, val tree: BoxDef) extends CompilationUnit(ast)

