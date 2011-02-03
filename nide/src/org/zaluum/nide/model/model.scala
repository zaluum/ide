package org.zaluum.nide.model

/*object BoxClassName {
  def parse(str:String) : BoxClassName = {
    if (str.contains('$')) {
      def fullName(name:BoxClassName, names : List[String]):BoxClassName = {
        if (names.isEmpty) name 
        else fullName(InnerBoxClassName(name,names(0)),names.drop(1))
      }
      val names = str.split('$').toList
      fullName (ExtBoxClassName(names(0)), names.drop(1))
    }else{
      ExtBoxClassName(str) 
    }
  }
}
sealed trait BoxClassName {
  def isFullyQualifiedClassname: Boolean
  protected def checkCharStart(c: Char) = Character.isJavaIdentifierStart(c) || Character.isIdentifierIgnorable(c)
  protected def checkCharPart(c: Char) = Character.isJavaIdentifierPart(c) || Character.isIdentifierIgnorable(c)
  protected def partOk(part: String) = {
    !part.isEmpty &&
      checkCharStart(part(0)) &&
      part.view(1, part.length).forall { checkCharPart(_) }
  }
  def toRelativePathClass = toRelativePath + ".class"
  def toRelativePath = toString.replace(".", "/") 
  def classNameWithoutPackage = toString.split(".").lastOption
  def internal = toString.replace('.','/')
  def descriptor = "L" + internal + ";"
}
case class ExtBoxClassName(className: String) extends BoxClassName {
  override def toString = className
  private def partClassname = className.split("[\\.]").toList;
  def isFullyQualifiedClassname = {
    def checkParts = {
      val parts = partClassname
      parts.length != 0 && parts.forall {partOk(_)}
    }
    className != null && checkParts
  }
}
case class InnerBoxClassName(parent: BoxClassName, className: String) extends BoxClassName {
  if (className==null || className == "") throw new Exception
  override def toString = parent + "$" + className
  def isFullyQualifiedClassname = {
    parent.isFullyQualifiedClassname && partOk(className)
  }

}*/