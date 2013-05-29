package org.lancegatlin.parsing

trait Exceptions extends Exception {
  def exceptions: List[Throwable]
  override lazy val toString = s"Exceptions(${exceptions.mkString(", ")})"
  override def getMessage = toString
}

object Exceptions {
  def apply(_exceptions: List[Throwable]) = new Exceptions {
    def exceptions: List[Throwable] = _exceptions
  }
  def unapply(e: Exceptions) : Option[List[Throwable]] =
    if(e eq null) scala.None
    else scala.Some(e.exceptions)
}