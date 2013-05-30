package org.lancegatlin.parsing

trait Exceptions extends Exception {
  def exceptions: List[Throwable]
  override lazy val toString = if(exceptions.length == 1) exceptions.head.toString else s"Exceptions(${exceptions.mkString(", ")})"
  override def getMessage = toString
}

object Exceptions {
  def apply(_exceptions: List[Throwable]) = new Exceptions {
    def exceptions: List[Throwable] = _exceptions.map(_ match {
      case Exceptions(exceptions) => exceptions
      case exception => exception :: Nil
    }).flatten
  }
  def unapply(e: Exceptions) : Option[List[Throwable]] =
    if(e eq null) scala.None
    else scala.Some(e.exceptions)
}