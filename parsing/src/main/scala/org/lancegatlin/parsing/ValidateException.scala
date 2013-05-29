package org.lancegatlin.parsing

// Value may be null if the value wasn't set
trait ValidateException extends Exception {
  def message : String

  def field : Symbol
  def value : Option[Any]

  def valueStr = value.map(v => s"(${v.toString})").getOrElse("")

  override lazy val toString = s"${field.name}$valueStr $message"
  override def getMessage = toString
}

case class InvalidValueException(field: Symbol, _value: Any, message : String = "invalid value") extends ValidateException {
  lazy val value = Some(_value)
}

object InvalidValueException {
  def apply(field: String, _value : Any, message : String) : InvalidValueException = InvalidValueException(Symbol(field), _value, message)
  def apply(field: String, _value : Any) : InvalidValueException = InvalidValueException(Symbol(field), _value)
}

case class MissingValueException(field: Symbol) extends ValidateException {
  def fieldName = field
  def value = None
  def message = "missing value"
}

object MissingValueException {
  def apply(field: String) : MissingValueException = MissingValueException(Symbol(field))
}

object ValidateException {
  import TryAll.util._

  def apply(fieldPair: (Symbol,Any), _message: String, cause: Option[Throwable] = None) = new ValidateException {
    def field = fieldPair._1
    lazy val value = Some(fieldPair._2)
    def message = _message
    override def getCause = cause.orNull
  }

  object util {
    implicit class pimpMySymbolTuple2[A](val field: (Symbol, A)) extends AnyVal {
      def fieldName = field._1
      def value = field._2
    }

    implicit class pimpMyValidateExceptionList(val exceptions: List[ValidateException]) extends AnyVal {
      def throwIfFailure = if(exceptions.nonEmpty) throw Exceptions(exceptions)
    }

    def validateAll(vs: List[ValidateException]*) : List[ValidateException] =
      vs.reduce(_ ::: _)

    def isValid(field: Symbol, value: Any, test: Boolean, message : String) : List[ValidateException] =
      if(test) Nil else InvalidValueException(field, value, message) :: Nil

    def nonEmpty[T](field: (Symbol, Option[T])) : List[ValidateException] =
      if(field._2.nonEmpty) Nil else MissingValueException(field._1) :: Nil

    def isNotNull[T](field: Symbol, value: Any) : List[ValidateException] =
      isValid(field,value, value != null, "must not be null")

    def isSet(field: Symbol, value: String) : List[ValidateException] =
      isValid(field,value, value != null && value.length > 0, "must be set")

    def min[T](field: Symbol, value: T)(min: T)(implicit o: Ordering[T])  : List[ValidateException] =
      isValid(field, value, o.gt(value, min), s"must be greater than $min")

    def max[T](field: Symbol, value : T)(max: T)(implicit o: Ordering[T]) : List[ValidateException] =
      isValid(field, value, o.lt(value,max), s"must be less than $max")

    def range[T](field: Symbol, value: T)(min: T, max: T)(implicit o: Ordering[T]) : List[ValidateException] =
      isValid(field, value, o.lt(min,value) && o.lt(value,max), s"must be greater than $min and less than $max")
  }
}

