package org.lancegatlin.parsing

import scala.xml._
import scala.util._

// Experimental - this was interesting to me but in the end doesn't seem to have any benefits over parse1?
object ParsePattern5 {
  case class Person(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  )

  implicit class pimpMyFunc1[A,B](val me: Function1[A,B]) extends AnyVal {
    def map[C](f: B => C) : Function1[A,C] = { a => f(me(a)) }
    def flatMap[C](f: B => Function1[A,C]) : Function1[A,C] = { a => f(me(a))(a) }
  }

  val parsePerson : NodeSeq => Option[Person] =
    for {
      optFirstName <- { e:NodeSeq => (e \ "firstName").headOption.map(_.text) }
      optMiddleName <- { e:NodeSeq => (e \ "middleName").headOption.map(_.text) }
      optLastName <- { e:NodeSeq => (e \ "lastName").headOption.map(_.text) }
      optSAge <- { e:NodeSeq => (e \ "age").headOption.map(_.text) }
      optAge <- { e:NodeSeq => optSAge.map(s_age => Try(s_age.toInt).toOption.filter(_< 150)).flatten }
    } yield
      ParsePattern4.lift4(Person.apply)(optFirstName,optMiddleName,optLastName,optAge)

}
