package org.lancegatlin.parsing

import scala.util.Either.RightProjection
import scala.xml.NodeSeq
import scala.util.Try

// Parser that parses result OR provides an error message on the first error encountered
object ParsePattern2 {
  case class Person(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  )

  // Syntatic sugar to return an error message if Option is None
  implicit class pimpMyOption[T](val me: Option[T]) extends AnyVal {
    def orErr(err: String) : RightProjection[String,T] = me match {
      case Some(value) => Right(value).right
      case None => Left(err).right
    }
  }

  def parsePerson(xml: NodeSeq) : Either[String, Person] =
    for {

    // Same as above except now we are using Either[String, String]
    // onErr will ensure that if None is returned the value will be Left("error message").right
    // if Some(value) is returned then onErr will extract the value and will return Right(value).right
      firstName <- (xml\"firstName").headOption.map(_.text) orErr("Missing first name!")

      middleName <- (xml\"middleName").headOption.map(_.text) orErr("Missing middle name!")
      lastName <- (xml\"lastName").headOption.map(_.text) orErr("Missing last name!")
      s_age <- (xml\"age").headOption.map(_.text) orErr("Missing age!")
      age <- Try(s_age.toInt).toOption orErr(s"'$s_age' is not a number!")
      validAge <- Some(age).filter(_ < 150) orErr(s"'$s_age' is not less than 150!")
    } yield
      Person(firstName,middleName,lastName,validAge)
}

