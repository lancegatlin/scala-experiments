package org.lancegatlin.parsing

import scala.xml.NodeSeq
import scala.util.Try

// Parser that parses result OR returns None - no way to tell why it failed
object ParsePattern1 {
  case class Person(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  )

  def parsePerson(xml: NodeSeq) : Option[Person] =
    for {
    // 1) Get a NodeSeq of all "firstName" elements
    // 2) Get the first element if it exists
    // 3) Map the element to its string content
    // 4) Extract the value from option
      firstName <- (xml\"firstName").headOption.map(_.text)

      // If firstName is None then Option.flatMap will ensure that execution stops above and None is returned for Option[Person]
      middleName <- (xml\"middleName").headOption.map(_.text)

      lastName <- (xml\"lastName").headOption.map(_.text)
      s_age <- (xml\"age").headOption.map(_.text)

      // If the age element is missing, execution will not reach this point
      // Using Try here to suppress the NumberFormatException
      age <- Try(s_age.toInt).toOption.filter(_ < 150)
    } yield
      Person(firstName,middleName,lastName,age)

}
