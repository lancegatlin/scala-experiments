package org.lancegatlin.parsing

import scala.xml._

// This parser that provides a log (a monad Writer) inaddition to the result of the parse
// The log can be used to return error messages for all problems detected
// Or it can be used to return warnings, info, etc
object ParsePattern4 {
  case class Person(
    firstName: String,
    middleName: String,
    lastName: String,
    age: Int
  )

  // A case class monad Writer that can accumulate log items in addition the parse result
  case class Parse[A](log: List[Any], result: Option[A]) {

    // repr/flatMap that implements the monad Writer pattern
    def map[B](f: Option[A] => Option[B]) = Parse(log,f(result))
    def flatMap[B](f: Option[A] => Parse[B]) = {
      val inner = f(result)
      Parse(log ++ inner.log, inner.result)
    }

    // Syntatic sugar that ensures that if the result is None then the log item is logged otherwise the value
    // is just returned
    def orLog(item: Any) = result match {
      case Some(_) => this
      case None => Parse(log :+ item, result)
    }
  }

  object Parse {
    def empty[A] : Parse[A] = Parse[A](Nil, None)

    // Make a new Parse from an Option with an empty log (if success)
    // If an exception is thrown then add as log item
    def apply[A](opt: => Option[A]) : Parse[A] = {
      try {
        Parse[A](Nil, opt)
      } catch {
        case e : Exception => Parse(e :: Nil ,None)
      }
    }

    // monad Writer pattern - this is used to add an item to the log but don't care about value
    def tell(log: List[Any]) : Parse[Unit] = Parse[Unit](log, None)

    // Many times during parsing we may need to extract a value and then parse it - each step may fail separately
    // This function is used to flatten nested parsing
    def flatten[A](optParse: => Option[Parse[A]]) : Parse[A] =
      optParse match {
        case Some(parse) => parse
        case None => Parse.empty
      }
  }

  // This is a standard functional pattern function that will convert a function that takes any 4 arguments into one
  // that takes an Option for each argument and returns an Option of the return value. If any argument is None the
  // return value will be None
  def lift4[A,B,C,D,E](me: Function4[A,B,C,D,E]) = { (oa: Option[A],ob:Option[B],oc:Option[C],od:Option[D]) =>
    for(a <- oa;b <- ob;c <- oc;d <- od) yield me(a,b,c,d)
  }

  def parsePerson(xml: NodeSeq) : Parse[Person] =
    for {

      // 1) Construct a Parse object
      // 2) Extract firstName from xml and get its text value as above
      // 3) If this fails (or throws an exception) then None is returned
      // 4) orLog will ensure that if None is returned then the message is logged (also the exception if any)
      // 5) As execution flows down the for-comprehension, the Parse repr/flatMap functions will ensure that log items
      // are automatically accumulated and returned in the Parse[Person] result
      // 6) Because Options are extracted from the Parse object, execution will not stop if a value is not present
      // all values will be examined (and all potential error messages accumulated)
      optFirstName <- Parse {
        (xml\"firstName").headOption.map(_.text)
      }.orLog("Missing first name!")

      // Sometimes it is necessary to make decisions based on a parsed value
      // Working with parsed values requires extracting the value out of the Option (if it exists)
      // The Parse.flatten pattern is used when a expression returns a Parse itself
      // Parse.flatten will extract the Option and return it and ensure the logs are accumulated
      // The specific pattern below is used to inject a log item but ignore the result value
      _ <- Parse.flatten {
        for {
          firstName <- optFirstName
        } yield
          // Parse.tell is used to inject a log item - the value it returns is Unit (monad Writer pattern)
          Parse.tell(s"WARNING: It's $firstName!" :: Nil)
      }

      optMiddleName <- Parse {
        (xml\"middleName").headOption.map(_.text)
      }.orLog("Missing middle name!")

      optLastName <- Parse {
        (xml\"lastName").headOption.map(_.text)
      }.orLog("Missing last name!")

      optSAge <- Parse {
        (xml\"age").headOption.map(_.text)
        // Note: we could convert age to integer here, but if we did we would not be able to distinguish a missing age
        // from an invalid age in the log
      }.orLog("Missing age!")

      // Here we need to convert the extracted string to an integer - to work with it must Parse.flatten pattern
      optAge <- Parse.flatten {
        for {
          s_age <- optSAge
        } yield Parse(Some(s_age.toInt)).orLog(s"$s_age is an invalid number!")
      }
    } yield
      lift4(Person.apply)(optFirstName,optMiddleName,optLastName,optAge)
}

