package org.lancegatlin

import annotation.tailrec

/*
"You are given a function 'secret()' that accepts a single integer parameter and returns an integer.
In your favorite programming language, write a command-line program that takes one command-
line argument (a number) and determines if the secret() function is additive [secret(x+y) = secret(x) +
secret(y)], for all values x and y, where x and y are prime numbers less than the number passed via the
command-line argument."
 */

object Int {
  def unapply(s : String) : Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}

object MyApp {

  def secret(n: Int) : Int = n

  def isAdditive(f: Int => Int, x: Int, y: Int) : Boolean = f(x+y) == f(x) + f(y)

  def primes(i: Stream[Int] = Stream from 2) : Stream[Int] = i.head #:: primes(i.tail.filter(_ % i.head != 0))

  def primesLessThan(maxN: Int) = primes().takeWhile(_ < maxN)

  case class Args(maxN: Option[Int] = None, showAll: Boolean = false, showHelp: Boolean = false)

  @tailrec def parseArgs(stringArgs : List[String], args: Args) : Args = {
    stringArgs match {
      case Nil => args
      case head :: tail =>
        head match {
          case Int(n) => parseArgs(tail, args.copy(maxN = Some(n)))
          case "-A" | "--show-all" => parseArgs(tail, args.copy(showAll = true))
          case "-h" | "-help" | "--help" | "help" => parseArgs(tail, args.copy(showHelp = true))
        }
    }
  }

  def showHelp {
    println("<command> [-A|--show-all] [-h|--help] integer\n\t-A,--show-all\tDisplay all additive prime combinations")
  }

  def main(stringArgs: Array[String]) {
    val args = parseArgs(stringArgs.toList, Args())
    if(args.showHelp) {
      showHelp
    } else {
      args.maxN match {
        case Some(n) =>

          val s : Stream[(Boolean, Int, Int)] = for {
            x <- primesLessThan(n)
            y <- primesLessThan(n)
          } yield (isAdditive(secret, x, y), x, y)

          if (args.showAll) {
            s foreach println
          }

          print("Function is additive? ")
          if(s forall(tuple => tuple._1 == true)) {
            println("Yes")
          } else {
            println("No")
          }
        case None =>
          println("[Error] Must pass a valid integer as an argument!")
          showHelp
      }
    }
  }
}