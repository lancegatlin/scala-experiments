package org.lancegatlin.parsing

import scala.util._
import play.api.libs.json._

object ParseApp extends App {
// TODO: Move this to separate experiment don't complicate this example with custom XML dsl
//  implicit class pimpMySeq[T](val me: Seq[T]) extends AnyVal {
//    def get(index : Int) : Option[T] = if(index < me.length) Some(me(index)) else None
//  }

//  implicit class pimpMyNodeSeq(val me: NodeSeq) extends AnyVal {
//    def x(elementName: String) = me \\ elementName
//    def \(index: Int) = me.get(index)
//    def x(index : Int) = me.get(index)
//  }
//
//  def text() : Node => String = { _.text }
//  def node() : Node => NodeSeq = { _.child }
//
//  implicit class pimpMyOptionNode(val me: Option[Node]) extends AnyVal {
//    def \[T](f: Node => T) = me.repr(f(_))
//  }


  override def main(args: Array[String]) {
    val xml =
      <document>
        <person>
          <firstName>Lance</firstName>
          <middleName>David</middleName>
          <lastName>Gatlin</lastName>
          <age>35</age>
        </person>
      </document> ::
      <document>
        <person>
          <firstName>Lance</firstName>
          <lastName>Gatlin</lastName>
          <age>thirty-five!</age>
        </person>
      </document> ::
      <document>
        <person>
          <firstName>Lance</firstName>
          <middleName>David</middleName>
          <lastName>Gatlin</lastName>
          <age>thirty-five!</age>
        </person>
      </document> ::
      <document>
        <person>
          <firstName>Lance</firstName>
          <lastName>Gatlin</lastName>
        </person>
      </document> ::
      <document>
        <person>
          <firstName>Lance</firstName>
          <middleName>David</middleName>
          <lastName>Gatlin</lastName>
          <age>165</age>
        </person>
      </document> ::
      <document>
        <person>
          <firstName></firstName>
          <middleName></middleName>
          <lastName></lastName>
          <age>-1</age>
        </person>
      </document> ::
      Nil

    println("Parse1\n----")
    xml map(xml => ParsePattern1.parsePerson(xml \ "person")) foreach(println(_))
    println("Parse2\n----")
    xml map(xml => ParsePattern2.parsePerson(xml \ "person")) foreach(println(_))
    println("Parse4\n----")
    xml map(xml => ParsePattern4.parsePerson(xml \ "person")) foreach(println(_))
    println("Parse5\n----")
    xml map(xml => ParsePattern5.parsePerson(xml \ "person")) foreach(println(_))
    println("Parse6\n----")
    import ParsePattern6._ // implicits
    xml map(xml => ParsePattern6.PersonParser.parse(xml \ "person")) foreach(println(_))
    println("Parse7\n----")
    xml map(xml => ParsePattern7.parsePerson(xml \ "person")) foreach(println(_))
    println("Parse8\n----")
    xml map(xml => ParsePattern8.parsePerson(xml \ "person")) foreach(println(_))
    println("Parse9\n----")
    xml map(xml => ParsePattern9.parsePerson(xml \ "person")) foreach(println(_))

    println("Parse9.json\n----")

    {
      import ParsePattern9._
      val jsons =
        """{"firstName":"Lance","middleName":"David","lastName":"Gatlin","age":35}""" ::
        """{"firstName":"Lance","lastName":"Gatlin","age":35}""" ::
        """{"firstName":"Lance","middleName":"David","lastName":"Gatlin","age":"thirty-five!"}""" ::
        """{"firstName":"Lance","lastName":"Gatlin"}""" ::
        """{"firstName":"Lance","middleName":"David","lastName":"Gatlin","age":165}""" ::
          """{"firstName":null,"middleName":null,"lastName":null,"age":-1}""" ::
        Nil


      jsons.map({ json =>
        println(json)
        val jv = Json.parse(json)
        println(Person.json.reads(jv))
      })
    }

  }
}

