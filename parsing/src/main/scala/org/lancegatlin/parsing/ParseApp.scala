package org.lancegatlin.parsing


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
      Nil

    xml map(xml => ParsePattern1.parsePerson(xml \ "person")) foreach(println(_))
    println
    xml map(xml => ParsePattern2.parsePerson(xml \ "person")) foreach(println(_))
    println
    xml map(xml => ParsePattern4.parsePerson(xml \ "person")) foreach(println(_))
    println
    xml map(xml => ParsePattern5.parsePerson(xml \ "person")) foreach(println(_))
    println
    import ParsePattern6._ // implicits
    xml map(xml => ParsePattern6.PersonParser.parse(xml \ "person")) foreach(println(_))
    println
    xml map(xml => ParsePattern7.parsePerson(xml \ "person")) foreach(println(_))
    println
  }
}