package io.univalence.excelsius




case class A(name:String, age:Int, plouf:List[B])

case class B(name:String)

case class D(group:String,la:List[A], grp2:Int)



case class Clic(clic_id:String, clic_type:String)

case class Search(search_id:String, search_type:String, clics:List[Clic])

case class Session(session_id:String, searchs:List[Search])

object Example {

  def main(args: Array[String]) {


    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + ((t1 - t0)/1000000) + "ms")
      result
    }




    val l = List(Session("1",
      List(Search("A", "PRODUCT",
      List(Clic("△", "INFO"), Clic("○", "INFO"))), Search("B","NAME", List(Clic("□","BUY"),Clic("□","BUY"))))),Session("2", List(Search("C", "PRODUCT", Nil))))



    l.excel()

    System.exit(0)

    val as: List[A] = List(A("hello", 1, List(B("yo"), B("lo"))), A("hallo", 2, Nil))


    time(Iterator.continually(List(D("grp", List(as, as, as).flatten, 1))).flatten.take(500).toList.excel())
  }

}
