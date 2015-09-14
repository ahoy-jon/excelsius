package io.univalence.excelsius




case class A(name:String, age:Int, plouf:List[B])

case class B(name:String)

case class D(group:String,la:List[A], grp2:Int)

object Example {

  def main(args: Array[String]) {


    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + ((t1 - t0)/1000000) + "ms")
      result
    }

    val as: List[A] = List(A("hello", 1, List(B("yo"), B("lo"))), A("hallo", 2, Nil))


    time(Iterator.continually(List(D("grp", List(as, as, as).flatten, 1))).flatten.take(500).toList.excel())
  }

}
