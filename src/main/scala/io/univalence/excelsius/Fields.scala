package io.univalence.excelsius

import java.awt.Desktop
import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import shapeless._
import shapeless.labelled._

import scala.collection.GenMap
import scala.language.higherKinds
import scala.util.Random


trait TypeName[T] {
  def name: String
}

object TypeName {
  import scala.reflect.runtime.universe.TypeTag

  implicit def fromTypeTag[T](implicit typeTag: TypeTag[T]): TypeName[T] = new TypeName[T] {
    override def name: String = typeTag.tpe.toString
  }
}



case class Encode(main: List[String], cont: List[(Int, List[Encode])]) {

  def toRows: List[List[String]] = {

    //val color = if(cont == Nil) "XXXXXX" else Random.alphanumeric.take(6).mkString
    val mainC: List[String] = main
    mainC :: cont.flatMap({ case (i, l) => {
      val take: List[String] = mainC.take(i)
      l.flatMap(e => {
        val map: List[List[String]] = e.toRows.map(l => {
          take ::: l ::: mainC.drop(i + l.size)
        })
        map
      })
    }
    })
  }
}




trait FieldSpec[V] {
  def modifier: String

  def pr(v: V): Encode
}

trait LowPriorityFieldSpec {

  implicit def fieldSpec[T]: FieldSpec[T] = new FieldSpec[T] {
    override def modifier: String = ""

    private def str(v:T):String = if(v == null) "" else v.toString

    override def pr(v: T): Encode = Encode(List(str(v)), Nil)
  }
}

object FieldSpec extends LowPriorityFieldSpec {

  implicit def fieldSpecOpt[T](implicit fieldSpec: FieldSpec[T]): FieldSpec[Option[T]] = new FieldSpec[Option[T]] {
    override def modifier: String = "?" + fieldSpec.modifier

    override def pr(v: Option[T]): Encode = v match {
      case None => Encode(List("<->"), Nil)
      case Some(e) => fieldSpec.pr(e)
    }
  }


  implicit def fieldSpecList[T](implicit fieldSpec: FieldSpec[T]): FieldSpec[List[T]] = new FieldSpec[List[T]] {
    override def modifier: String = "[]" + fieldSpec.modifier

    override def pr(v: List[T]): Encode = {
      if(v.isEmpty) Encode(List("[-]"),Nil) else {
        Encode(List(Encode(List("a"),List(0 -> v.map(fieldSpec.pr))).toRows.tail.flatten.mkString(", ")),Nil)
      }
    }
  }

}


trait Fields[L] {
  def fieldnames: List[(String,String)]

  def pr(a: L): Encode
}


trait LowPriorityFields {


  implicit def caseClassFields[F, G](implicit gen: LabelledGeneric.Aux[F, G], encode: Lazy[Fields[G]]): Fields[F] =
    new Fields[F] {
      override def fieldnames: List[(String,String)] = encode.value.fieldnames

      override def pr(a: F): Encode = encode.value.pr(gen.to(a))
    }

  implicit def hcon[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K],
                                                tv:TypeName[H],
                                                leftSpec: Lazy[FieldSpec[H]],
                                                tailEncode: Lazy[Fields[T]]): Fields[FieldType[K, H] :: T] = {
    new Fields[FieldType[K, H] :: T] {
      override def fieldnames: List[(String,String)] = (key.value.name + leftSpec.value.modifier, tv.name )  :: tailEncode.value.fieldnames

      override def pr(a: ::[FieldType[K, H], T]): Encode = {
        val pr1: Encode = leftSpec.value.pr(a.head)
        val pr2: Encode = tailEncode.value.pr(a.tail)
        Encode(pr1.main ::: pr2.main, pr1.cont ::: pr2.cont.map({ case (p, b) => (p + 1, b) }))
      }
    }
  }
}

trait Iter[R[_]] {
  def suffix: String

  def headOption[T](a: R[T]): Option[T]

  def pr[T](l: R[T])(implicit fields: Fields[T]): Encode
}


object Iter {

  implicit def opts: Iter[Option] = new Iter[Option] {
    override def suffix: String = "?"

    override def headOption[T](a: Option[T]): Option[T] = a

    override def pr[T](l: Option[T])(implicit fields: Fields[T]): Encode = {

      val size: Int = fields.fieldnames.size
      l match {
        case None => Encode(List.fill(size)("<->"), Nil)
        case Some(t) => fields.pr(t)
      }
    }
  }

  implicit def list: Iter[List] = new Iter[List] {
    override def suffix: String = "[]"

    override def headOption[T](a: List[T]): Option[T] = a.headOption

    override def pr[T](l: List[T])(implicit fields: Fields[T]): Encode = {
      val size: Int = fields.fieldnames.size
      Encode(List.fill(size)("[-]"), List(0 -> l.map(fields.pr)))

    }
  }
}


object Fields extends LowPriorityFields {



  implicit def hnil[L <: HNil]: Fields[L] = new Fields[L] {
    override def fieldnames: List[(String,String)] = Nil

    override def pr(a: L): Encode = Encode(Nil, Nil)
  }

 /* implicit def diveIterValue[K <: Symbol, M[_], T <: HList, H](implicit key: Witness.Aux[K],
                                                               headEncode: Lazy[Fields[H]],
                                                               tailEncode: Lazy[Fields[T]],
                                                               iter: Iter[M]): Fields[FieldType[K, M[H]] :: T] = {
    new Fields[FieldType[K, M[H]] :: T] {
      override def fieldnames: List[(String,String)] = {
        headEncode.value.fieldnames.map({case (a,b) => (key.value.name + iter.suffix + "." + a , b)}) ::: tailEncode.value.fieldnames
      }

      override def pr(a: ::[FieldType[K, M[H]], T]): Encode = {
        val size: Int = headEncode.value.fieldnames.size

        val pr1: Encode = iter.pr(a.head)(headEncode.value)
        val pr2: Encode = tailEncode.value.pr(a.tail)
        Encode(pr1.main ::: pr2.main, pr1.cont ::: pr2.cont.map({ case (i, e) => (i + size, e) }))

      }
    }
  }*/


  implicit def singleIterValue[M[_], T](implicit encode: Lazy[Fields[T]], iter: Iter[M]): Fields[M[T]] =
    new Fields[M[T]] {
      override def pr(a: M[T]): Encode = iter.pr(a)(encode.value)

      override def fieldnames: List[(String, String)] = encode.value.fieldnames.map({ case (a, b) => (iter.suffix + "." + a, b) })
    }

  /*
    implicit def directDive[M[_], H](implicit encode:Lazy[Fields[H]], iter: Iter[M]):Fields[M[H]] = {
      new Fields[M[H]] {
        override def pr(a: M[H]): Encode = ???

        override def fieldnames: List[String] = encode.value.fieldnames
      }
    }
  */

  implicit def diveValue[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K],
                                                     headEncode: Lazy[Fields[H]],
                                                     tailEncode: Lazy[Fields[T]]): Fields[FieldType[K, H] :: T] = {
    new Fields[FieldType[K, H] :: T] {
      override def fieldnames: List[(String,String)] = headEncode.value.fieldnames.map({case (a,b) => (key.value.name + "." + a , b)}) ::: tailEncode.value.fieldnames

      override def pr(a: ::[FieldType[K, H], T]): Encode = {
        val size: Int = headEncode.value.fieldnames.size
        val pr1: Encode = headEncode.value.pr(a.head)
        val pr2: Encode = tailEncode.value.pr(a.tail)
        Encode(pr1.main ::: pr2.main, pr1.cont ::: pr2.cont.map({case (i, v) => (i + size, v)}))
      }
    }
  }

  def fields[A](implicit tmr:Fields[A]) = tmr

  def fieldnames[A](implicit tmr: Fields[A]) = tmr.fieldnames

  def encode[A](a: A)(implicit tmr: Fields[A]) = tmr.pr(a)
}


object ExploreStruct {
  case class A(name: Option[String], yo: String, b: List[B])

  case class B(yo: Option[String], hello: List[String], c: List[C])

  case class C(hello: String, od: D)

  case class D(name: Option[String])

  class YO()

  def main(args: Array[String]) {

    import Fields._
    val strings: List[String] = fieldnames[A].map(_._1)
    assert(strings == List("name?", "yo", "b[].yo?", "b[].hello[]", "b[].c[].hello", "b[].c[].od.name?"))

    val data: (Int, A) = (1, A(None, "abc", List(B(None, List("Yo"), List(C("hello", D(Option("name"))))))))

    import ConvertCsv._

    convertCsv[Map[String,Long]]

    println(fieldnames[(Int,List[(Int, A)])])
    println(encode(data).toRows)


  }
}

trait ConvertCsv[T] {
  def header():Option[List[String]]
  def rows(t:T):List[List[String]]
}

trait ConvertCsvRowEncode[T] extends ConvertCsv[T]

trait LowPriorityConvertCsvRow {
  implicit def noField[T](implicit fieldSpec: FieldSpec[T]):ConvertCsvRowEncode[T] = new ConvertCsvRowEncode[T] {
    override def rows(t: T): List[List[String]] = fieldSpec.pr(t).toRows

    override def header(): Option[List[String]] =  Some(List("-"))
  }
}

object ConvertCsvRowEncode extends  LowPriorityConvertCsvRow {
  implicit def defaultConvertCsv[T](implicit fields: Fields[T]):ConvertCsvRowEncode[T] = {
    new ConvertCsvRowEncode[T] {
      override def rows(t: T): List[List[String]] = fields.pr(t).toRows

      override def header(): Option[List[String]] = Option(fields.fieldnames.map(_._1)).filterNot(_ == Nil)
    }
  }
}


trait LowPriorityConvertCsv {
  implicit def defaultConverCsv[T](implicit rowEncode: ConvertCsvRowEncode[T]):ConvertCsv[T] = rowEncode
}

object ConvertCsv extends LowPriorityConvertCsv {

  def convertCsv[T](implicit convertCsv: ConvertCsv[T]) = convertCsv


  implicit def arrayConvertCsv[T](implicit convert:ConvertCsv[T]):ConvertCsv[Array[T]] = new ConvertCsv[Array[T]] {
    override def rows(t: Array[T]): List[List[String]] = listConvertCsv[T].rows(t.toList)

    override def header(): Option[List[String]] = convert.header()
  }


  implicit def listConvertCsv[T](implicit convert:ConvertCsv[T]):ConvertCsv[List[T]] = {
    new ConvertCsv[List[T]] {
      override def rows(t: List[T]): List[List[String]] = t.flatMap(convert.rows)

      override def header(): Option[List[String]] = convert.header()
    }
  }

  implicit def mapConvertCsv[M[X,Y],A,B](implicit ev: M[A, B] <:< GenMap[A, B],convert:ConvertCsv[(A,B)]):ConvertCsv[M[A,B]] = {
    new ConvertCsv[M[A, B]] {
      override def rows(t: M[A, B]): List[List[String]] = t.toList.flatMap(convert.rows)

      override def header(): Option[List[String]] = convert.header()
    }

  }
}


object ConvertUtils {


  def open(file: File): Unit = {
    val desktop: Desktop = Desktop.getDesktop
    desktop.open(file)
  }

  def newFile(extension: String): File = {
    val name: String = "inspect/" + Random.alphanumeric.take(6).mkString + "." + extension
    val file: File = new File(name)
    file
  }

  def spit(file: File, content: String): Unit = {
    file.getParentFile.mkdirs()
    Files.write(Paths.get(file.getAbsolutePath), content.getBytes(StandardCharsets.ISO_8859_1))

  }


}
