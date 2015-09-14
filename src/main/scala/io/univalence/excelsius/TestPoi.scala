package io.univalence.excelsius

import java.awt
import java.awt.Desktop
import java.io.{File, FileOutputStream}

import org.apache.poi.ss.usermodel.{FillPatternType, HorizontalAlignment, BorderStyle}
import org.apache.poi.xssf.usermodel._

import scala.util.Random

import org.apache.spark.rdd.RDD



trait ConvertExcel[T] {
  def pr(a:T):List[Encode]
  def fieldnames:List[(String,String)]

}

trait LowPriorityConvertExcel {

  implicit def defaultConvertExcel[T](implicit fields: Fields[T]):ConvertExcel[T] =new ConvertExcel[T] {
    override def pr(a: T): List[Encode] = List(fields.pr(a))

    override def fieldnames: List[(String, String)] = fields.fieldnames
  }
}

object ConvertExcel extends LowPriorityConvertExcel {


  implicit def listConvertExcel[T](implicit fields:Fields[T]):ConvertExcel[List[T]] = new ConvertExcel[List[T]] {
    override def pr(a: List[T]): List[Encode] = a.map(fields.pr)

    override def fieldnames: List[(String, String)] = fields.fieldnames
  }

  implicit def rdd[T](implicit convertExcel: ConvertExcel[List[(T,Long)]]):ConvertExcel[RDD[T]] = {
      new ConvertExcel[RDD[T]] {
        override def pr(a: RDD[T]): List[Encode] = convertExcel.pr(a.countByValue().toList)

        override def fieldnames: List[(String, String)] = convertExcel.fieldnames
      }
    }

}



object TestPOI {

  val color: List[awt.Color] = {

    val n = 6
    val seed:Long = 0L
    val colors: List[awt.Color] = (for (i <- 0 until  n) yield {
      java.awt.Color.getHSBColor((1f / (n + 1)) * i, 0.3f, 1f)
    }).toList

    new Random(seed).shuffle(colors)
  }

  val colors: Iterator[awt.Color] = Iterator.continually(color).flatten



  def toExcel[A](a:A, file:File)(implicit convertExcel: ConvertExcel[A]) = {



    file.getParentFile.mkdirs()


    val medium: BorderStyle = BorderStyle.HAIR

    val out = new FileOutputStream(file)
    // create a new workbook
    val wb = new XSSFWorkbook()
    // create a new sheet
    val sheet = wb.createSheet()

    // declare a row object reference





    def nextBackGroundColor() = colors.next()


    val createRow  = sheet.createRow(0)



    val headerStyle = wb.createCellStyle()


    headerStyle.setAlignment(HorizontalAlignment.RIGHT)



    convertExcel.fieldnames.zipWithIndex.foreach({case ((n,_),i) =>
      val createCell: XSSFCell = createRow.createCell(i)
      createCell.setCellStyle(headerStyle)
      createCell.setCellValue(n)})






    def addLeftRightBorder(l:List[(String,XSSFCellStyle)]):List[(String,XSSFCellStyle)] = {


      l match {
        case Nil => Nil
        case s :: Nil => {

          val createCellStyle: XSSFCellStyle = wb.createCellStyle()
          createCellStyle.cloneStyleFrom(s._2)

          createCellStyle.setBorderLeft(medium)
          createCellStyle.setBorderRight(medium)

          List((s._1, createCellStyle))
        }

        case first :: (xs :+ last) => {
          val firstStyle: XSSFCellStyle = wb.createCellStyle()
          firstStyle.cloneStyleFrom(first._2)
          firstStyle.setBorderLeft(medium)

          val lastStyle = wb.createCellStyle()
          lastStyle.cloneStyleFrom(last._2)
          lastStyle.setBorderRight(medium)
          (first._1, firstStyle) :: (xs :+ (last._1, lastStyle))
        }

      }
    }




    def toRowList (e:Encode):List[List[(String, XSSFCellStyle)]] = {


      val firstStyle: XSSFCellStyle = wb.createCellStyle()

      firstStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND)
      val color1: XSSFColor = new XSSFColor(nextBackGroundColor())

      firstStyle.setFillForegroundColor(color1)



      val firstFontStyle: XSSFFont = wb.createFont()
      firstFontStyle.setBold(true)

      firstFontStyle.setBoldweight(2)



      firstStyle.setBorderTop(medium)


      firstStyle.setFont(firstFontStyle)




      val restStyle: XSSFCellStyle = wb.createCellStyle()


      val restFontStyle: XSSFFont = wb.createFont()



      restFontStyle.setColor(new XSSFColor(awt.Color.GRAY))
      restFontStyle.setItalic(true)


      restStyle.setFont(restFontStyle)
      restStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND)
      restStyle.setFillForegroundColor(color1)





      import e._

      val first = addLeftRightBorder(e.main.map((_,firstStyle)))
      lazy val mainC = e.main.map((_,restStyle))



      first :: cont.flatMap({ case (i, l) => {
        val take = addLeftRightBorder(mainC.take(i))
        l.flatMap(e => {
          val map: List[List[(String, XSSFCellStyle)]] = toRowList(e).map(l => {
            take ::: l ::: addLeftRightBorder(mainC.drop(i + l.size))
          })
          map
        })
      }
      })
    }




    def effectExcel(e:List[Encode]):Unit = {

      val rowList: List[List[(String, XSSFCellStyle)]] = e.flatMap(toRowList)

      rowList.zipWithIndex.foreach({ case (l, i) => {
        val r = sheet.createRow(i + 1)

        l.zipWithIndex.foreach({ case ((v, s), ii) => {
          val c = r.createCell(ii)
          c.setCellStyle(s)
          c.setCellValue(v)

        }
        })

      }
      })


    }


    effectExcel(convertExcel.pr(a))


    wb.write(out)

    out.close()

    //Desktop.getDesktop.open(file)


  }



}
