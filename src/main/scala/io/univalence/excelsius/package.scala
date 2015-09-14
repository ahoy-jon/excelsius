package io.univalence

import java.io.File

package object excelsius {





  implicit class InpectOps[A](data: A) {


    def showDf()(implicit convertExcel: ConvertExcel[A]) = excel()


    def excel()(implicit convert:  ConvertExcel[A]): Unit = {

      val file: File = ConvertUtils.newFile("xlsx")
      

      TestPOI.toExcel(data, file)
    }
  }

}
