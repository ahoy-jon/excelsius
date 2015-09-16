name := "excelsius"

organization := "io.univalence"

scalaVersion := "2.11.7"

libraryDependencies += "com.chuusai" % "shapeless_2.11" % "2.2.5"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7"

libraryDependencies += "org.apache.poi" % "poi" % "3.12"

libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.12"

libraryDependencies += "org.apache.spark" % "spark-core_2.11" % "1.4.0" % "compile"

publishTo := Some(Resolver.file("file",  new File( "/Users/jon/Project/m2-repo")) )
