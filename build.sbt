import Dependencies._

ThisBuild / scalaVersion     := "3.1.0"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "com.ktedon"
ThisBuild / organizationName := "ktedon"

lazy val root = (project in file("."))
  .settings(
    name := "dumbledore",
    libraryDependencies += scalaTest % Test
  )

// https://mvnrepository.com/artifact/org.typelevel/cats-core
libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
// https://mvnrepository.com/artifact/org.typelevel/cats-collections-core
libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.3"
// https://mvnrepository.com/artifact/org.typelevel/cats-parse
libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.6"
