name := "mapping"

val projectOrganization = "io.github.essentiale"
val projectVersion = "1.0.0"

//ThisBuild / useCoursier := false
//Set ttl of snapshots so they always refresh
javaOptions += "-Dcoursier.ttl=0s"

lazy val `mapping` = project
  .in(file("."))
  .settings(organization := projectOrganization)
  .settings(version := projectVersion)
  .settings(publish / skip := true)
  .settings(scalaVersion := "2.13.2")
  .aggregate(
    `essentiale-mapping-main`,
    `essentiale-mapping-test`
  )

lazy val `essentiale-mapping-test` = project
  .in(file("test"))
  .settings(name := "mapping-test")
  .settings(organization := projectOrganization)
  .settings(version := projectVersion)
  .settings(publish / skip := true)
  .settings(scalacOptions += "-Ywarn-macros:after")
  .settings(scalaVersion := "2.13.2")
  .dependsOn(`essentiale-mapping-main`)
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9")


lazy val `essentiale-mapping-main` = project
  .in(file("main"))
  .settings(name := "mapping")
  .settings(scalaVersion := "2.13.2")
  .settings(organization := projectOrganization)
  .settings(version := projectVersion)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)

