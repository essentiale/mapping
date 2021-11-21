name := "mapping"

val projectOrganization = "io.github.essentiale"
val projectVersion      = "1.0.3"
val projectHomepage     = Some(url("https://github.com/essentiale/mapping"))
val projectScmInfo      = Some(ScmInfo(url("https://github.com/essentiale/mapping"), "git@github.com:essentiale/mapping.git"))
val projectDevelopers   = List(Developer("Volchik", "Volchik", "1barboss7@gmail.com", url("https://github.com/essentiale")))
val projectLicense      = ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

lazy val `mapping` = project
  .in(file("."))
  .settings(organization := projectOrganization)
  .settings(homepage := projectHomepage)
  .settings(version := projectVersion)
  .settings(scmInfo := projectScmInfo)
  .settings(developers := projectDevelopers)
  .settings(licenses += projectLicense)
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
  .settings(homepage := projectHomepage)
  .settings(version := projectVersion)
  .settings(scmInfo := projectScmInfo)
  .settings(developers := projectDevelopers)
  .settings(licenses += projectLicense)
  .settings(publish / skip := true)
  .settings(scalacOptions += "-Ywarn-macros:after")
  .settings(scalaVersion := "2.13.2")
  .dependsOn(`essentiale-mapping-main`)
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9")

lazy val `essentiale-mapping-main` = project
  .in(file("main"))
  .settings(publishMavenStyle := true)
  .settings(publishTo := sonatypePublishTo.value)
  .settings(sonatypeCredentialHost := "s01.oss.sonatype.org")
  .settings(sonatypeRepository := "https://s01.oss.sonatype.org/service/local")
  .settings(name := "mapping")
  .settings(scalaVersion := "2.13.2")
  .settings(organization := projectOrganization)
  .settings(homepage := projectHomepage)
  .settings(version := projectVersion)
  .settings(scmInfo := projectScmInfo)
  .settings(developers := projectDevelopers)
  .settings(licenses += projectLicense)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)
