credentials += Credentials(Path.userHome / ".sbt" / "0.13" / "sonatype.sbt")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.10")