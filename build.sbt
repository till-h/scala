name := "arithmeticparser"

version := "0.1"

scalaVersion := "2.10.4"

resolvers += "SonaType" at "https://oss.sonatype.org/content/groups/public"

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.10" % "2.2.1" % "test",
                            "net.databinder.dispatch" %% "dispatch-core" % "0.11.2")
