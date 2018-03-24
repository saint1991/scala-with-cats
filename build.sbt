
lazy val root = (project in file("cats"))
  .settings(
    name := "cats",
    version := "0.1.0",
    scalaVersion := "2.12.4",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.1.0",
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    ),
    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-deprecation",
      "-unchecked",
      "-feature",
      "-language:higherKinds",
      "-Xlint",
      "-Xfatal-warnings",
      "-Ypartial-unification"
    ),
    transitiveClassifiers := Seq("sources"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  )
