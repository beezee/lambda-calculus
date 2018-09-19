// give the user a nice default project!
lazy val root = (project in file(".")).
  settings(
    organization := "bz",
    scalaVersion := "2.12.5",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.3.0-M24",
      "io.frees" %% "iotaz-core" % "0.3.10" ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8", // yes, this is 2 args
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:-unused,_",
      "-Yno-adapted-args",
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:imports,privates,locals",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ybackend-parallelism", java.lang.Runtime.getRuntime.availableProcessors.toString,
      "-Ycache-plugin-class-loader:last-modified",
      //"-Xlog-implicits",
      "-Ycache-macro-class-loader:last-modified"),
    name := "lambda-calculus"
  )
