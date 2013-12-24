import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "youtube-play"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
//    jdbc,
//    anorm,
    "org.reactivemongo" %% "play2-reactivemongo" % "0.10.0"
//     exclude("org.scala-stm", "scala-stm_2.10.0")
  )
  
  


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
    testOptions in Test += Tests.Argument("junitxml", "console")
  )

}
