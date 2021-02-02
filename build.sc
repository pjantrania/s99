import mill._, scalalib._
object s99 extends ScalaModule {
  def scalaVersion = "2.13.1"

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:3.2.2",
      ivy"org.scalatest::scalatest:3.2.2"
    )

    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}
