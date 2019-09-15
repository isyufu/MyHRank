package practice.interview_preparation_kit

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WarmUpChallenges extends FunSuite{

  test("CountingValleys") {
    //8
    //UDDDUDUU
    def countingValleys(n: Int, s: String): Int = {
      var res = 0
      s.map{
        case 'U' => 1
        case 'D' => -1
      }.foldLeft(0){
        case (sum, x) => if (sum == 0 && x == -1) res+=1; sum + x
      }
      res
    }

    assert(countingValleys(8, "UDDDUDUU") ==  1)
  }

  test("jumpingOnClouds") {
    def jumpingOnClouds(c: Array[Int]): Int = {
      import scala.annotation.tailrec
      @tailrec
      def f(a:Array[Int], s:Int): Int ={
        if(a.isEmpty)
          s
        else if(a.length >= 2 && a(1) == 0)
          f(a.drop(2), s+1)
        else
          f(a.drop(1), s+1)
      }

      f(c.drop(1), 0)
    }

    jumpingOnClouds(Array(0, 0, 0, 1, 0, 0)) == 3
    jumpingOnClouds(Array(0, 0, 0, 0, 1, 0)) == 3

  }

  test("repeatedString") {
    def repeatedString(s: String, n: Long): Long = {
      if(s.isEmpty)
        0
      else {
        val h = 'a'
        val countFullStr:Long = n / s.length
        val inStr = s.count(_ == h)
        val tailL = (n % s.length).toInt
        val tail =  s.take(tailL)
        val inTail:Long = tail.count(_ == h)
        //      println(s"$h $countFullStr $inStr $tailL $tail  $inTail")
        (countFullStr * inStr) + inTail
      }
    }


    val s = "kmretasscityylpdhuwjirnqimlkcgxubxmsxpypgzxtenweirknjtasxtvxemtwxuarabssvqdnktqadhyktagjxoanknhgilnm"

    require(repeatedString(s, 736778906400L) == 51574523448L)
    require(repeatedString("x",970770) == 0)
    require(repeatedString("",970770) == 0)

    require(repeatedString("cfimaakj",554045874191L  ) == 138511468548L)
    require(repeatedString("aba", 10) == 7L)
    require(repeatedString("a", 1000000000000L) == 1000000000000L)
  }

  test("sockMerchant") {
    def sockMerchant(n: Int, ar: Array[Int]): Int = {
      import scala.collection.mutable
      val m = mutable.HashMap[Int, Int]().withDefault(_ => 0)
      ar.foreach(x =>
        m.update(x, m(x)+1)
      )
      m.values.map(_/2).sum
    }


    sockMerchant(9, Array(10, 20, 20, 10, 10, 30, 50, 10, 20)) == 3

  }
}
