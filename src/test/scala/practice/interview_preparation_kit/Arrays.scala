package practice.interview_preparation_kit

import org.scalatest._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Arrays extends FunSuite {
  test("rotLeft"){
    def rotLeft(a: Array[Int], d: Int): Array[Int] = {
      a.drop(d) ++ a.take(d)
    }

    assert(rotLeft((1 to 5).toArray, 4).deep == Array(5,1,2,3,4).deep)

  }

  test("hourglassSum") {
    def hourglassSum(arr: Array[Array[Int]]): Int = {
      val maxStartInd = arr.length - 2 -1
      def sumFromCenter(r0:Int, c0:Int) = {
        val hourglassSum = (
          for {
            r <- 0 to 2
            c <- 0 to 2
            if !(c==0 && r==1) && !(c==2 && r==1)
          } yield arr(r0+r)(c0+c)
          ).sum
        hourglassSum
      }

      val maxSum = (
        for {
          r0 <- 0 to maxStartInd
          c0 <- 0 to maxStartInd
        } yield sumFromCenter(r0, c0)
        ).max

      maxSum
    }

    val sum1 = 28
    val a1 = Array(
      Array(-9, -9, -9,  1, 1, 1),
      Array(0, -9,  0,  4, 3, 2),
      Array(-9, -9, -9,  1, 2, 3),
      Array(0,  0,  8,  6, 6, 0),
      Array(0,  0,  0, -2, 0, 0),
      Array(0,  0,  1,  2, 4, 0))

    val sum2 = 19
    val a2 = Array(
      Array(1, 1, 1, 0, 0, 0),
      Array(0, 1, 0, 0, 0, 0),
      Array(1, 1, 1, 0, 0, 0),
      Array(0, 0, 2, 4, 4, 0),
      Array(0, 0, 0, 2, 0, 0),
      Array(0, 0, 1, 2, 4, 0))

    assert(hourglassSum(a1) == sum1)
    assert(hourglassSum(a2) == sum2)
  }
}
