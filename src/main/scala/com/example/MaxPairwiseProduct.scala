
import java.util.Scanner

/**
  * Created by deil on 10/09/16.
  */
object MaxPairwiseProduct {
  def main(args: Array[String]) {

    val s = new Scanner(System.in)
    val a = s.nextInt()
    val numbers: Array[Int] = Array.fill(a){0}
    for(i <- 0 until a) {
      numbers(i) = s.nextInt()
    }


    println(findMaxPairwise(numbers))
  }

  def findMaxPairwise(numbers: Seq[Int]): Long = {
    val n = numbers.size
    var maxIndex1 = -1
    var maxIndex2 = -1
    for{ i <- 0 until n } yield {
      if( maxIndex1 == -1 || numbers(i) > numbers(maxIndex1)) maxIndex1 = i
    }
    for{ j <- 0 until n if j != maxIndex1} yield {
      if(maxIndex2 == -1 || numbers(j) > numbers(maxIndex2)) maxIndex2 = j
    }
    numbers(maxIndex1).asInstanceOf[Long] * numbers(maxIndex2)
  }

}
