package com.example.gcd

import java.util.Scanner

import com.example.util.BenchmarkHelper._

import scala.math.BigInt

/**
  * Greatest common divider using Euclidian algorithm
  */
object GCDEuclidian extends App{

  def calculateGCDfor(a: BigInt, b: BigInt): BigInt = {
    if (b == BigInt(0)) a
    else {
      if(a > b) calculateGCDfor(b, a - b * (a / b))
      else calculateGCDfor(a, b - a * (b / a))
    }
  }

  val  s = new Scanner(System.in)
  val a = s.nextBigInteger()
  val b = s.nextBigInteger()

  //Test for example for a = 74356764380* 13   &   b = 74356764380 * 5
  time {
    println{
      s"GCD for numbers $a and $b is: \t" + calculateGCDfor(a, b)
    }
  }
}
