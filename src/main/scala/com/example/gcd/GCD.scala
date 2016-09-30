package com.example.gcd

import java.util.Scanner

import com.example.util.BenchmarkHelper._

/**
  * Greatest common divider
  */
object GCD extends App{

  def calculateGCDfor(a: BigInt, b: BigInt): BigInt = {
    var biggestGCD: BigInt = 1
    for( i <- BigInt(1) to a + b) {
      if(a % i == 0 && b % i == 0) biggestGCD = i
    }
    biggestGCD
  }

  val  s = new Scanner(System.in)
  val a = s.nextBigInteger()
  val b = s.nextBigInteger()

  time {
    println{
      s"GCD for numbers $a and $b is: \t" + calculateGCDfor(a, b)
    }
  }
}
