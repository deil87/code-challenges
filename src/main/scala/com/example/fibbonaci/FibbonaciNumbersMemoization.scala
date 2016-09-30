package com.example.fibbonaci

import java.util.Scanner

import com.example.util.BenchmarkHelper._
import com.example.util.Memo
import com.example.util.Memo.==>

/**
  * It calculates fibb(45) for 113 ms instead of 5,5 s
  */
object FibbonaciNumbersMemoization extends App{

  lazy val calculateFibbFor: Int ==> BigInt = Memo {
    case number if number <= 1 => number
    case number if number > 1 => calculateFibbFor(number - 1) + calculateFibbFor(number -2)
  }

  val  s = new Scanner(System.in)
  val a = s.nextInt()


  time {
    println{
      s"Fibbonaci for number $a:" +
          calculateFibbFor(a)
    }
  }
}
