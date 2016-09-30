package com.example.fibbonaci

import java.util.Scanner
import com.example.util.BenchmarkHelper._

/**
  * Created by deil on 30/09/16.
  */
object FibbonaciNumbers extends App{

  def calculateFibbFor(n: Int): Int = n match {
    case number if number <= 1 => number
    case number if number > 1 => calculateFibbFor(number -1) + calculateFibbFor(number -2)
  }

  val  s = new Scanner(System.in)
  val a = s.nextInt()

  time {
    println{
      s"Fibbonaci for number $a:" + calculateFibbFor(a)
    }
  }
}
