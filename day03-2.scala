/*
--- Day 3: No Matter How You Slice It ---
--- Part Two ---

Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of fabric with any other claim. If you can somehow draw attention to it, maybe the Elves will be able to make Santa's suit after all!

For example, in the claims above, only claim 3 is intact after all claims are made.

What is the ID of the only claim that doesn't overlap?


 */

import scala.io.Source
import scala.collection.mutable

  case class Claim(
      elf: Int,
      fromLeft: Int,
      fromTop: Int,
      width: Int,
      height: Int,
      var overlapped: Boolean = false
  )

  case class Point(claims: mutable.Set[Int] = mutable.Set[Int]()) {
    override def toString(): String = {
      if (claims.size > 1) "X"
      else if (claims.size == 0) "."
      else claims.last.toString
    }
  }

  def parseInput(input: String): Claim = {
    val inputSplit: Array[String] = input.split(" ")
    val elf: Int = inputSplit(0).drop(1).toInt
    val start = inputSplit(2).split(",").map(_.stripSuffix(":")).map(_.toInt)
    val size = inputSplit(3).split("x").map(_.toInt)
    Claim(elf, start(0), start(1), size(0), size(1))
  }

  val startTime = System.nanoTime()
  val inputFile = "day03input.txt"
  val FabricX = 1000
  val FabricY = 1000

  val fabric = Array.ofDim[Point](FabricX, FabricY)
  for (x <- (0 until FabricX); y <- (0 until FabricY)) fabric(x)(y) = Point()

  val input = Source
    .fromFile(inputFile)
    .getLines
    .toArray
    .map(parseInput)

  for (claim <- input) {
    for (width <- (0 until claim.width);
         height <- (0 until claim.height)) {
      val x = claim.fromLeft + width
      val y = claim.fromTop + height
      if (fabric(y)(x).claims.size == 1)
        input(fabric(y)(x).claims.last - 1).overlapped = true
      if (fabric(y)(x).claims.size >= 1) claim.overlapped = true
      fabric(y)(x).claims.add(claim.elf)
    }
  }
  println(input.filter(_.overlapped == false)(0))
  val endTime = System.nanoTime()

  println(s"This took ${(endTime - startTime) / 1000000} msecs")

