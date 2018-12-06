/*

--- Day 3: No Matter How You Slice It ---

The Elves managed to locate the chimney-squeeze prototype fabric for Santa's suit (thanks to someone who helpfully wrote its box IDs on the wall of the warehouse in the middle of the night). Unfortunately, anomalies are still affecting them - nobody can even agree on how to cut the fabric.

The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.

Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:

    The number of inches between the left edge of the fabric and the left edge of the rectangle.
    The number of inches between the top edge of the fabric and the top edge of the rectangle.
    The width of the rectangle in inches.
    The height of the rectangle in inches.

A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall. Visually, it claims the square inches of fabric represented by # (and ignores the square inches of fabric represented by .) in the diagram below:

...........
...........
...#####...
...#####...
...#####...
...#####...
...........
...........
...........

The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For example, consider the following claims:

#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2

Visually, these claim the following areas:

........
...2222.
...2222.
.11XX22.
.11XX22.
.111133.
.111133.
........

The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not overlap either of them.)

If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric are within two or more claims?

 */

import scala.io.Source
import scala.collection.mutable

  case class Claim(elf: Int,
                   fromLeft: Int,
                   fromTop: Int,
                   width: Int,
                   height: Int)

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
      fabric(y)(x).claims.add(claim.elf)
    }
  }

  var counter = 0
  for (x <- (0 until FabricX); y <- (0 until FabricY)) {
    if (fabric(x)(y).claims.size > 1) counter = counter + 1
  }

  val endTime = System.nanoTime()
  println(counter)
  println(s"This took ${(endTime - startTime) / 1000000} msecs")

