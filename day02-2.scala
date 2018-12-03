/*
--- Day 2: Inventory Management System ---
--- Part Two ---

Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz

The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)


 */

import scala.io.Source
import util.control.Breaks._

  def compare(leftBox: String, rightBox: String): Boolean = {
    if (leftBox
          .zip(rightBox)
          .map { case (a, b) => if (a != b) 1 else 0 }
          .sum == 1) true
    else false
  }

  val startTime = System.nanoTime()
  val inputFile = "day02input.txt"

  val input = Source
    .fromFile(inputFile)
    .getLines
    .toArray

  breakable {
    for (combo <- input.combinations(2)) {
      if (compare(combo(0), combo(1))) {
        println(combo(0).intersect(combo(1)))
        break
      }
    }
  }
  val endTime = System.nanoTime()

  println(s"This took ${(endTime - startTime) / 1000000} msecs")
