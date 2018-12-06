/*
--- Day 5: Alchemical Reduction ---
--- Part Two ---

Time to improve the polymer.

One of the unit types is causing problems; it's preventing the polymer from collapsing as much as it should. Your goal is to figure out which unit type is causing the most problems, remove all instances of it (regardless of polarity), fully react the remaining polymer, and measure its length.

For example, again using the polymer dabAcCaCBAcCcaDA from above:

    Removing all A/a units produces dbcCCBcCcD. Fully reacting this polymer produces dbCBcD, which has length 6.
    Removing all B/b units produces daAcCaCAcCcaDA. Fully reacting this polymer produces daCAcaDA, which has length 8.
    Removing all C/c units produces dabAaBAaDA. Fully reacting this polymer produces daDA, which has length 4.
    Removing all D/d units produces abAcCaCBAcCcaA. Fully reacting this polymer produces abCBAc, which has length 6.

In this example, removing all C/c units was best, producing the answer 4.

What is the length of the shortest polymer you can produce by removing all units of exactly one type and fully reacting the result?

 */

import scala.io.Source

  def react(input: String): List[Char] = {
    def isReactive(a: Char, b: Char): Boolean =
      (a != b) && (a.toUpper == b.toUpper) && (a.toLower == b.toLower)

    input.foldLeft(List[Char]()) {
      case (leftSide, char)
        if (leftSide.isEmpty || !isReactive(leftSide.head, char)) =>
        char :: leftSide
      case (leftSide, char) => leftSide.tail
    }
  }

  def removePolymer(input: String, letter: String): String =
    input
      .replaceAll(letter, "")
      .replaceAll(letter.toUpperCase, "")

  val startTime = System.nanoTime()
  val inputFile = "day05input.txt"
  val input = Source
    .fromFile(inputFile)
    .getLines
    .next
  val result = "abcdefghijklmnopqrstuvwxyz".map { char =>
    react(removePolymer(input, char.toString)).length
  }.min

  val endTime = System.nanoTime()
  println(result)
  println(s"This took ${(endTime - startTime) / 1000000} msecs")
