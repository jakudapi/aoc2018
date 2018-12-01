/*
--- Day 1: Chronal Calibration ---
--- Part Two ---

You notice that the device repeats the same frequency change list over and over. To calibrate the device, you need to find the first frequency it reaches twice.

For example, using the same list of changes above, the device would loop as follows:

    Current frequency  0, change of +1; resulting frequency  1.
    Current frequency  1, change of -2; resulting frequency -1.
    Current frequency -1, change of +3; resulting frequency  2.
    Current frequency  2, change of +1; resulting frequency  3.
    (At this point, the device continues from the start of the list.)
    Current frequency  3, change of +1; resulting frequency  4.
    Current frequency  4, change of -2; resulting frequency  2, which has already been seen.

In this example, the first frequency reached twice is 2. Note that your device might need to repeat its list of frequency changes many times before a duplicate frequency is found, and that duplicates might be found while in the middle of processing the list.

Here are other examples:

    +1, -1 first reaches 0 twice.
    +3, +3, +4, -2, -4 first reaches 10 twice.
    -6, +3, +8, +5, -6 first reaches 5 twice.
    +7, +7, -2, -7, -4 first reaches 14 twice.

What is the first frequency your device reaches twice?


 */

  import scala.io.Source
  import scala.collection.mutable.Set
  import util.control.Breaks._


  val startTime = System.nanoTime()
  val inputFile = "day01input.txt"
  val tracker: Set[Int] = Set(0)
  var frequency = 0
  var endOfInput = true

  def modify(acc: Int, op: String, value: Int): Int = {
    op match {
      case "+" => acc + value
      case "-" => acc - value
    }
  }

  val input: Array[(String, Int)] = Source
    .fromFile(inputFile)
    .getLines
    .toArray
    .map(_.splitAt(1)) // ("+", "12")
    .map { tuple => (tuple._1, tuple._2.toInt) } // ("+", 12)

  do {
    breakable{
      for ((sign, value) <- input) {
        frequency = modify(frequency, sign, value)
        if (tracker.contains(frequency)) {
          endOfInput = false
          break
        }
        else tracker.add(frequency)
      }
    }
  } while(!tracker.contains(frequency) || endOfInput)

  val endTime = System.nanoTime()
  println(frequency)
  println(s"This took ${(endTime-startTime)/1000000} msecs")
