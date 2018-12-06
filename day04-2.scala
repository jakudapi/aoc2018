/*
--- Day 4: Repose Record ---
Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?

In the example above, Guard #99 spent minute 45 asleep more than any other guard or minute - three times in total. (In all other cases, any guard spent any minute asleep at most twice.)

What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 99 * 45 = 4455.)
 */

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListMap}

  case class Record(dt: LocalDateTime, action: String)

  case class Guard(id: Int, shifts: ArrayBuffer[Shift] = ArrayBuffer[Shift]())

  case class Shift(
      dt: LocalDate,
      minutes: Array[Int] = Array.ofDim[Int](60),
      var lastFellAsleep: Int = -1
  )

  val Format = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  val utcTz = ZoneId.of("UTC")

  def parse(input: String): Record = {
    val (date, note) = input.splitAt(18)
    val dt: LocalDateTime =
      LocalDateTime.parse(date.drop(1).dropRight(1), Format)
    Record(dt, note.tail)
  }

  val startTime = System.nanoTime()
  val inputFile = "day04input.txt"

  val input: Array[Record] = Source
    .fromFile(inputFile)
    .getLines
    .toArray
    .map(parse)
    .sortWith { (left, right) =>
      left.dt.isBefore(right.dt)
    }

  val guards = ListMap[Int, Guard]()
  var currentGuard = -1

  for (record <- input) {
    if (record.action(0) == 'G') {
      val id = record.action.tail.split(" ").apply(1).drop(1).toInt
      if (!guards.contains(id)) guards(id) = Guard(id)
      currentGuard = id
      guards(currentGuard).shifts //add a new shift
        .append(Shift(record.dt.toLocalDate.plusDays(1)))
    } else if (record.action(0) == 'f') {
      val fellAsleep = record.dt.getMinute
      guards(currentGuard).shifts.last.minutes(fellAsleep) = 1  // mark that minute as asleep
      guards(currentGuard).shifts.last.lastFellAsleep = fellAsleep

    } else { //guard wakes up
      val wokeUp = record.dt.getMinute
      val wasAsleep = guards(currentGuard).shifts.last.lastFellAsleep + 1
      (wasAsleep until wokeUp).map { i =>
        guards(currentGuard).shifts.last.minutes(i) = 1 // fill in between minutes as asleep
      }
    }

  }

  val results: Array[(Int, Int, Int)] = guards.values.map { guard =>
    {
      val sumByMinute = guard.shifts.map(_.minutes).reduce { (s1, s2) =>
        s1.zip(s2).map { case (x, y) => x + y }
      }
      val frequencyMinute = sumByMinute.max
      val commonMinute = sumByMinute.indexOf(frequencyMinute)

      (guard.id, frequencyMinute, commonMinute)
    }
  }.toArray

  val targetGuard = results.sortWith { (left, right) =>
    left._2 > right._2
  }.head

  val endTime = System.nanoTime()
  println(targetGuard)
  println(s"${targetGuard._1 * targetGuard._3}")
  println(s"This took ${(endTime - startTime) / 1000000} msecs")
