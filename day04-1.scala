/*
--- Day 4: Repose Record ---

You've sneaked into another supply closet - this time, it's across from the prototype suit manufacturing lab. You need to sneak inside and fix the issues with the suit, but there's a guard stationed outside the lab, so this is as close as you can safely get.

As you search the closet for anything that might help, you discover that you're not the first person to want to sneak in. Covering the walls, someone has spent an hour starting every midnight for the past few months secretly observing this guard post! They've been writing down the ID of the one guard on duty that night - the Elves seem to have decided that one guard was enough for the overnight shift - as well as when they fall asleep or wake up while at their post (your puzzle input).

For example, consider the following records, which have already been organized into chronological order:

[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up

Timestamps are written using year-month-day hour:minute format. The guard falling asleep or waking up is always the one whose shift most recently started. Because all asleep/awake times are during the midnight hour (00:00 - 00:59), only the minute portion (00 - 59) is relevant for those events.

Visually, these records show that the guards are asleep at these times:

Date   ID   Minute
            000000000011111111112222222222333333333344444444445555555555
            012345678901234567890123456789012345678901234567890123456789
11-01  #10  .....####################.....#########################.....
11-02  #99  ........................................##########..........
11-03  #10  ........................#####...............................
11-04  #99  ....................................##########..............
11-05  #99  .............................................##########.....

The columns are Date, which shows the month-day portion of the relevant day; ID, which shows the guard on duty that day; and Minute, which shows the minutes during which the guard was asleep within the midnight hour. (The Minute column's header shows the minute's ten's digit in the first row and the one's digit in the second row.) Awake is shown as ., and asleep is shown as #.

Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up. For example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.

If you can figure out the guard most likely to be asleep at a specific time, you might be able to trick that guard into working tonight so you can have the best chance of sneaking in. You have two strategies for choosing the best guard/minute combination.

Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?

In the example above, Guard #10 spent the most minutes asleep, a total of 50 minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas any other minute the guard was asleep was only seen on one day).

While this example listed the entries in chronological order, your entries are in the order you found them. You'll need to organize them before they can be analyzed.

What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 10 * 24 = 240.)

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
      val sleptPerShift = guard.shifts.map { shift =>
        shift.minutes.sum
      }
      val sumByMinute = guard.shifts.map(_.minutes).reduce { (s1, s2) =>
        s1.zip(s2).map { case (x, y) => x + y }
      }
      val commonMinute = sumByMinute.indexOf(sumByMinute.max)
      (guard.id, sleptPerShift.sum, commonMinute)
    }
  }.toArray

  val targetGuard = results.sortWith { (left, right) =>
    left._2 > right._2
  }.head

  val endTime = System.nanoTime()
  println(targetGuard)
  println(s"${targetGuard._1 * targetGuard._3}")
  println(s"This took ${(endTime - startTime) / 1000000} msecs")

