package foo.bar.baz

import scala.io.StdIn

/**
  * Created by sally on 09/09/2017.
  */
object DateDifference {
  def main(args: Array[String]): Unit = {
    println("Please input a pair of dates (in format : DD MM YYYY, DD MM YYYY), type 'END' to end")
    startRound()
    def startRound(): Unit = {
      val inputString = StdIn.readLine("your input : ")
      inputString match {
        case "END" => println("End of it.")
        case input => {
          println(processInput(input))
          startRound()
        }
      }
    }
  }

  def processInput(input: String): String = {
    input.split(",").toList match {
      case (earlyDate :: lateDate :: Nil) =>
        verifyDateString(earlyDate.trim) match {
          case Some(earlyList) =>
            verifyDateString(lateDate.trim) match {
              case Some(lateList) =>
                getDatesDiff(earlyList, lateList) match {
                  case Some(diff) => input + ", " + diff.toString
                  case _ => "first date is later than second date"
                }
              case _ => "Invalid late date"
            }
          case _ => "Invalid early date"
        }
      case _ => "Error in format"
    }
  }

  def verifyDateString(date: String): Option[Seq[Int]] = {
    date.trim.split(" ") match {
      case (componentList) if componentList.length == 3 =>
        val intList = componentList.flatMap { component =>
          try {
            Some (component.toInt)
          } catch {
            case e: Exception =>
              println("Not invalid integer: " + component)
              None
          }
        }.toList
        intList match {
          case (day :: month :: year :: Nil) =>
            if (year >= 1900 && year <= 2010) {
              if (day > 0 && ((Seq(1,3,5,7,8,10,12).contains(month) && day < 32) || (Seq(4,6,9,11).contains(month) && day < 31) || (month == 2 && ((day < 29 || (day == 29 && year % 4 == 0)))))) {
                Some(Seq(year, month, day))
              } else None
            } else {
              println("YYYY is not in correct range")
              None
            }
          case _ => None
        }
      case _ => None
    }
  }

  def getDatesDiff(earlyList: Seq[Int], lateList: Seq[Int]): Option[Int] = {
    if (lateList(0) < earlyList(0) || (lateList(0) == earlyList(0) && lateList(1) < earlyList(1)) || (lateList(0) == earlyList(0) && lateList(1) == earlyList(1) && lateList(2) < earlyList(2))) {
      None
    } else {
      val earlyDays = daysToStart(earlyList(1), earlyList(2), earlyList(0) % 4 == 0)
      val lateDays = daysToStart(lateList(1), lateList(2), lateList(0) % 4 == 0)
      val yearDiff = yearDifference(earlyList(0), lateList(0), 0)
      Some(yearDiff + lateDays - earlyDays)
    }
  }
  def daysToStart(month: Int, diff: Int, specialFeb: Boolean): Int = {
    (month - 1) match {
      case monthInt if Seq(1,3,5,7,8,10,12).contains(monthInt) =>
        daysToStart(month - 1, diff + 31, specialFeb)
      case monthInt if Seq(4,6,9,11).contains(monthInt) =>
        daysToStart(month - 1, diff + 30, specialFeb)
      case monthInt if monthInt == 2 && specialFeb =>
        daysToStart(month - 1, diff + 29, specialFeb)
      case monthInt if monthInt == 2 && !specialFeb =>
        daysToStart(month - 1, diff + 28, specialFeb)
      case _ => diff
    }
  }
  def yearDifference(startYear: Int, endYear: Int, diff: Int): Int = {
    if (startYear < endYear) {
      yearDifference(startYear, endYear - 1, diff + (if((endYear - 1) % 4 == 0) 366 else 365 ))
    }
    else diff
  }
}