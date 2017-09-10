package foo.bar.baz

import org.scalatest.{BeforeAndAfter, FunSpec}

/**
  * Created by sally on 09/09/2017.
  */
class DateDifferenceSpec extends FunSpec with BeforeAndAfter {

  describe("Input '01 01 1990'") {
    it("should return format error") {
      assert(DateDifference.processInput("01 01 1990") === "Error in format")
    }
  }

  describe("Input '01 00 1990, 32 01 1991'") {
    it("should return invalid date") {
      assert(DateDifference.processInput("01 00 1990, 32 01 1991") === "Invalid early date")
    }
  }

  describe("Input '01 01 1990, 32 01 1990'") {
    it("should return invalid date") {
      assert(DateDifference.processInput("01 01 1990, 32 01 1990") === "Invalid late date")
    }
  }

  describe("Input '01 01 1991, 02 01 1990'") {
    it("should return error in date") {
      assert(DateDifference.processInput("01 01 1991, 02 01 1990") === "first date is later than second date")
    }
  }

  describe("Input '27 02 1990, 02 03 1990'") {
    it("should return correct result 3") {
      assert(DateDifference.processInput("27 02 1990, 02 03 1990") === "27 02 1990, 02 03 1990, 3")
    }
  }

  describe("Input '27 02 1992, 02 03 1992'") {
    it("should return correct result  4") {
      assert(DateDifference.processInput("27 02 1992, 02 03 1992") === "27 02 1992, 02 03 1992, 4")
    }
  }

  describe("Input '27 02 1990, 27 02 1991'") {
    it("should return correct result 365") {
      assert(DateDifference.processInput("27 02 1990, 27 02 1991") === "27 02 1990, 27 02 1991, 365")
    }
  }

  describe("Input '27 02 1992, 27 02 1993'") {
    it("should return correct result 366") {
      assert(DateDifference.processInput("27 02 1992, 27 02 1993") === "27 02 1992, 27 02 1993, 366")
    }
  }

  describe("Input '01 02 2000, 10 01 2005'") {
    it("should return correct result 1805") {
      assert(DateDifference.processInput("01 02 2000, 10 01 2005") === "01 02 2000, 10 01 2005, 1805")
    }
  }
}
