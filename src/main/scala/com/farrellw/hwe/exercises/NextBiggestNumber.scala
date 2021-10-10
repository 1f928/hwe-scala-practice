package com.farrellw.hwe.exercises

object NextBiggestNumber {

  def intToDigits(i: Int): Vector[Int] = i.toString.map(_.asDigit).toVector
  def digitsToInt(l: Vector[Int]): Int = l.mkString("").toInt

  /*
  For a given number, return the next largest number that can be created by rearranging that number's digits.
  If no larger number can be created, return -1
   */

  def getNextBiggestNumber(i: Integer): Int = {
    val digits: Vector[Int] = intToDigits(i)
    val len = digits.length

    // Reading right-to-left:
    for (i <- Range(len - 2, -1, -1)) {

      // Find first digit lower than prior digit
      if (digits(i) < digits(i + 1)) {

        // Swap low number with seen higher number closest in value
        // And sort digits below the swap-line before reassembling
        val leftDigits = digits.slice(0, i)
        val otherDigits = digits.slice(i, len).sorted
        val low = digits(i)
        val high = otherDigits.filter(_ > low).head
        val highIndex = otherDigits.indexOf(high)
        val rightDigits =
          otherDigits.slice(0, highIndex) ++:
          otherDigits.slice(highIndex + 1, otherDigits.length)

        return digitsToInt(leftDigits ++: Vector(high) ++: rightDigits)
      }
    }

    return -1
  }
}
