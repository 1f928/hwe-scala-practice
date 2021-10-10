package com.farrellw.hwe.exercises

// NOTE - For simplicity sake, solve assuming old roman numeral style. 4 is IIII and NOT IV
object RomanNumeral {
  /*
    Map containing the numerical value to its roman numeral equivalent.
   */
  val INT_TO_ROMAN: Map[Int, String] = Map[Int, String](
    1000 -> "M",
    500 ->  "D",
    100 ->  "C",
    50 ->  "L",
    10 ->  "X",
    5 ->  "V",
    1 ->  "I"
  )

  /*
    Map containing a roman numeral to its numerical value equivalent.
   */
  val ROMAN_TO_INT: Map[String, Int] = Map[String, Int](
    "M" -> 1000,
    "D" -> 500,
    "C" -> 100,
    "L" -> 50,
    "X" -> 10,
    "V" -> 5,
    "I" -> 1
  )

  val ROMAN_VALS: List[Int] = List(1000, 500, 100, 50, 10, 5, 1)

  /*
    Given a roman numeral ( e.g. CCX ), returns the numerical equivalent ( e.g. 210 ).
    If the roman numeral contains any invalid roman numerals, returns None.
    // NOTE - For simplicity sake, solve assuming old roman numeral style. 4 is IIII and NOT IV
   */
  def convertRomanToInt(s: String): Option[Int] = {
    var sum: Int = 0
    for (char <- s.split("").toList) {
      if (ROMAN_TO_INT contains char) sum += ROMAN_TO_INT.get(char).get
      else return None
    }

    return Some(sum)
  }

  /*
    Given a complete roman numeral ( e.g. CCX ), returns the numerical equivalent ( e.g. 210 ).
    If the roman numeral contains any invalid roman numerals, return an exception
    // NOTE - For simplicity sake, solve assuming old roman numeral style. 4 is IIII and NOT IV
   */
  def convertRomanToIntEither(s: String): Either[Exception, Int] = {
    var sum: Int = 0
    for (char <- s.split("").toList) {
      if (ROMAN_TO_INT contains char) sum += ROMAN_TO_INT.get(char).get
      else return Left(new RuntimeException("bad char"))
    }

    return Right(sum)
  }

  /*
    Given a number, return the roman numeral equivalent
   */
  def convertIntToRoman(i: Int): Option[String] = {
    val remove: Int = INT_TO_ROMAN.keys.fold(0)((big, n) => if (n <= i && n > big) n else big)
    val char: String = INT_TO_ROMAN.get(remove).get

    val remainder: Int = i - remove
    if (remainder <= 0) Some(char) else Some(char + convertIntToRoman(remainder).get)
  }
}
