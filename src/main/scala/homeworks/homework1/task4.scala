package homeworks.homework1

object task4 extends App {
  /**
   * Реализуйте метод optionPairMax, возвращающий максимум двух Option.
   * Если оба Option определены, нужно вернуть тот, значеие которого больше.
   * Если определен только один Option, нужно вернуть его.
   * Если ни один не определен, нужно вернуть None.
   */
  def optionPairMax(option1: Option[Int], option2: Option[Int]): Option[Int] =
    (option1, option2) match {
      case (Some(value1), Some(value2)) => Some(math.max(value1, value2))
      case (Some(value1), None) => Some(value1)
      case (_, opt2) => opt2
    }

  println(optionPairMax(Some(3), Some(5)))
  // Some(5)
  println(optionPairMax(Some(1), Some(2)))
  // Some(2)
  println(optionPairMax(Some(6), None))
  // Some(6)
  println(optionPairMax(None, Some(8)))
  // Some(8)
  println(optionPairMax(None, None))
  // None
}
