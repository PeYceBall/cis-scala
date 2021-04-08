package homeworks.homework1

import scala.annotation.tailrec

object task1 extends App {

  /**
   * Напишите хвосторекурсивную функцию, возвращающую n-e число из последовательности коров Нараяны,
   * задаваемой отношением
   *
   * a_0 = a_1 = a_2 = 1
   * a_n = a_{n-1} + a_{n-3}
   *
   * https://oeis.org/A000930
   *
   * @param n номер числа последовательности
   * @return n-ое число последовательности коров Нараяны (согласно формуле выше)
   */

  @tailrec
  def narayanaCowsRec(n: Int, prevprev: Int, prev: Int, cur: Int): Int = {
    if (n < 3) cur
    else narayanaCowsRec(n - 1, prev, cur, cur + prevprev)
  }

  def narayanaCows(n: Int): Int = narayanaCowsRec(n, 1, 1, 1)

  (for (i <- 0 until 10) yield s"$i) ${narayanaCows(i)}").foreach(println)
  //  0) 1
  //  1) 1
  //  2) 1
  //  3) 2
  //  4) 3
  //  5) 4
  //  6) 6
  //  7) 9
  //  8) 13
  //  9) 19
}
