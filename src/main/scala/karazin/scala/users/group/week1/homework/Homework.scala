package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
import scala.math.{BigInt, toDegrees}
import java.math.BigInteger
import scala.math
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean = {
      if(b == true) false;
      else
        true;
    }

    def and(left: Boolean, right: Boolean): Boolean = {
      if (left == false) false;
      else if (right == false) false;
      else true;
    }

    def or(left: Boolean, right: Boolean): Boolean = {
      if(left == true) true;
      else if(right == true) true;
      else
        false;
    }
  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (num1, num2) => {
      def multiply(num1: BigInt, num2: BigInt, accumulator: BigInt) : BigInt = {
        if (num1 == 0 || num2 == 0)
          accumulator
        else if (num2 < 0)
          multiply(num1, num2 + 1, accumulator - num1)
        else
          multiply(num1, num2 - 1, accumulator + num1)
      }
      multiply(num1, num2, 0)
    }

    val power: (BigInt, BigInt) => BigInt = (num1, num2) => {
      def calculatePower(num1: BigInt, num2: BigInt, accumulator: BigInt) : BigInt = {
        if (num2 > 0)
          calculatePower(num1, num2 - 1, accumulator * num1)
        else if (num2 < 0)
          calculatePower(num1, -num2, 1 / accumulator)
        else
          accumulator
      }
      calculatePower(num1, num2, 1)
    }

    val fermatNumber: (Int) => BigInt = (num: Int) => power(BigInt(2), power(BigInt(2), num))+BigInt(1);

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = (n: Int) =>{
      if(n <= 0) BigInt(0);
      var str = "1";

      if (n > 1) str = "11";

      var i = 3;
      while (i <= n) {
        str += '$';
        val len = str.length;
        var cnt = 1;
        var tmp = "";
        val arr = str.toCharArray;
        for (j <- 1 until len) {
          if (arr(j) != arr(j - 1)) {
            tmp += cnt + 0;
            tmp += arr(j - 1);
            cnt = 1;
          } else {
            cnt += 1;
          }
        }
        str = tmp;
        i += 1;
      }
      BigInt(str);
    }
    
  end `Look-and-say Sequence`

end Homework