/* Andrew Vadnais
 * CSC344
 * Assignment 3
 * 15 March 2018
 * */


import scala.io.StdIn.readLine

object Main {
  var input = ""
  var str = ""
  var i = 0

  //gets current char
  def current(): Char = {
    if (i + 1 == str.length())
      '$'
    else {
      str.charAt(i)
    }
  }

  def main(args: Array[String]): Unit = {

    //get pattern from user and parse
    print("pattern? ")
    val inputPattern = readLine() + "$"
    val patternParsed = new parse(0, inputPattern)
    val pattern = patternParsed.parseS()

    //get input string from user
    print("string? ")
    Main.str = readLine() + "$"

    //loop until quit
    while (Main.str != "quit$") {

      //evaluate string against pattern
      val eval = pattern.eval()
      if (eval && (!(i < str.length() - 1)))
        println("match")
      else
        println("no match")

      print("string? ")
      Main.i = 0
      Main.str = readLine() + "$"
    }
  }
}

class parse(i: Int, patt: String) {
  var pos: Int = i
  val pat: String = patt
  val len: Int = pat.length()

  def check(): Char = {
    if (pos + 1 == len)
      '$'
    else
      pat.charAt(pos)

  }

  //define parse rules below
  def parseS(): S = {
    S(parseE(): E, '$')
  }

  def parseE(): E = {
    E(parseT(), parseE2())
  }

  def parseE2(): E2 = {
    if (check() == '|') {
      pos += 1
      E2('|', parseE3())
    }
    else
      null
  }

  def parseE3(): E3 = {
    E3(parseT(), parseE2())
  }

  def parseT(): T = {
    if (check() == ')')
      null
    else
      T(parseF(), parseT2())
  }

  def parseT2(): T2 = {
    if (check() != '|' && check() != ')' && check() != '$')
      T2(parseF(), parseT2())
    else
      null
  }

  def parseF(): F = {
    F(parseA(), parseF2())
  }

  def parseF2(): F2 = {
    if (check() == '?')
      F2(parseOptional(), parseF2())
    else
      null
  }

  def parseOptional(): Char = {
    pos += 1
    '?'
  }

  def parseA(): A = {
    A(parseC(), parseA2())
  }

  def parseA2(): A2 = {
    if (check() == '(')
      A2(parseL(), parseA3())
    else
      null
  }

  def parseL(): Char = {
    pos += 1
    '('
  }

  def parseA3(): A3 = {
    A3(parseE(), parseR())
  }

  def parseR(): Char = {
    pos += 1
    ')'
  }

  def parseC(): C = {
    if (mayStep()) {
      pos += 1
      C(pat.charAt(pos - 1))
    }
    else
      null
  }

  def mayStep(): Boolean = {
    check().isLetter || check().isDigit || check() == '.' || check() == ' '
  }

  //define grammar rules below
  case class S(E: E, EOF: Char) {
    def eval(): Boolean = {
      E.eval()
    }
  }

  case class E(T: T, E2: E2) {
    def eval(): Boolean = {
      if (!T.eval()) {
        if (E2 == null)
          false
        else
          E2.eval()
      }
      else
        true
    }
  }

  case class E2(OR: Char, E3: E3) {
    def eval(): Boolean = {
      E3.eval()
    }
  }

  case class E3(T: T, E2: E2) {
    def eval(): Boolean = {
      if (E2 == null)
        T.eval()
      else
        T.eval() || E2.eval()
    }
  }

  case class T(F: F, T2: T2) {
    def eval(): Boolean = {
      if (T2 == null)
        F.eval()
      else
        F.eval() && T2.eval()
    }
  }

  case class T2(F: F, T2: T2) {
    def eval(): Boolean = {
      if (this.T2 == null)
        this.F.eval()
      else
        F.eval() && T2.eval()
    }
  }

  case class F(A: A, F2: F2) {
    def eval(): Boolean = {
      if (A.eval())
        true
      else {
        if (F2 == null)
          false
        else
          F2.eval()
      }
    }
  }

  case class F2(q: Char, F2: F2) {
    def eval(): Boolean = {
      if (q == '?')
        true
      else
        false
    }
  }

  case class A(C: C, A2: A2) {
    def eval(): Boolean = {
      if (C == null)
        A2.eval()
      else
        C.eval()
    }
  }

  case class A2(open: Char, A3: A3) {
    def eval(): Boolean = {
      A3.eval()
    }
  }

  case class A3(E: E, close: Char) {
    def eval(): Boolean = {
      E.eval()
    }
  }

  case class C(TR: Char) {
    def eval(): Boolean = {
      if ((Main.str.charAt(Main.i) == this.TR || this.TR == '.') &&
        Main.str.charAt(Main.i) != '$') {
        Main.i += 1
        true
      }
      else
        false
    }
  }

}
