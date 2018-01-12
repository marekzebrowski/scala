object AtbashCipher {
  val subs = "zyxwvutsrqponmlkjihgfedcba"
  private def substitute(c: Char):String =
    if (c.isDigit) c.toString
    else if (c.isLetter) subs(c.toLower - 'a').toString
    else ""

  def encode(s: String): String = {
    s.map(substitute).mkString.grouped(5).mkString(" ")
  }

  def decode(s: String): String = s.map(substitute).mkString("")

}
