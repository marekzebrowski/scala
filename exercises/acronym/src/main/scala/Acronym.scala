object Acronym {
  //not super efficient - while loop + string builder would be more efficient, but longer
  def abbreviate(phrase: String): String = {
    phrase.split("\\W").filter(_.nonEmpty).map(_.head.toUpper).mkString
  }
}
