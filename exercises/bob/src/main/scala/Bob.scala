object Bob {
  def response(statement: String): String = {
    statement.trim match {
      case "" => "Fine. Be that way!"
      case s if s.forall(c => if(c.isLetter) c.isUpper else true) && s.exists(_.isLetter) => "Whoa, chill out!"
      case s if s.endsWith("?") => "Sure."
      case _ => "Whatever."
   }
  }
}