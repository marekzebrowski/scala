object Anagram {
  def anagrams(word: String, anagrams: Seq[String]) = {
    val lcWord = word.toLowerCase
    val chars = lcWord.sorted
    anagrams.filter { candidate =>
      val lcCandidate = candidate.toLowerCase
      val sortedCandidate = lcCandidate.sorted
      sortedCandidate == chars && lcCandidate != word.toLowerCase
    }
  }
}
