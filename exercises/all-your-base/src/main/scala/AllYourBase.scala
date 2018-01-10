import scala.annotation.tailrec

//https://www.codeproject.com/Articles/16035/Base-Conversion-of-Very-Long-Positive-Integers
object AllYourBase {
  def rebase(inputBase: Int, inputDigits: List[Int], outputBase: Int): Option[List[Int]] = {
    if (inputBase < 2 || outputBase < 2)
      None
    else if (inputDigits.isEmpty)
      None
    else if(inputDigits.head <= 0)
      None
    else if(inputDigits.exists(i => i < 0 || i >= inputBase))
      None
    else {
      val r = doRebase(inputBase, inputDigits.dropWhile(_ == 0), outputBase)
      if(r.isEmpty) None else Some(r)

    }
  }

  private def doRebase(inputBase: Int, inputDigits: List[Int], outputBase: Int):List[Int] = {
    val iArr = inputDigits.reverse.toArray
    val il = iArr.length
    val outLen = il * (inputBase / outputBase + 1)
    val ts = new Array[Int](outLen + 10) //  int[] ts = new int[ol+10]; //assign accumulation array
    val cums = new Array[Int](outLen + 10) // int[] cums = new int[ol+10]; //assign the result array
    ts(0) = 1
    var i = 0
    while (i < il) {
      var j = 0
      while (j < outLen) {
        cums(j) = cums(j) + ts(j) * iArr(i)
        var temp = cums(j)
        var rem = 0
        var ip = j
        do {
          rem = temp / outputBase
          cums(ip) = temp - rem * outputBase
          ip = ip + 1
          cums(ip) = cums(ip) + rem
          temp = cums(ip)
        } while (temp >= outputBase)
        j = j + 1
      }
      j = 0
      while (j < outLen) {
        ts(j) = ts(j) * inputBase
        j = j + 1
      }
      j = 0
      while (j <= outLen) {
        var temp = ts(j)
        var rem = 0
        var ip = j
        do {
          rem = temp / outputBase
          ts(ip) = temp - rem * outputBase
          ip = ip + 1
          ts(ip) = ts(ip) + rem
          temp = ts(ip)
        } while (temp >= outputBase)
        j = j + 1
      }
      i = i + 1
    }
    var r = List.empty[Int]
    var j = 0
    while (j < outLen + 10) {
      r = cums(j) :: r
      j = j + 1
    }
    r.dropWhile(_ == 0)
  }

}
