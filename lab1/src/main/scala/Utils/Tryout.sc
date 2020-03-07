"hello".substring(0, 0).length
val dictionary: Map[String, String] = Map(
  "bonjour" -> "bonjour",
  "hello" -> "bonjour",
  "yo" -> "bonjour",
  "je" -> "je",
  "j" -> "je",
  "suis" -> "etre",
  "veux" -> "vouloir",
  "aimerais" -> "vouloir",
  "bière" -> "biere",
  "bières" -> "biere",
  "croissant" -> "croissant",
  "croissants" -> "croissant",
  "et" -> "et",
  "ou" -> "ou",
  "svp" -> "svp",
  "stp" -> "svp"
)

def stringDistance(s1: String, s2: String): Int = {
  // Iterative algorithm
  /*val matrix = Array.ofDim[Int](s1.length + 1,s2.length + 1)
  var i, j, substitutionCost = 0

  for (i <- 0.to(s1.length)) {
    matrix(i)(0) = i
  }

  for (j <- 0.to(s2.length)) {
    matrix(0)(j) = j
  }

  for (i <- 1.to(s1.length)) {
    for (j <- 1.to(s2.length)) {
      if (s1.charAt(i - 1) == s2.charAt(j-1)) {
        substitutionCost = 0
      } else {
        substitutionCost = 1
      }

      matrix(i)(j) = Math.min(matrix(i-1)(j) + 1, Math.min(matrix(i)(j-1) + 1, matrix(i-1)(j-1) + substitutionCost))
    }
  }

  matrix(s1.length)(s2.length)*/

  // Recursive version
  /*def levLoop(s: String, t: String, i: Int, j: Int): Int = {
    if (i == 0) j
    else if (j == 0) i
    else {
      val removal = levLoop(s, t, i-1, j) + 1
      val insertion = levLoop(s,t, i, j-1) + 1
      val substitution = if (s.charAt(i) == t.charAt(j)) levLoop(s, t, i-1, j- 1)
      else levLoop(s, t, i-1, j-1) + 1
      Math.min(removal, Math.min(insertion, substitution))
    }
  }

  levLoop(s1, s2, s1.length - 1, s2.length - 1)*/

  // Recursive version
  def levLoop(s: String, t: String) : Int = {
    if (s.length == 0) {
      t.length
    } else if (t.length == 0) {
      s.length
    } else {
      val removal = levLoop(s.substring(0, s.length - 1), t)
      val insertion = levLoop(s, t.substring(0, t.length - 1))
      val substitution = levLoop(s.substring(0, s.length - 1), t.substring(0, t.length - 1))
      if (s.charAt(s.length - 1) == t.charAt(t.length - 1)) {
        substitution
      } else {
        Math.min(removal, Math.min(insertion, substitution)) + 1
      }
    }
  }

  levLoop(s1, s2)
}

dictionary.keys.reduceLeft((a, b) => if (stringDistance(a, "veu") < stringDistance(b, "veu")) a else b)