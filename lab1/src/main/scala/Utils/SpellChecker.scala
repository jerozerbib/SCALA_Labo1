package Utils

import Dictionary.dictionary

object SpellChecker {
  /**
    * Calculate the Levenshtein distance between two words.
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenshtein distance between "s1" and "s2"
    */
  // TODO - Step 2
  def stringDistance(s1: String, s2: String): Int = {
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

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number, this function just returns it.
    * @param misspelledWord the misspelled word to correct
    * @return the closest word from "misspelledWord"
    */
  // TODO - Step 2
  def getClosestWordInDictionary(misspelledWord: String): String =
  // Check if misspelled word start with '_' if pseudonym or is a number (we assume it's only positive integer numbers)
  // Can also check if misspelled.match("^-?\d+(\.\d*)?$") for negative, or floating point numbers.
    if (misspelledWord.charAt(0) == '_' || misspelledWord.forall(c => c.isDigit)) {
      misspelledWord
    } else {
      dictionary.keys.reduceLeft((a,b) => if (stringDistance(a, misspelledWord) < stringDistance(b, misspelledWord))
        a else b)
    }
}
