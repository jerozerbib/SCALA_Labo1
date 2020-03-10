package Chat

import Tokens._
import Utils.Dictionary.dictionary
import Utils.SpellChecker._

class Tokenizer(input: String) {
  val punctuationRegex = "[.?!*,'\\s]+"
  // We need to store the tokens list for the class as a variable since collections are immutable
  // and we need to update it after we tokenize(). We also need to store the counter for the current token.
  var tokenList : Array[(String, Token)] = Array()
  var counter = 0
  /**
    * Separate the user's input into tokens.
    */
  // TODO - Step 3
  def tokenize(): Unit = {
    // Remove punctuation and multiple spaces and replace with simple whitespace
    val splitIt = input.replaceAll(punctuationRegex, " ").split(" ")
    // Split the string by whitespace
    val tokens = splitIt.map(s => {
      val word = dictionary.getOrElse(s, getClosestWordInDictionary(s))
      // match on the normalized form of the word
      val normalized = dictionary.get(word)
      normalized match {
        case Some(s) => getToken(s)
        case None =>    // In case it's a number, a pseudo or something else and isn't found in the dictionary
          word match {
            case s if s.startsWith("_") => s -> PSEUDO
            case s if s.charAt(0).isDigit => s -> NUM
            case s  => s -> UNKNOWN
          }
      }
    })
    // add the last EOL token
    tokenList = tokens :+ ("EOL" -> EOL)
  }

  def getToken(word: String): (String, Token) =
    word match {
      case "bonjour" => "bonjour" -> BONJOUR
      case "je" => "je" -> JE
      case "etre" => "etre" -> ETRE
      case "vouloir" => "vouloir" -> VOULOIR
      case "biere" => "biere" -> BIERE
      case "croissant" => "croissant" -> CROISSANT
      case "et" => "et" -> ET
      case "ou" => "ou" -> OU
      case "svp" => "svp" -> UNKNOWN
      case s  => s -> UNKNOWN
    }

  /**
    * Get the next token of the user input, or OEL if there is no more token.
  	* @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  // TODO - Step 3
  def nextToken(): (String, Token) = {
    val nextToken = tokenList(counter)
    counter += 1
    nextToken
  }
}
