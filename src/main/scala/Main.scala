
package object helper {

  // Generate list of possible words from passed file.
  def wordList(fname : String = "dictionaryWords.txt") : List[Word] = {
    val source = io.Source.fromFile( fname )
    val words: List[String] = source.getLines.toList
    var result: List[Word] = List()
    words.foreach(w => {
      var split = w.split(",")
      result = result.::(new Word(split(0), split(1)))
    })
    source.close()
    result
  }

  def getCategoryInput(max: Int) : Int = {
    try {
      val line: Int = scala.io.StdIn.readLine("> ").toInt

      if (line < 1) {
        println("> The number must greater than zero.")
        getCategoryInput(max)
      } else if (line > max) {
        println("> The number must not greater than the number of categories.")
        getCategoryInput(max)
      } else line
    } catch {
      case e: NumberFormatException => {
        println("> Invalid number format.")
        getCategoryInput(max)
      }
    }
  }

  // Return a random word from the passed list.
  def randomWord(words : List[Word]) : Word = {
    words( scala.util.Random.nextInt(words.length) )
  }

  // Split the word into individual letters.
  def wordSplit(word : String) : List[Char] = {
    word.toList
  }

  // Join the list of characters together with a space in-between.
  def wordJoin(wordlist : List[Char]) : String = {
    wordlist.mkString(" ")
  }

  // Set of upper case letters.
  def alphaSet : Set[Char] = {
    ('A' to 'Z').toSet
  }

  // Generate a new guess list based on letter, current matches and actual word.
  def applyGuess(letter : Char, guesslist : List[Char], hanglist : List[Char]) : List[Char] = {
    guesslist.zip(hanglist).map({case(g,h) => if (letter.isLetter && letter == h) h else g})
  }
}

class Word(namec: String, hintc: String = "") {
  var name: String = namec
  var hint: String = hintc
}

class Category(namec: String, pathc: String) {
  var name: String = namec
  var path: String = pathc
  var words: List[Word] = helper.wordList(path)
}

object Main extends App {
  // Cheery intro to the Hangman game.
  println("Welcome to the Hangman word guessing game.")

  println("Select Category:")
  val categories: List[Category] = List(
    new Category("Animal", "src/resources/animal.txt"),
    new Category("Job", "src/resources/job.txt"),
  )

  for (c <- categories) println(c.name)
  val cateInput: Int = helper.getCategoryInput(categories.length)
  val words: List[Word] = categories(cateInput - 1).words
  val word: Word = helper.randomWord(words)
  println("Hint: %s".format(word.hint))

  val hanglist: List[Char] = helper.wordSplit(word.name.toUpperCase)
  var guesslist: List[Char] = hanglist.map({case(c) => if (c.isLetter) '_' else c })
  var wrongguess: List[Char] = List()
  var fnewgame = false
  var remain = 10
  var score = 5
  while (!fnewgame && remain > 0) {
    if (hanglist == guesslist) {
      fnewgame = true
      println("Congratulations on your win with score %d, the answer is %s.".format(score, word.name))
    } else {
      print("%s\tscore %d, remaining wrong guess %d".format(
        guesslist.mkString(" "),
        score,
        remain,
      ))
      if (wrongguess.length > 0) {
        print(", wrong guessed %s".format(wrongguess.mkString(" ")))
      }
      println()

      val line = scala.io.StdIn.readLine("> ").toUpperCase
      val letter : List[Char] = line.toList
      if (letter.length != 1 || !letter.head.isLetter) {
         println("Not a valid guess -> " + line)
      } else {
        // check letter have in hang word.
        val index = hanglist.indexOf(letter.head)
        if (index != -1) { // valid
          if (guesslist(index) == '_') {
            // apply
            guesslist = helper.applyGuess(letter.head, guesslist, hanglist)
            score += 10
          } else {
            score -= 2
            println("> Already guessed.")
          }
        } else { 
          // wrong guess
          remain -= 1
          score -= 2
          if (!wrongguess.contains(letter.head)) {
            wrongguess = wrongguess.::(letter.head)
          }
        }
      }
    }
  }

  println("Thank you for playing Hangman!")
}