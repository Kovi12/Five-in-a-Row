import javax.sound.sampled.AudioSystem
import scala.annotation.tailrec
import scala.io.StdIn

object Main {
  type Line = List[Player]
  type Board = List[Line]

  def profileID:Int = 1

  def newline = "\n"

  def toPos(c: Char): Player =
    c match {
      case 'X' => One
      case '0' => Two
      case _ => Empty
    }
  // creates a board from a string
  def makeBoard(s: String): Board = {

    val sequence =
      (((str: String) => str.split(newline).toList)
        andThen (_.map(_.toList))
        andThen (_.map(_.map(toPos)))
        )
    sequence(s)
  }

  // checks if the position (x,y) board b is free
  def isFree(x:Int, y:Int, b:Board): Boolean = if(b(x)(y) == Empty) true else false

  // returns the "other" player from a position, if it exists
  private def complement(p: Player): Player = p match {
    case One => Two
    case Two => One
    case Empty => Empty
  }

  def show(b: Board): String = {
    def toChar(p: Player): Char = p match {
      case One => 'X'
      case Two => 'O'
      case _ => '.'
    }

    val rows = b.indices
    val cols = b.head.indices

    val header = cols.mkString(" ")
    val rowLines = rows.map { row =>
      val rowElements = b(row).map(toChar).mkString(" ")
      s"$row $rowElements"
    }

    val boardString = (header +: rowLines).mkString("\n")

    s"  $boardString"
  }


  // Returns a list of columns from a board
  private def getColumns(b:Board): Board =
    b.transpose

  //returns the first diagonal as a line
  private def getFstDiag(b:Board): Line = {
    for (x <- b.indices.toList)
      yield b(x)(x)
  }

  //returns the second diagonal as a line
  private def getSndDiag(b:Board): Line =
    for (x <- b.indices.toList)
      yield b(x)(b.size - x - 1)

  // retrieves all the diagonals above the first line
  private def getAboveFstDiag(b: Board): List[Line] =
    for (i <- 1.until(b.size).toList)
      yield for ((x, y) <- 0.until(b.size - i).toList.zip(i.until(b.size).toList))
        yield b(x)(y)

  private def getBelowFstDiag(b: Board): List[Line] = getAboveFstDiag(getColumns(b))

  private def getAboveSndDiag(b: Board): List[Line] = getAboveFstDiag(b.map(_.reverse))

  private def getBelowSndDiag(b: Board): List[Line] = getAboveFstDiag(getColumns(b.map(_.reverse)))

  private def allLines(b: Board): List[Line] = (b ++ getColumns(b) ++ getAboveFstDiag(b) ++ getAboveSndDiag(b) ++ getBelowFstDiag(b) ++ getBelowSndDiag(b)) ++ (getFstDiag(b) :: Nil) ++ (getSndDiag(b) :: Nil)

  private def winner(p: Player)(b: Board): Boolean = {
    allLines(b).filter(_.size >= 5).exists(subline => subline.sliding(5).contains(List.fill(5)(p)))
  }


  def update(p: Player)(ln: Int, col: Int, b: Board) : Board = b.patch(ln, Seq(b(ln).patch(col,Seq(p),1)),1)

  private def next(p: Player)(b: Board): List[Board] =
    for {
      (row, i) <- b.zipWithIndex
      (elem, j) <- row.zipWithIndex
      if elem == Empty
    } yield update(p)(i, j, b)



  @tailrec
  private def gradeLine(line: Line, i: Int, p: Player, countPlayer: Int, countEmpty: Int, countOpponent: Int, weight: Int): Int = {
    if (i >= line.size) weight
    else if (line(i) == p) gradeLine(line, i + 1, p, countPlayer + 1, 1, countOpponent, weight + countPlayer * 10)
    else if (line(i) == complement(p)) gradeLine(line, i + 1, p, 1, 1, countOpponent + 1, weight - countPlayer * 1004)
    else gradeLine(line, i + 1, p, 1, countEmpty + 1, 1, weight + 0 * countEmpty)
  }


  def heuristicValue(player: Player, board: Board): Int = {
    val lines = allLines(board).filter(_.size >= 5).flatMap(_.sliding(5))

    val playerProgress = lines.map(line => gradeLine(line, 0, player, 1, 1, 1, 0)).sum

    val opponent = complement(player)
    val opponentLines = lines.filter(line => line.contains(opponent))
    val blockingLines = opponentLines.count(line => line.count(_ == Empty) > 0)

    val playerProgressWeight = 10
    val blockingLinesWeight = 7
    val heuristicValue = playerProgress * playerProgressWeight - blockingLines * blockingLinesWeight

    heuristicValue
  }


  def playSound(): Unit = {
    val soundFile = getClass.getResource("/sound/magic.wav")
    if (soundFile == null) return
    val sound = AudioSystem.getClip
    sound.open(AudioSystem.getAudioInputStream(soundFile))
    sound.start()
  }


  private def minMaxAlphaBeta(player: Player, board: Board, depth: Int, alpha: Int, beta: Int): (Int, Board) = {
    if (winner(One)(board)) {
      (1000, board) // Player One wins
    } else if (winner(Two)(board)) {
      (-1000, board) // Player Two wins
    } else if (depth == 0) {
      (heuristicValue(player, board), board)
    } else {
      val nextMoves = next(player)(board)
      if (nextMoves.isEmpty) {
        (heuristicValue(player, board), board)
      } else {
        var bestMove: Board = nextMoves.head
        var bestScore: Int = 0
        if (player == One) {
          bestScore = Int.MinValue
          var alphaValue = alpha
          for (move <- orderMoves(nextMoves)) { // Order the moves
            val (score, _) = minMaxAlphaBeta(Two, move, depth - 1, alphaValue, beta)
            if (score > bestScore) {
              bestScore = score
              bestMove = move
            }
            alphaValue = alphaValue.max(bestScore)
            if (alphaValue >= beta)
              return (bestScore, bestMove)
          }
        } else {
          bestScore = Int.MaxValue
          var betaValue = beta
          for (move <- orderMoves(nextMoves)) { // Order the moves
            val (score, _) = minMaxAlphaBeta(One, move, depth - 1, alpha, betaValue)
            if (score < bestScore) {
              bestScore = score
              bestMove = move
            }
            betaValue = betaValue.min(bestScore)
            if (alpha >= betaValue)
              return (bestScore, bestMove)
          }
        }
        (bestScore, bestMove)
      }
    }
  }

  private def orderMoves(moves: List[Board]): List[Board] = {
    // Sort the moves based on a heuristic evaluation
    moves.sortBy(move => -heuristicValue(One, move))
  }

  private def enterMove(move: String, b: Board): String = {
    try{
      val x = move.charAt(0).asDigit
      val y = move.charAt(1).asDigit
      if (x >= 0 && x < b.size && y >= 0 && y < b.size && isFree(x, y, b)) move
      else {
        println("Enter move (x and y coordinates) as xy: ")
        enterMove(StdIn.readLine(), b)
      }
    } catch {
      case _: Exception => println("Enter move (x and y coordinates) as xy: ")
        enterMove(StdIn.readLine(), b)

    }


  }


  // for testing purposes only.
  def main(args: Array[String]) = {
    val empty =
      """......
        |......
        |......
        |......
        |......
        |......""".stripMargin.replace("\r\n", "\n")
    println("Choose Player: X or 0")
    val player = toPos(StdIn.readChar())
    @tailrec
    def loop(board: Board): Player = {
      if(winner(One)(board)) One else if (winner(Two)(board)) Two else if (!board.flatten.contains(Empty)) Empty
      else {
        println("AI has moved: ")
        val newState = minMaxAlphaBeta(complement(player), board, 4, Int.MinValue, Int.MaxValue)._2
        playSound()

        println(show(newState))

        if(winner(One)(newState)) One else if(winner(Two)(newState)) Two else if (!newState.flatten.contains(Empty)) Empty else {
          //the ai plays a move
          println("Enter move (x and y coordinates) as xy: ")
          val move = enterMove(StdIn.readLine(), newState)
          val newState2 = update(One)(move.charAt(0).asDigit, move.charAt(1).asDigit, newState)
          loop(newState2)
        }
      }
    }
    println("Winner is: " + loop(makeBoard(empty)))
  }
}

