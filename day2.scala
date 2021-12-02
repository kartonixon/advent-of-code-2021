import scala.io.Source
import scala.util.Using

object Day2 extends App {
  type Command = (String, Int)

  def parseLine(line: String): Command = {
    val words = line.split(" ")
    (words.head, words.last.toInt)
  }

  def readFile(fileName: String): List[Command] = {
    Using.resource(Source.fromResource(fileName)) { source =>
      return source.getLines().map(parseLine).toList
    }
  }

  def calculatePart1(commands: List[Command]): Int ={
    val horizontal = commands.map(c => if (c._1 == "forward") c._2 else 0).sum
    val depth = commands.map(c => if (c._1 == "down") c._2 else if (c._1 == "up") -c._2 else 0).sum
    horizontal * depth
  }

  def calculatePart2(commands: List[Command]): Int ={
    var (aim, horizontal, depth) = (0, 0, 0)
    commands.foreach(c => {
      if (c._1 == "forward") {
        horizontal += c._2
        depth += aim * c._2
      } else if (c._1 == "up") {
        aim -= c._2
      } else if (c._1 == "down") {
        aim += c._2
      }
    })
    horizontal * depth
  }

  val input = readFile(fileName = "day2-input.txt")
  println(calculatePart1(input))
  println(calculatePart2(input))
}