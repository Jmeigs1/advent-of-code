import scala.io.Source
import scala.io.BufferedSource
import scala.collection.mutable.Queue
import scala.util.chaining._

var myRegex = raw"mul\((\d{1,3}),(\d{1,3})\)".r

@main def main() = {
  part1()
  part2()
}

def part1() = {
  var count = 0
  val source = Source.fromFile("input.txt")
  val data = source.mkString

  val matches = myRegex.findAllIn(data)
  while (matches.hasNext) {
    matches.next()
    count += matches.group(1).toInt * matches.group(2).toInt
  }
  println(count)
  source.close()
}

def part2() = {
  var count = 0
  var data = ""
  val source = Source.fromFile("input.txt")
  source pipe
    doDontParser pipe
    multParser pipe
    (iter => iter.foreach(d => count += d))

  println(count)
  source.close()
}

var test = false

def testString(q: Queue[Char], str: String): Boolean = {
  val comp = q.clone().mkString
  val start = comp.length() - str.length()

  if (start < 0) return false

  val end = comp.length()

  if (test && q.length == 10) {
    println(comp.substring(start, end))
    test = false
  }
  return str == comp.substring(start, end)
}

def multParser(in: Iterator[Char]): Iterator[Int] = {
  var queue = Queue[Char]()

  def getData(): Option[Int] = {
    while (in.hasNext) {
      val next = in.next()
      if (queue.length == 12)
        queue.dequeue()
      queue.enqueue(next)

      val found = myRegex.findFirstMatchIn(queue.mkString)
      if (found.isDefined) {
        queue.clear()
        return Some(found.get.group(1).toInt * found.get.group(2).toInt)
      }
    }
    return None
  }

  return Iterator
    .continually(getData())
    .takeWhile(c => c.isDefined)
    .map(c => c.get)
}

def doDontParser(in: Iterator[Char]): Iterator[Char] = {
  var enabled = true
  var queue = Queue[Char]()

  def getData(): Option[Char] = {
    while (in.hasNext) {
      val next = in.next()
      if (queue.length == 7)
        queue.dequeue()
      queue.enqueue(next)

      if (!enabled && queue.length > 3)
        enabled = testString(queue, "do()")

      if (enabled && queue.length > 6)
        enabled = !testString(queue, "don't()")

      if (enabled)
        return Option(next)
    }
    return None
  }

  return Iterator
    .continually(getData())
    .takeWhile(c => c.isDefined)
    .map(c => c.get)
}
