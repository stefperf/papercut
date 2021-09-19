// Late solution to Riddler Classic @ https://fivethirtyeight.com/features/can-you-get-the-paper-cut/
//
// This is my 1st Scala program and is badly overengineered just to play around with the language's features.
//
import scala.collection.mutable

// print solution to the riddle
object Application extends App {
  val m = 8.5
  val n = 11
  println(f"For a paper sheet of size $m x $n, the expected nr. of cuts is ${PapercutPuzzle().expectedCuts(m, n)}%.3f.")
}

// add the ability to memoize unary functions, with a fully hidden overflow-safe implementation based on a WeakHashMap
trait CanMemoize {
  // generic memoized subclass of Function1
  private[this] class MemFunction1[Input, Output](func: Input => Output) extends Function1[Input, Output] {
    private[this] val cache = mutable.WeakHashMap.empty[Input, Output]
    private[this] val cachedFunc = input => cache.getOrElseUpdate(input, func(input))
    final override def apply(input: Input): Output = cachedFunc(input)
  }
  // replace the given unary function with a memoized one acting on the same types
  def memoize[Input, Output](func: Input => Output): Function1[Input, Output] = new MemFunction1[Input, Output](func)
}

// inefficient recursive solution to the papercut puzzle
abstract class PapercutPuzzle {
  def isStrip(m: Double, n: Double): Boolean = (m min n) <= 1
  def expectedCuts(m: Double, n: Double): Double = {
    if (isStrip(m, n)) 0  // 0 cuts are needed if the paper sheet is already a strip
    else 1 + (expectedCuts(m-1, n) + expectedCuts(m, n-1)) / 2  // 1 cut + the expected cuts for the resulting sheet
  }
}

// defining factory method returning an appropriate concrete singleton object
object PapercutPuzzle {
  def apply(): PapercutPuzzle = MemoizedPapercutPuzzle
}

// singleton object providing a memoized solution to the puzzle
object MemoizedPapercutPuzzle extends PapercutPuzzle with CanMemoize {
  // memoized function
  protected[this] val expectedCutsMemoized = memoize[Tuple2[Int, Int], Double](input => {
    val (m, n) = input
    super.expectedCuts(m, n)
  })
  // call the memoized function after "compacting" the input in a parsimonious way
  override def expectedCuts(m: Double, n: Double): Double = {
    val mc: Int = m.ceil.toInt
    val nc: Int = n.ceil.toInt
    val normalizedInput = if (mc <= nc) (mc, nc) else (nc, mc)
    expectedCutsMemoized(normalizedInput)
  }
}
