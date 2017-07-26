package fpinscala.localeffects

import scala.collection.mutable


object Mutable {

  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray

    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }

    def partition(l: Int, r: Int, pivotIndex: Int) = {
      val pivot = arr(pivotIndex)
      swap(pivotIndex, r)

      var j = l
      for (i <- l until r) if (arr(i) < pivot) {
        swap(i, j)
        j += 1
      }

      swap(j, r)
      j
    }

    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pivotIndex = partition(l, r, l + (r - l) / 2)
      qs(l, pivotIndex - 1)
      qs(pivotIndex + 1, r)
    }

    qs(0, arr.length - 1)
    arr.toList
  }

}

sealed trait ST[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {

    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {

    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }

}

object ST {

  def apply[S, A](a: => A): ST[S, A] = {

    lazy val memo = a

    new ST[S, A] {
      def run(s: S): (=> A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st[Null].run(null)._1
}

sealed trait STRef[S, A] {

  protected var cell: A

  def read: ST[S, A] = ST(cell)

  def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }

}

object STRef {

  def apply[S, A](a: A): ST[S, STRef[S, A]] =
    ST(new STRef[S, A] { var cell: A = a })

}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

// Scala requires an implicit Manifest for constructing arrays.
sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {

  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.length)

  // Write a value at the give index of the array
  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {

    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }

  }

  // Read the value at the given index of the array
  def read(i: Int): ST[S, A] = ST(value(i))

  // Turn the array into an immutable list
  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    ST(
      xs.foldLeft(value) { case (acc, (i, v)) =>
        acc(i) = v
        acc
      }
    )

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {

  // Construct an array of the given size filled with the value v
  def apply[S, A:Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = Array.fill(sz)(v)
    })

  def fromList[S, A:Manifest](xs: List[A]): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] {
      lazy val value: Array[A] = xs.toArray
    })

}

object Immutable {

  def noop[S]: ST[S, Unit] = ST(())

  // This is ridiculous...
  def partition[S](a: STArray[S, Int], left: Int, right: Int, pivotIndex: Int): ST[S, Int] = for {
    pivot <- a.read(pivotIndex)
    _ <- a.swap(pivotIndex, right)
    l <- STRef(left)
    _ <- (left until right).foldLeft(noop[S])((s, i) => for {
      _ <- s
      value <- a.read(i)
      _  <- if (value < pivot) for {
        j <- l.read
        _ <- a.swap(i, j)
        _ <- l.write(j + 1)
      } yield () else noop[S]
    } yield ())
    pi <- l.read
    _ <- a.swap(pi, right)
  } yield pi

  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l >= r) noop[S] else for {
    pivotIndex <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pivotIndex - 1)
    _ <- qs(a, pivotIndex + 1, r)
  } yield ()

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S]: ST[Unit, List[Int]] = for {
        arr    <- STArray.fromList(xs)
        size   <- arr.size
        _      <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
  })

}

sealed abstract class STHashMap[S, K, V] {

  protected def value: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(value.size)

  // Read the value at the given index of the array
  def read(k: K): ST[S, Option[V]] = ST(value.get(k))

  // Write a value at the give index of the array
  def write(k: K, v: V): ST[S, Unit] = new ST[S, Unit] {

    def run(s: S): (Unit, S) = {
      value += k -> v
      ((), s)
    }

  }

}

object STHashMap {

  // Construct an array of the given size filled with the value v
  def apply[S, K, V](): ST[S, STHashMap[S, K, V]] =
    ST(new STHashMap[S, K, V] {
      lazy val value: mutable.HashMap[K, V] = mutable.HashMap.empty
    })

  def fromList[S, K, V](xs: List[(K, V)]): ST[S, STHashMap[S, K, V]] =
    ST(new STHashMap[S, K, V] {
      lazy val value: mutable.HashMap[K, V] = mutable.HashMap(xs: _*)
    })

}
