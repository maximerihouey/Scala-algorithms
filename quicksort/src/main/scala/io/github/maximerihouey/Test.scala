package io.github.maximerihouey
import scala.collection.mutable.ListBuffer;
import java.io._

/**
  * Created by maxime on 29/10/16.
  */
object Test {

  def range(n: Int, startIndex: Int, stepSize: Int): Array[Int] = {
    val array = new Array[Int](n);
    for (i <- 0 to (n - 1)) {
      array(i) = startIndex + i * stepSize;
    }
    return array;
  }

  def fisherYatesShuffle(array: Array[Int]) = {
    //    -- To shuffle an array a of n elements (indices 0..n-1):
    //    for i from 0 to n−2 do
    //      j ← random integer such that i ≤ j < n
    //    exchange a[i] and a[j]
    for(i <- 0 to array.length-2) {
      val j = i + scala.util.Random.nextInt(array.length - i);
      Quicksort.switch(array, i, j);
    }
  }

  def main(args: Array[String]) {

    val exampleSize = 100
    val example = range(exampleSize, 1, 1)
    fisherYatesShuffle(example)
    Quicksort.quicksort(example)

    var isOrdered: Boolean = true
    for(i <- 0 to example.length-2) {
      if(example(i) > example(i+1)){
        isOrdered = false
      }
    }
    println("Ordered ? %s".format(if(isOrdered) "Yes" else "No"))
  }
}
