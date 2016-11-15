package io.github.maximerihouey

/**
  * Created by maxime on 16/11/16.
  */
object Quicksort {

  def switch(array: Array[Int], index1: Int, index2: Int) = {
    val temp = array(index1)
    array(index1) = array(index2)
    array(index2) = temp
  }

  def partition(array: Array[Int], left: Int, right: Int): Int = {
    val pivotIndex: Int = left + scala.util.Random.nextInt(right - left)
    val pivotValue: Int = array(pivotIndex)
    switch(array, pivotIndex, right)
    var i = left
    for(j <- left to right-1) {
      if(array(j) <= pivotValue) {
        switch(array, j, i)
        i += 1
      }
    }
    switch(array, i, right)
    return i
  }

  def _quicksort(array: Array[Int], left: Int, right: Int) {
    if(left < right) {
      val p = partition(array, left, right)
      _quicksort(array, left, p-1)
      _quicksort(array, p+1, right)
    }
  }

  def quicksort(array: Array[Int]) {
    _quicksort(array, 0, array.length-1)
  }
}
