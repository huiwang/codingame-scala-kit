package com.truelaurel.collection

object ArrayUtil {
  def update[T](arr: Array[T], i: Int, v: T) = {
    val res = arr.clone()
    res(i) = v
    res
  }

  def update[T](arr: Array[T], i: Int, f: T => T) = {
    val res = arr.clone()
    res(i) = f(arr(i))
    res
  }
}
