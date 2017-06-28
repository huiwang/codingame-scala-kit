package com.truelaurel.collection

object ArrayUtil {
  def update[T](arr: Array[T], i: Int, v: T): Array[T] = {
    val res = arr.clone()
    res(i) = v
    res
  }

  def update[T](arr: Array[T], i: Int, f: T => T): Array[T] = {
    val res = arr.clone()
    res(i) = f(arr(i))
    res
  }

  def fastNotContains3(elts: Array[Int], e: Int): Boolean = {
    if (elts(0) == e) false
    else if (elts(1) == e) false
    else elts(2) != e
  }

  def fastContains2(elts: Array[Int], e: Int): Boolean = {
    if (elts(0) == e) true
    else elts(1) == e
  }
}
