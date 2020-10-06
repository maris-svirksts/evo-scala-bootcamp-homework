package basic

import scala.collection.immutable.ListMap

object HomeWork4 {

  // Homework
  //
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.

  def mergeKeys[T](value: Map[T, Int]): Set[T] = {
    val result = for {
      (key, value) <- value
    } yield key

    result.toSet
  }

  def sortConsideringEqualValues[T](
      map: Map[T, Int]
  ): List[Map[Set[T], Int]] = {
    val mapSortedByValue = map.groupBy(_._2)

    val result = for {
      (key, value) <- mapSortedByValue
    } yield (mergeKeys(value) -> value.toSeq(0)._2)

    println(result)

    val results = ???
    results
  }

}
