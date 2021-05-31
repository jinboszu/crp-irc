/*
 * This program is part of the paper "On the integer programming
 * formulation for the relaxed restricted container relocation problem".
 *
 * Copyright (c) 2020 Bo Jin <jinbostar@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

package crp

import crp.Graph.Neighborhood

import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue}

class Graph[T]() {
  val nodes = new HashMap[T, Neighborhood[T]]()

  def addNode(a: T): Unit = nodes(a) = Neighborhood(new HashSet[T](), new HashSet[T]())

  def addEdge(a: T, b: T): Unit = {
    nodes(a).succ += b
    nodes(b).prec += a
  }

  def sorting(): ArrayBuffer[T] = {

    val seq = new ArrayBuffer[T]()

    val temp = nodes.map({ case (k, v) => k -> v.clone() })
    val heads = new Queue[T]()

    for ((k, v) <- temp if v.prec.isEmpty)
      heads.enqueue(k)

    while (temp.nonEmpty) {
      if (heads.isEmpty)
        return null

      val head = heads.dequeue()
      seq += head
      for (s <- temp(head).succ) {
        temp(s).prec -= head
        if (temp(s).prec.isEmpty)
          heads.enqueue(s)
      }
      temp -= head
    }
    seq
  }
}


object Graph {

  case class Neighborhood[T](prec: HashSet[T], succ: HashSet[T]) {
    override def clone() = Neighborhood(prec.clone(), succ.clone())
  }

}