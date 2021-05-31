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

class State(S: Int, T: Int, C: Int, bay: Matrix[Int], h: Array[Int],
            var next: Int, val loc: Array[Position], val min: Matrix[Int]
           ) extends Layout(S, T, C, bay, h) {

  def nonEmpty: Boolean = next <= C

  def relocate(m: Move): Unit = {
    val Move(s1, s2) = m

    val c = bay(s1, h(s1))

    bay(s1, h(s1)) = 0
    min(s1, h(s1)) = 0
    h(s1) -= 1

    h(s2) += 1

    bay(s2, h(s2)) = c
    min(s2, h(s2)) = math.min(c, min(s2, h(s2) - 1))

    loc(c) = Position(s2, h(s2))
  }

  def isNextRetrievable: Boolean = {
    val Position(sn, tn) = loc(next)
    h(sn) == tn
  }

  def retrieveNext(): Unit = {
    val Position(sn, tn) = loc(next)
    loc(next) = Position(0, 0)
    bay(sn, tn) = 0
    min(sn, tn) = 0
    h(sn) -= 1
    next += 1
  }

  def topItem(s: Int): Int = bay(s, h(s))

  def stackMin(s: Int): Int = min(s, h(s))

  def nonEmpty(s: Int): Boolean = h(s) > 0

  def nonFull(s: Int): Boolean = h(s) < T

  override def clone() = new State(S, T, C, bay.clone(), h.clone(), next, loc.clone(), min.clone())
}


object State {

  def apply(S: Int, T: Int, C: Int, bay: Matrix[Int], h: Array[Int]): State = {
    val next = C + 1 - h.sum
    val loc = Array.ofDim[Position](C + 1)
    val min = Matrix.ofDim[Int](S + 1, T + 1)

    for (s <- 1 to S) {
      min(s, 0) = C + 1
      for (t <- 1 to h(s)) {
        loc(bay(s, t)) = Position(s, t)
        min(s, t) = math.min(bay(s, t), min(s, t - 1))
      }
    }
    new State(S, T, C, bay, h, next, loc, min)
  }
}
