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

import ilog.concert.IloIntVar
import ilog.cplex.IloCplex

import scala.collection.mutable.ArrayBuffer

abstract class Model(S: Int, T: Int, C: Int, N: Int, oom: Boolean,
                     m: IloCplex, a: Map[(Int, Int, Int), IloIntVar], b: Map[Int, IloIntVar]) {

  def solve(): Result = {
    if (oom)
      return null

    val start = m.getCplexTime
    val solved = m.solve()
    val timeElapsed = m.getCplexTime - start
    if (solved)
      Result(m.getStatus, m.getCplexStatus, Some(m.getObjValue.round.toInt), timeElapsed, checkSolution())
    else
      Result(m.getStatus, m.getCplexStatus, None, timeElapsed, null)
  }

  def end(): Unit = m.end()

  private def decode(n: Int): State = {
    val bay = Matrix.ofDim[Int](S + 1, T + 1)
    val h = Array.ofDim[Int](S + 1)

    for (s <- 1 to S) {
      val stack = (n to C).filter(c => m.getValue(a(n, C + s, c)).round.toInt == 1)
      h(s) = stack.length
      for (c <- stack) {
        val t = stack.count(d => m.getValue(a(n, d, c)).round.toInt == 1) + 1
        if (bay(s, t) != 0)
          return null
        bay(s, t) = c
      }
    }

    State(S, T, C, bay, h)
  }

  private def checkSolution(): Solution = {

    val moves = new ArrayBuffer[Move]()
    val snaps = Array.ofDim[State](N + 1)

    for (n <- 1 to N) {
      val state = decode(n)
      if (state == null)
        return null
      snaps(n) = state
    }

    for (n <- 1 until N) {
      val seq = transition(n, snaps(n), snaps(n + 1))
      if (seq == null)
        return null
      moves ++= seq
    }

    Solution(moves, snaps)
  }


  private def transition(n: Int, curr: State, next: State): ArrayBuffer[Move] = {
    val g = new Graph[RichMove]()

    val outMoves = Array.fill(S + 1)(new ArrayBuffer[RichMove]())
    val inMoves = Array.fill(S + 1)(new ArrayBuffer[RichMove]())

    for (c <- n + 1 to C if curr.loc(c) != next.loc(c)) {
      val m = RichMove(curr.loc(c), next.loc(c))
      val s1 = curr.loc(c).s
      val s2 = next.loc(c).s
      outMoves(s1) += m
      inMoves(s2) += m
      g.addNode(m)
    }

    val moves = Array.ofDim[ArrayBuffer[RichMove]](S + 1)
    for (s <- 1 to S)
      moves(s) = outMoves(s).sortBy(_.t1)(Ordering[Int].reverse) ++ inMoves(s).sortBy(_.t2)

    for (s <- 1 to S if moves(s).length >= 2)
      for (i <- 0 to moves(s).length - 2)
        g.addEdge(moves(s)(i), moves(s)(i + 1))

    val seq = g.sorting()
    // in case of precedence relation cycle
    if (seq == null)
      null
    else
      seq.map(_.toMove)
  }
}

object Model {
  def encode(layout: Layout): Matrix[Int] = {
    val A = Matrix.ofDim[Int](layout.C + layout.S + 1, layout.C + 1)
    for (s <- 1 to layout.S; t <- 1 to layout.h(s)) {
      A(layout.C + s, layout.bay(s, t)) = 1
      for (tt <- 1 until t)
        A(layout.bay(s, tt), layout.bay(s, t)) = 1
    }
    A
  }
}
