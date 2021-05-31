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

import scala.collection.mutable.ArrayBuffer

/**
  * A mathematical formulation and complexity considerations for the blocks relocation problem
  * Marco Caserta, Silvia Schwarze, Stefan Voß
  * European Journal of Operational Research 2012 219(1) 96-104
  */

object MinMax {

  def solve(ins: Instance): Solution = {
    val state = ins.toState
    val moves = new ArrayBuffer[Move]()
    val snaps = Array.ofDim[State](state.C + 1)

    while (state.nonEmpty) {
      snaps(state.next) = state.clone()
      while (!state.isNextRetrievable) {
        val m = decide(state)
        state.relocate(m)
        moves += m
      }
      state.retrieveNext()
    }
    Solution(moves, snaps)
  }

  private def decide(state: State): Move = {
    val sn = state.loc(state.next).s
    val c = state.topItem(sn)

    val (good, bad) = (1 to state.S).filter(s => s != sn && state.nonFull(s)).partition(s => state.stackMin(s) >= c)
    if (good.nonEmpty)
      Move(sn, good.minBy(state.stackMin))
    else
      Move(sn, bad.maxBy(state.stackMin))
  }
}
