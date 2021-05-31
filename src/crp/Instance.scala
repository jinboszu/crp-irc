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

import java.io.OutputStream

import scala.io.Source

class Instance(S: Int, T: Int, C: Int, bay: Matrix[Int], h: Array[Int]) extends Layout(S, T, C, bay, h) {

  def toState = State(S, T, C, bay.clone(), h.clone())

  def toCRP_I(memoryLimit: Double = 8e10,
              out: OutputStream = null,
              warn: OutputStream = System.err,
              timeLimit: Double = 3600,
              absMIPGap: Double = 0.99,
              usePreProcess: Boolean = true,
              incumbent: Array[State] = MinMax.solve(this).snaps
             ): CRP_I = CRP_I.model(this, memoryLimit, out, warn, timeLimit, absMIPGap, usePreProcess, incumbent)

  def toCRP_Ir(memoryLimit: Double = 8e10,
               out: OutputStream = null,
               warn: OutputStream = System.err,
               timeLimit: Double = 3600,
               absMIPGap: Double = 0.99,
               incumbent: Array[State] = MinMax.solve(this).snaps
              ): CRP_Ir = CRP_Ir.model(this, memoryLimit, out, warn, timeLimit, absMIPGap, incumbent)

  def toCRP_Irc(memoryLimit: Double = 8e10,
                out: OutputStream = null,
                warn: OutputStream = System.err,
                timeLimit: Double = 3600,
                absMIPGap: Double = 0.99,
                incumbent: Array[State] = MinMax.solve(this).snaps
               ): CRP_Irc = CRP_Irc.model(this, memoryLimit, out, warn, timeLimit, absMIPGap, incumbent)
}

object Instance {

  def readCaserta(file: String): Instance = {
    val source = Source.fromFile(file)
    val lines = source.getLines()
    val head = lines.next().split(" ").map(_.toInt)
    val S = head(0)
    val C = head(1)
    val T = C / S + 2
    val bay = Matrix.ofDim[Int](S + 1, T + 1)
    val h = Array.ofDim[Int](S + 1)
    for (s <- 1 to S) {
      val line = lines.next().split(" ").map(_.toInt)
      h(s) = line(0)
      for (t <- 1 to h(s))
        bay(s, t) = line(t)
    }
    source.close()
    new Instance(S, T, C, bay, h)
  }
}
