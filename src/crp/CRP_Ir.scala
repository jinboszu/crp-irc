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

import ilog.concert.{IloIntVar, IloNumVar}
import ilog.cplex.IloCplex

import scala.collection.mutable.ArrayBuffer

/**
  * A new binary formulation of the restricted Container Relocation Problem based on a binary encoding of configurations
  * Virgile Galle, Cynthia Barnhart, Patrick Jaillet
  * European Journal of Operational Research 2018 267(2) 467-477
  */

class CRP_Ir(S: Int, T: Int, C: Int, N: Int, oom: Boolean,
             m: IloCplex, a: Map[(Int, Int, Int), IloIntVar], b: Map[Int, IloIntVar],
             x: Map[(Int, Int), IloIntVar]
            ) extends Model(S, T, C, N, oom, m, a, b) {}

object CRP_Ir extends Cplex {

  def model(ins: Instance,
            memoryLimit: Double,
            out: OutputStream,
            warn: OutputStream,
            timeLimit: Double,
            absMIPGap: Double,
            incumbent: Array[State]
           ): CRP_Ir = {

    val S: Int = ins.S
    val T: Int = ins.T
    val C: Int = ins.C
    val N: Int = C - S + 1
    val A: Matrix[Int] = Model.encode(ins)

    val numVars = (2.0 * C * C * C + 3.0 * (1.0 + S) * C * C + (1.0 + 3.0 * S) * C - (6.0 - 5.0 * S - 6.0 * S * S + 5.0 * S * S * S)) / 6.0 + (1.0 * C * C - C - 1.0 * S * S + S) / 2.0
    val numCons = (4.0 * (2.0 + S) * C * C * C - 3.0 * (4.0 + S) * C * C + (10.0 + 11.0 * S) * C - (13.0 - 16.0 * S + 5.0 * S * S + 4.0 * S * S * S) * S) / 6.0 + (1.0 * C * C - C - 1.0 * S * S + S) / 2.0 + (6.0 * C * C * C * C * C + 15.0 * (-4.0 + S) * C * C * C * C + 10.0 * (21.0 - 10.0 * S + 1.0 * S * S) * C * C * C - 15.0 * (20.0 - 13.0 * S + 2.0 * S * S) * C * C + 2.0 * (72.0 - 55.0 * S + 10.0 * S * S) * C + (-144.0 + 410.0 * S - 425.0 * S * S + 190.0 * S * S * S - 31.0 * S * S * S * S) * S) / 30.0

    if (numVars * numCons >= memoryLimit)
      return new CRP_Ir(S, T, C, N, true, null, null, null, null)

    implicit val m = new IloCplex()
    val a = (for (n <- 1 to N; c <- n to C + S; d <- n to C) yield (n, c, d) -> m.boolVar(s"a($n,$c,$d)")).toMap
    val b = (for (d <- N + 1 to C) yield d -> m.boolVar(s"b($d)")).toMap
    val x = (for (n <- 1 until N; c <- n + 1 to C) yield (n, c) -> m.boolVar(s"x($n,$c)")).toMap

    m.setOut(out)
    m.setWarning(warn)
    m.setParam(IloCplex.Param.TimeLimit, timeLimit)
    m.setParam(IloCplex.Param.MIP.Tolerances.AbsMIPGap, absMIPGap)

    m.addMinimize(
      sum(for (n <- 1 until N; c <- n + 1 to C) yield x(n, c)) + sum(for (d <- N + 1 to C) yield b(d))
    )

    // Constraint (1): initialize for n=1
    for (c <- 1 to C + S; d <- 1 to C)
      m.add(
        a(1, c, d) === A(c, d)
      )

    // Constraint (2): a container is in one stack
    for (n <- 2 to N; c <- n to C)
      m.add(
        sum(for (s <- 1 to S) yield a(n, C + s, c)) === 1
      )

    // Constraint (3): a container cannot block itself
    for (n <- 2 to N; c <- n to C)
      m.add(
        a(n, c, c) === 0
      )

    // Constraint (4): two containers cannot block each other at the same time
    for (n <- 2 to N; c <- n to C; d <- n to C if d != c)
      m.add(
        a(n, c, d) + a(n, d, c) <= 1
      )

    // Constraint (5): for two containers in the same stack, one must block the other
    for (n <- 2 to N; s <- 1 to S; c <- n to C; d <- n to C if d != c)
      m.add(
        a(n, c, d) + a(n, d, c) >= a(n, C + s, c) + a(n, C + s, d) - 1
      )

    // Constraint (6): for two containers in different stacks, they cannot block each other
    for (n <- 2 to N; s <- 1 to S; c <- n to C; d <- n to C if d != c)
      m.add(
        a(n, c, d) + a(n, d, c) <= 2 - a(n, C + s, c) - sum(for (r <- 1 to S if r != s) yield a(n, C + r, d))
      )

    // Constraint (7): height limitation
    for (n <- 2 to N; s <- 1 to S)
      m.add(
        sum(for (d <- n to C) yield a(n, C + s, d)) <= T
      )

    // Constraint (8): variables for n=N
    for (d <- N + 1 to C; c <- N until d)
      m.add(
        b(d) >= a(N, c, d)
      )

    // Constraints (9') and (10'): if c is not moved, it will keep relation with all containers below it and will not
    // change its stack
    for (n <- 1 until N; c <- n + 1 to C; d <- n + 1 to C + S if d != c) {
      m.add(
        a(n + 1, d, c) <= a(n, d, c) + x(n, c)
      )
      m.add(
        a(n + 1, d, c) >= a(n, d, c) - x(n, c)
      )
    }

    // Constraint (11'): if c is moved, it will change its stack
    for (n <- 1 until N; c <- n + 1 to C; s <- 1 to S)
      m.add(
        x(n, c) + a(n, C + s, c) + a(n + 1, C + s, c) <= 2
      )

    // Constraint (12'): two containers are moved, they will not keep their relation
    // FIXME: This constraint may lead to support relation cycle
    for (n <- 1 until N; c <- n + 1 to C; d <- n + 1 to C if d != c)
      m.add(
        x(n, c) + x(n, d) + a(n, c, d) + a(n + 1, c, d) <= 3
      )

    // Constraint (19): a container blocking the target must be moved
    for (n <- 1 until N; c <- n + 1 to C)
      m.add(
        x(n, c) >= a(n, n, c)
      )

    // Constraint (20): LIFO rules between two stacks
    // FIXME: This constraint may lead to precedence relation cycle
    for (n <- 1 until N;
         c <- n + 1 to C + S;
         e <- n + 1 to C + S if e != c;
         d <- n + 1 to C if d != c && d != e;
         f <- n + 1 to C if f != c && f != e && f != d)
      m.add(
        a(n, c, d) + a(n, e, f) + a(n + 1, e, d) + a(n + 1, c, f) <= 3 + a(n, e, d)
      )


    // use incumbent
    if (incumbent != null) {
      val vars = new ArrayBuffer[IloNumVar]()
      val values = new ArrayBuffer[Double]()

      for (n <- 2 to N) {
        val An = Model.encode(incumbent(n))
        for (c <- n to C + S; d <- n to C) {
          vars += a(n, c, d)
          values += An(c, d)
        }
      }

      val last = incumbent(N)

      for (s <- 1 to S; t <- 1 to last.h(s) if last.bay(s, t) > N) {
        vars += b(last.bay(s, t))
        values += (if (last.bay(s, t) > last.min(s, t - 1)) 1 else 0)
      }

      for (n <- 1 until N; c <- n + 1 to C) {
        vars += x(n, c)
        values += (if (incumbent(n).loc(c) != incumbent(n + 1).loc(c)) 1 else 0)
      }

      m.addMIPStart(vars.toArray, values.toArray)
    }

    assert(numVars.round.toInt == m.getNcols)
    assert(numCons.round.toInt == m.getNrows)

    new CRP_Ir(S, T, C, N, false, m, a, b, x)
  }
}
