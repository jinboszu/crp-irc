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

package run

import java.io.{File, FileWriter, PrintWriter}

import crp.{Instance, MinMax}

object CRP_I {
  def main(args: Array[String]): Unit = {

    val file = if (args.nonEmpty)
      s"results/crp/crp-i-caserta-(${args(0)},${args(1)},${args(2)},${args(3)}).csv"
    else
      "results/crp/crp-i-caserta.csv"
    println(s"save results to $file")

    new File(file).getParentFile.mkdirs()
    val writer = new PrintWriter(new FileWriter(file), true)

    writer.println("H,S,I,T,C,MinMax,MemoryOut,Status,CplexStatus,Objective,TimeElapsed")

    val HS = if (args.nonEmpty)
      List((args(0).toInt, args(1).toInt, args(2).toInt, args(3).toInt))
    else
      List(
        (3, 3, 1, 40), (3, 4, 1, 40), (3, 5, 1, 40), (3, 6, 1, 40), (3, 7, 1, 40), (3, 8, 1, 40),
        (4, 4, 1, 40), (4, 5, 1, 40), (4, 6, 1, 40), (4, 7, 1, 40),
        (5, 4, 1, 40), (5, 5, 1, 40), (5, 6, 1, 40), (5, 7, 1, 40), (5, 8, 1, 40), (5, 9, 1, 40), (5, 10, 1, 40),
        (6, 6, 1, 40), (6, 10, 1, 40),
        (10, 6, 1, 40), (10, 10, 1, 40)
      )


    for ((h, s, start, end) <- HS) {
      for (i <- start to end) {
        val ins = Instance.readCaserta(s"data/CRPTestcases_Caserta/data$h-$s-$i.dat")
        val t = ins.T
        val c = ins.C
        val sol = MinMax.solve(ins)
        val minmax = sol.length

        val crp_i = ins.toCRP_I(incumbent = sol.snaps)
        val res = crp_i.solve()

        if (res == null) {
          println(f"$h-$s-$i: minmax=$minmax, memoryOut=true")
          writer.println(f"$h,$s,$i,$t,$c,$minmax,true")
        }
        else {
          println(f"$h-$s-$i: minmax=$minmax, memoryOut=false, status=${res.status}, cplexStatus=${res.cplexStatus}, objective=${res.objOpt.getOrElse("NA")}, timeElapsed=${res.timeElapsed}%.3f")
          writer.println(f"$h,$s,$i,$t,$c,$minmax,false,${res.status},${res.cplexStatus},${res.objOpt.getOrElse("NA")},${res.timeElapsed}%.3f")
        }

        crp_i.end()
      }

    }
    writer.close()
  }

}
