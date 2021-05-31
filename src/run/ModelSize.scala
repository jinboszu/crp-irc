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

object ModelSize {
  def main(args: Array[String]): Unit = {

    val file = "results/crp/modelsize-caserta.csv"
    println(s"save results to $file")

    new File(file).getParentFile.mkdirs()
    val writer = new PrintWriter(new FileWriter(file), true)

    writer.println("H,S,C,numVarsCRP_Ir,numConsCRP_Ir,sizeCRP_Ir,numVarsCRP_Irc,numConsCRP_Irc,sizeCRP_Irc")

    val HS = List(
      (3, 3), (3, 4), (3, 5), (3, 6), (3, 7), (3, 8),
      (4, 4), (4, 5), (4, 6), (4, 7),
      (5, 4), (5, 5), (5, 6), (5, 7), (5, 8), (5, 9), (5, 10),
      (6, 6), (6, 10),
      (10, 6), (10, 10)
    )

    for ((h, s) <- HS) {
      val S = s
      val C = h * s
      val N = C - S + 1

      val numVarsCRP_Ir = (2.0 * C * C * C + 3.0 * (1.0 + S) * C * C + (1.0 + 3.0 * S) * C - (6.0 - 5.0 * S - 6.0 * S * S + 5.0 * S * S * S)) / 6.0 + (1.0 * C * C - C - 1.0 * S * S + S) / 2.0
      val numConsCRP_Ir = (4.0 * (2.0 + S) * C * C * C - 3.0 * (4.0 + S) * C * C + (10.0 + 11.0 * S) * C - (13.0 - 16.0 * S + 5.0 * S * S + 4.0 * S * S * S) * S) / 6.0 + (1.0 * C * C - C - 1.0 * S * S + S) / 2.0 + (6.0 * C * C * C * C * C + 15.0 * (-4.0 + S) * C * C * C * C + 10.0 * (21.0 - 10.0 * S + 1.0 * S * S) * C * C * C - 15.0 * (20.0 - 13.0 * S + 2.0 * S * S) * C * C + 2.0 * (72.0 - 55.0 * S + 10.0 * S * S) * C + (-144.0 + 410.0 * S - 425.0 * S * S + 190.0 * S * S * S - 31.0 * S * S * S * S) * S) / 30.0
      val sizeCRP_Ir = numVarsCRP_Ir * numConsCRP_Ir

      val numVarsCRP_Irc = (2.0 * C * C * C + 3.0 * (1.0 + S) * C * C + (1.0 + 3.0 * S) * C - (6.0 - 5.0 * S - 6.0 * S * S + 5.0 * S * S * S)) / 6.0 + (1.0 * C * C - C - 1.0 * S * S + S) / 2.0 + 1.0 * C * C * N - 1.0 * C * C - 1.0 * C * N * N + 1.0 * C * N + 1.0 * N * N * N / 3.0 - 1.0 * N * N / 2.0 + 1.0 * N / 6.0
      val numConsCRP_Irc = 2.0 * C * C * C * S / 3.0 + 11.0 * C * C * C / 6.0 - 1.0 * C * C * S / 2.0 - 3.0 * C * C + 11.0 * C * S / 6.0 + 13.0 * C / 6.0 - 2.0 * S * S * S * S / 3.0 - 4.0 * S * S * S / 3.0 + 11.0 * S * S / 3.0 - 8.0 * S / 3.0
      val sizeCRP_Irc = numVarsCRP_Irc * numConsCRP_Irc

      writer.println(f"$h,$s,$C,$numVarsCRP_Ir,$numConsCRP_Ir,$sizeCRP_Ir,$numVarsCRP_Irc,$numConsCRP_Irc,$sizeCRP_Irc")

    }
    writer.close()
  }

}
