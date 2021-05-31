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

case class Layout(S: Int, T: Int, C: Int, bay: Matrix[Int], h: Array[Int]) {

  override def toString: String = {
    val width = C.toString.length
    val line = "-" * ((width + 3) * S + 1)
    val empty = " " * width
    val fmt = s"%${width}d"

    val sb = new StringBuilder()
    sb ++= s"$line\n"
    for (t <- T to 1 by -1) {
      for (s <- 1 to S) {
        if (bay(s, t) == 0)
          sb ++= s"| $empty "
        else
          sb ++= s"| ${fmt.format(bay(s, t))} "
      }
      sb ++= "|\n"
    }
    sb ++= line
    sb.result()
  }

}
