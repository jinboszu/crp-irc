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

import scala.reflect.ClassTag

class Matrix[T: ClassTag](n1: Int, n2: Int, data: Array[Array[T]]) {

  def apply(i: Int, j: Int): T = data(i)(j)

  def update(i: Int, j: Int, v: T): Unit = data(i)(j) = v

  override def clone() = new Matrix(n1, n2, data.map(_.clone()))

  def mkString(iFrom: Int, iTo: Int, jFrom: Int, jTo: Int): String = {
    val indexWidth = iTo.toString.length
    val indexFmt = s"%${indexWidth}d"
    var cellWidth = jTo.toString.length
    for (i <- iFrom to iTo)
      for (j <- jFrom to jTo)
        cellWidth = math.max(cellWidth, data(i)(j).toString.length)
    val cellFmt = s"%${cellWidth}s"

    val sb = new StringBuilder()
    sb ++= "[" + " " * indexWidth
    for (j <- jFrom to jTo)
      sb ++= s" ${cellFmt.format(j)}"
    sb ++= "\n"
    for (i <- iFrom to iTo) {
      sb ++= s" ${indexFmt.format(i)}"
      for (j <- jFrom to jTo)
        sb ++= s" ${cellFmt.format(data(i)(j))}"
      sb ++= "\n"
    }
    sb ++= "]"
    sb.result()
  }
}

object Matrix {
  def ofDim[T: ClassTag](n1: Int, n2: Int) = new Matrix(n1, n2, Array.ofDim[T](n1, n2))
}
