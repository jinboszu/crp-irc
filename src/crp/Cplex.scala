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

import crp.Cplex.RichNumExpr
import ilog.concert.{IloConstraint, IloNumExpr, IloRange}
import ilog.cplex.IloCplex

trait Cplex {

  def sum(seq: Iterable[IloNumExpr])(implicit m: IloCplex): IloNumExpr = m.sum(seq.toArray)

  implicit def asRichNumExpr(v: Int)(implicit m: IloCplex): RichNumExpr = new RichNumExpr(m, m.constant(v))

  implicit def asRichNumExpr(e: IloNumExpr)(implicit m: IloCplex): RichNumExpr = new RichNumExpr(m, e)

}

object Cplex {

  class RichNumExpr(m: IloCplex, expr: IloNumExpr) {

    def unary_-(): IloNumExpr = m.negative(expr)

    def +(e: IloNumExpr): IloNumExpr = m.sum(expr, e)

    def +(v: Double): IloNumExpr = m.sum(expr, v)

    def -(e: IloNumExpr): IloNumExpr = m.diff(expr, e)

    def -(v: Double): IloNumExpr = m.diff(expr, v)

    def *(e: IloNumExpr): IloNumExpr = m.prod(expr, e)

    def *(v: Double): IloNumExpr = m.prod(expr, v)

    def ===(e: IloNumExpr): IloConstraint = m.eq(expr, e)

    def ===(v: Double): IloRange = m.eq(expr, v)

    def <=(e: IloNumExpr): IloConstraint = m.le(expr, e)

    def <=(v: Double): IloRange = m.le(expr, v)

    def >=(e: IloNumExpr): IloConstraint = m.ge(expr, e)

    def >=(v: Double): IloRange = m.ge(expr, v)
  }

}