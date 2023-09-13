package Ndarray

import scala.collection.mutable.ArrayBuffer

enum BinOp(val f: (Varray, Varray)=>Varray):
  case add extends BinOp((lhs: Varray, rhs: Varray) => lhs += rhs)
  case sub extends BinOp((lhs: Varray, rhs: Varray) => lhs -= rhs)
  case mul extends BinOp((lhs: Varray, rhs: Varray) => lhs *= rhs)
  case div extends BinOp((lhs: Varray, rhs: Varray) => lhs /= rhs)
end BinOp

enum UnOp(val f: Varray=>Varray):
  case negate  extends UnOp(x => x.inegate())
  case reverse extends UnOp(x => x.ireverse())
end UnOp

enum Op[A]:
  case binOp(f: (A, A) => A)
  case unOp(f: A => A)
end Op


enum Expr:
  case Arg(val arg: Varray)
  case LazyArg(val arg: LazyVarray)
  case BinExpr(val lhs: Expr, val op: Op.binOp[Varray], val rhs: Expr)
  case UnExpr(val arg: Expr, val op: Op.unOp[Varray])
end Expr

extension(expr: Expr)
  def eval(): Varray =
    import Expr.*
    expr match
      case Arg(varr) => varr
      case LazyArg(larr) => larr.eval()
      case BinExpr(lhs, op, rhs) => op.f(lhs.eval(), rhs.eval())
      case UnExpr(arg, op) => op.f(arg.eval())

end extension

class Varray():
  var data: ArrayBuffer[Int] = ArrayBuffer()

  import Expr.*
//  import BinOp.*
//  import UnOp.*

  def this(other: ArrayBuffer[Int]) =
    this()
    data = other

  def this(other: Varray) =
    this()
    data = other.data.clone()

  def this(length: Int) =
    this()
    data = ArrayBuffer.range(0, length)

  def imap(f: Int => Int): Varray =
    for i <- data.indices do {
      data(i) = f(data(i))
    }
    this

  def map(f: Int => Int): LazyVarray =
    null

  def +=(other: Varray): Varray =
    for i <- data.indices do {
      data(i) += other.data(i)
    }
    this

  def *=(other: Varray): Varray =
    for i <- data.indices do {
      data(i) *= other.data(i)
    }
    this

  def -=(other: Varray): Varray =
    for i <- data.indices do {
      data(i) -= other.data(i)
    }
    this

  def /=(other: Varray): Varray =
    for i <- data.indices do {
      data(i) /= other.data(i)
    }
    this

  def inegate(): Varray =
    for i <- data.indices do {
      data(i) *= -1
    }
    this


  def ireverse(): Varray =
    for i <- data.indices do {
      data(i) = 1 / data(i)
    }
    this

  def -(): LazyVarray =
    LazyVarray(UnExpr(Arg(this), Op.unOp(x => -x)))

  def +(other: Varray): LazyVarray =
    val out = Varray(this) += other
    LazyVarray(Arg(out))
   
  def *(other: Varray): LazyVarray =
    val out = Varray(this) *= other
    LazyVarray(Arg(out))

  def -(other: Varray): LazyVarray =
    val out = Varray(this) -= other
    LazyVarray(Arg(out))

  def /(other: Varray): LazyVarray =
    val out = Varray(this) /= other
    LazyVarray(Arg(out))

end Varray



