package Ndarray

import scala.collection.mutable.ArrayBuffer

class LazyVarray:
  private var exprs: ArrayBuffer[Expr] = ArrayBuffer()

  import Expr.*
  import BinOp.*
  import UnOp.*

  def this(expr: Expr) =
    this()
    exprs.append(expr)

  def push(expr: Expr): Unit =
    exprs.append(expr)

  def pushAll(lazzy: LazyVarray, op: BinOp): Unit =
//    val frame = exprs.last
//    exprs.appendAll(lazzy.exprs)
    val expr = BinExpr(exprs.last, op, lazzy.exprs.last)
    exprs.append(expr)


  def +(other: Varray): LazyVarray =
    val expr = BinExpr(exprs.last, add, Arg(other))
    this.push(expr)
    this

  def *(other: Varray): LazyVarray =
    val expr = BinExpr(exprs.last, mul, Arg(other))
    this.push(expr)
    this

  def -(other: Varray): LazyVarray =
    val expr = BinExpr(exprs.last, sub, Arg(other))
    this.push(expr)
    this

  def /(other: Varray): LazyVarray =
    val expr = BinExpr(exprs.last, div, Arg(other))
    this.push(expr)
    this


  def +(other: LazyVarray): LazyVarray =
    this.pushAll(other, add)
    this

  def *(other: LazyVarray): LazyVarray =
    this.pushAll(other, mul)
    this

  def -(other: LazyVarray): LazyVarray =
    this.pushAll(other, sub)
    this

  def /(other: LazyVarray): LazyVarray =
    this.pushAll(other, div)
    this

  def eval(): Varray =
    // recursively calls everything
    exprs.last.eval()

end LazyVarray

