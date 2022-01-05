/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package utils

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._

class CircularQueuePtr[T <: CircularQueuePtr[T]](val entries: Int) extends Bundle {

  def this(f: Parameters => Int)(implicit p: Parameters) = this(f(p))

  // 这是循环队列指针的通用表达方式，由 {flag, ptr} 组成
  val PTR_WIDTH = log2Up(entries)
  val flag = Bool()
  val value = UInt(PTR_WIDTH.W)

  // 用 printf 打印的时候，会调用该模块的 toPrintable 函数，如果没有 override，则就使用默认的
  override def toPrintable: Printable = {
    p"$flag:$value"
  }

  final def +(v: UInt): T = {
    val entries = this.entries
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    if(isPow2(entries)){
      new_ptr := (Cat(this.flag, this.value) + v).asTypeOf(new_ptr)  // 这里有点意思，其实 flag 是作为一个进位符来使用的
    } else {
      val new_value = this.value +& v
      val diff = Cat(0.U(1.W), new_value).asSInt() - Cat(0.U(1.W), entries.U.asTypeOf(new_value)).asSInt()  // 如果 new_value 溢出了，就要 reverse flag
      val reverse_flag = diff >= 0.S
      new_ptr.flag := Mux(reverse_flag, !this.flag, this.flag)
      new_ptr.value := Mux(reverse_flag,
        diff.asUInt(),
        new_value
      )
    }
    new_ptr
  }

  final def -(v: UInt): T = {
    val flipped_new_ptr = this + (this.entries.U - v)  // “减法就是加法”
    val new_ptr = Wire(this.asInstanceOf[T].cloneType)
    new_ptr.flag := !flipped_new_ptr.flag
    new_ptr.value := flipped_new_ptr.value
    new_ptr
  }

  // 判断相等和不等需要同时比较 flag 和 ptr
  final def === (that_ptr: T): Bool = this.asUInt()===that_ptr.asUInt()

  final def =/= (that_ptr: T): Bool = this.asUInt()=/=that_ptr.asUInt()
}

trait HasCircularQueuePtrHelper {

  def isEmpty[T <: CircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    enq_ptr === deq_ptr
  }

  def isFull[T <: CircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): Bool = {
    (enq_ptr.flag =/= deq_ptr.flag) && (enq_ptr.value === deq_ptr.value)
  }

  def distanceBetween[T <: CircularQueuePtr[T]](enq_ptr: T, deq_ptr: T): UInt = {
    assert(enq_ptr.entries == deq_ptr.entries)
    Mux(enq_ptr.flag === deq_ptr.flag,
      enq_ptr.value - deq_ptr.value,
      enq_ptr.entries.U + enq_ptr.value - deq_ptr.value)
  }

  def isAfter[T <: CircularQueuePtr[T]](left: T, right: T): Bool = {
    val differentFlag = left.flag ^ right.flag
    val compare = left.value > right.value
    differentFlag ^ compare
  }

  def isBefore[T <: CircularQueuePtr[T]](left: T, right: T): Bool = {
    val differentFlag = left.flag ^ right.flag
    val compare = left.value < right.value
    differentFlag ^ compare
  }
}
