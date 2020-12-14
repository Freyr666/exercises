package aoc2020.day14

import aoc2020.utils._
import java.util.Map.Entry

object Solution {

  sealed trait Command
  final case class Mask(v: List[Char]) extends Command
  final case class Mem(addr: Long, data: Long) extends Command

  sealed trait Memory {
    import Memory._

    def updated(addr: List[Char], data: Long): Memory = {
      def loop(mem: Memory, addr: List[Char]): Memory =
        (addr, mem) match {
          case (Nil, _) =>
            Data(data)
          case ('X'::tl, NodeX(x)) =>
            NodeX(loop(x, tl))
          case ('1'::tl, NodeX(x)) =>
            NodeB(x, loop(x, tl))
          case ('0'::tl, NodeX(x)) =>
            NodeB(loop(x, tl), x)
          case ('X'::tl, NodeB(z, o)) =>
            NodeB(loop(z, tl), loop(o, tl))
          case ('1'::tl, NodeB(z, o)) =>
            NodeB(z, loop(o, tl))
          case ('0'::tl, NodeB(z, o)) =>
            NodeB(loop(z, tl), o)
          case _ => {
            println("Wrong " + mem)
            ???
          }
        }
      loop(this, addr)
    }

    def sum: Long = {
      def loop(mult: Long, mem: Memory): Long =
        mem match {
          case Data(d) => d * mult
          case NodeX(x) => loop(mult*2L, x)
          case NodeB(z, o) => loop(mult, z) + loop(mult, o)
        }
      loop(1, this)
    }
  }

  object Memory {

    final case class Data(v: Long) extends Memory
    final case class NodeX(x: Memory) extends Memory
    final case class NodeB(z: Memory, o: Memory) extends Memory

    lazy val default: Memory = {
      def loop(n: Int): Memory =
        if (n < 0) Data(0L)
        else NodeX(loop(n-1))
      loop(35)
    }
  }

  def parseCommands(it: Iterator[String]): List[Command] = {
    def parse(s: String): Option[Command] =
      if (s.startsWith("mask")) {
        val Array(_, mask) = s.split(" = ")
        Some(Mask(mask.toList))
      } else {
        val Array(mem, v) = s.split(" = ")
        mem.stripPrefix("mem[").stripSuffix("]")
          .toLongOption.flatMap { addr =>
            v.toLongOption.flatMap { data =>
              Some(Mem(addr, data))
            }
          }
      }
    it.flatMap(parse).toList
  }

  def countMemCellVals(prog: List[Command]): Long = {

    def maskVal(v: List[Char], zeros: Long = 0, ones: Long = 0): (Long, Long) =
      v match {
        case Nil => (zeros, ones)
        case 'X'::tl =>
          maskVal(tl, zeros << 1 | 1, ones << 1)
        case '1'::tl =>
          maskVal(tl, zeros << 1 | 1, ones << 1 | 1)
        case '0'::tl =>
          maskVal(tl, zeros << 1, ones << 1)
        case _ => ???
      }

    def loop(pc: List[Command], mask: (Long,Long), env: Map[Long, Long]): Long =
      pc match {
        case Nil =>
          env.values.sum
        case Mask(v)::rest =>
          loop(rest, maskVal(v), env)
        case Mem(addr, data)::rest => {
          val newEnv = env.updated(addr, (data & mask._1) | mask._2)
          loop(rest, mask, newEnv)
        }
      }
    loop(prog, (0,0), Map.empty)
  }

  def addressMask(prog: List[Command]): Long = {

    def applyMask(mask: List[Char], addr: Long): List[Char] = {
      def binRepr(acc: List[Boolean], n: Int, addr: Long): List[Char] =
        if (n < 0) acc.map(f => if (f) '1' else '0')
        else {
          val bit = addr % 2 == 1
          binRepr(bit::acc, n-1, addr >> 1)
        }
      binRepr(Nil, 35, addr)
        .zip(mask)
        .map {
          case (b, '0') => b
          case (_, '1') => '1'
          case (_, 'X') => 'X'
        }
    }

    def loop(pc: List[Command], mask: List[Char], env: Memory): Long = pc match {
      case Nil =>
        env.sum
      case Mask(v)::rest =>
        loop(rest, v, env)
      case Mem(addr, data)::rest => {
        val addrMask = applyMask(mask, addr)
        loop(rest, mask, env.updated(addrMask, data))
      }
    }

    loop(prog, Nil, Memory.default)
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val commands = getLines(args(0), parseCommands)

    // Task 1
    println("Sum of the values in memory " + countMemCellVals(commands))

    // Task 2
    println("Sum of the values in memory (Part II) " + addressMask(commands))

  }

}
