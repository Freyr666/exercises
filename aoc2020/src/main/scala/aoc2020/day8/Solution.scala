package aoc2020.day8

import aoc2020.utils._

object Solution {

  sealed trait Instruction
  final case class Acc(v: Int) extends Instruction
  final case class Jmp(off: Int) extends Instruction
  final case class Nop(dum: Int) extends Instruction

  type Mem = Vector[Option[Instruction]]

  def parseInst(it: Iterator[String]): Vector[Option[Instruction]] = {
    def parseLine(s: String): Option[Instruction] = {
      val Array(inst, v) = s.split(" ")
      inst match {
        case "acc" => v.toIntOption.map(Acc(_))
        case "jmp" => v.toIntOption.map(Jmp(_))
        case "nop" => v.toIntOption.map(Nop(_))
        case _ => None
      }
    }
    it.map(parseLine).toVector
  }

  def detectLoop(pc: Int, acc: Int, mem: Mem): Int =
    mem(pc) match {
      case None =>
        acc
      case Some(Nop(_)) =>
        detectLoop(pc+1, acc, mem.updated(pc, None))
      case Some(Jmp(off)) =>
        detectLoop(pc+off, acc, mem.updated(pc, None))
      case Some(Acc(v)) =>
        detectLoop(pc+1, acc+v, mem.updated(pc, None))
    }

  def fixLoop(pc: Int, acc: Int, mem: Mem): Int = {

    sealed trait State
    final case object Loop extends State
    final case class Term(acc: Int) extends State
    final case class Next(pc: Int, acc: Int, mem: Mem) extends State
    final case class Mod(pc: Int, acc: Int, mem: Mem) extends State

    def step(state: State): Seq[State] =
      state match {
        case Loop => Seq()
        case Term(_) => assert(false); Seq()
        case Mod(pc, acc, mem) =>
          if (pc >= mem.length) Seq(Term(acc))
          else mem(pc) match {
            case None => Seq(Loop)
            case Some(Acc(v)) =>
              Seq(Mod(pc+1, acc+v, mem.updated(pc,None)))
            case Some(Nop(_)) =>
              Seq(Mod(pc+1, acc, mem.updated(pc,None)))
            case Some(Jmp(off)) =>
              Seq(Mod(pc+off, acc, mem.updated(pc,None)))
          }
        case Next(pc, acc, mem) =>
          if (pc >= mem.length) Seq(Term(acc))
          else mem(pc) match {
            case None => Seq(Loop)
            case Some(Acc(v)) =>
              Seq(Next(pc+1, acc+v, mem.updated(pc,None)))
            case Some(Nop(off)) =>
              Seq(
                Next(pc+1, acc, mem.updated(pc,None)),
                Mod(pc+off, acc, mem.updated(pc,None))
              )
            case Some(Jmp(off)) =>
              Seq(
                Mod(pc+1, acc, mem.updated(pc,None)),
                Next(pc+off, acc, mem.updated(pc,None))
              )
          }
      }
      
    def loop(states: Seq[State]): Int = {
      val (terminated, executing) =
        states
          .flatMap(step)
          .partition {
            case Term(_) => true
            case _ => false
          }
      terminated match {
        case Term(acc)+:_ => acc
        case _ => loop(executing)
      }
    }
    loop(Seq(Next(pc, acc, mem)))
  }


  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val code = getLines(args(0), parseInst)

    // Task 1

    println("Acc value before inf loop: " +
      detectLoop(0, 0, code)
    )

    // Task 2

    println("Acc value after termination: " +
      fixLoop(0, 0, code)
    )
  }

}
