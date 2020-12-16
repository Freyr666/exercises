package aoc2020.day16

import aoc2020.utils._

object Solution {

  final case class Constr(name: String, ranges: List[Range])
  type Ticket = List[Int]

  def parseTicket(it: Iterator[String]): Option[(List[Constr], Ticket, List[Ticket])] = {

    def parseConstr(s: String) = {
      val Array(name, c) = s.split(':')
      val ranges = c
        .split("or")
        .map { r =>
          val Array(f, t) = r // " 1-3 "
            .trim() // "1-3"
            .split('-') // "1", "3"
            .map(_.toInt) // 1, 3
          f to t
        }
      Constr(name, ranges.toList)
    }

    def parseTicket(s: String): Ticket =
      s.split(',').view.flatMap(_.toIntOption).toList

    def stageConstr(
      c: List[Constr]
    ): Option[(List[Constr], Ticket, List[Ticket])] =
      it.nextOption() match {
        case None => None
        case Some("") => stageTicket(c)
        case Some(s) => stageConstr(parseConstr(s)::c)
      }

    def stageTicket(
      c: List[Constr],
      t: Option[Ticket] = None
    ): Option[(List[Constr], Ticket, List[Ticket])] =
      it.nextOption() match {
        case None => None
        case Some("your ticket:") => stageTicket(c)
        case Some("") => t.flatMap(stageOthers(c, _))
        case Some(s) => stageTicket(c, Some(parseTicket(s)))
      }

    def stageOthers(
      c: List[Constr],
      t: Ticket,
      o: List[Ticket] = Nil
    ): Option[(List[Constr], Ticket, List[Ticket])] =
      it.nextOption() match {
        case None => Some((c, t, o))
        case Some("nearby tickets:") => stageOthers(c, t)
        case Some(s) => stageOthers(c, t, parseTicket(s)::o)
      }

    stageConstr(Nil)
  }

  def findOutOfBoundsSum(constr: List[Constr], tickets: List[Ticket]): Long = {
    val ranges = constr.flatMap(_.ranges)
    tickets.view
      .flatMap(identity)
      .filter(i => ! ranges.exists(_.contains(i)))
      .foldLeft(0L)(_ + _)
  }

  def determFields(constr: List[Constr], my: Ticket, nearby: List[Ticket]): Long = {

    val proper = {
      val ranges = constr.flatMap(_.ranges)
      nearby.filter(t => t.forall(i => ranges.exists(_.contains(i))))
    }

    def narrowConstr(constr: List[List[Constr]], tickets: List[Ticket]): List[List[String]] =
      tickets match {
        case Nil => constr.map(l => l.map(_.name))
        case t::rest => {
          val upd = constr
            .zip(t)
            .map { case ((cs, num)) =>
              cs.filter(_.ranges.exists(r => r.contains(num)))
            }
          narrowConstr(upd, rest)
        }
      }

    def removeRedundant(constr: List[List[String]]): List[String] =
      if (! constr.exists(_.length > 1)) constr.map(_.head)
      else {
        val singletons = constr.flatMap {
          case l @ h::Nil => l
          case _ => Nil
        }
        removeRedundant(constr.map { l =>
          if (l.length == 1) l
          else l.filter(el => ! singletons.contains(el))
        })
      }

    val named =
      removeRedundant(narrowConstr(my.map(_ => constr), proper))
        .zip(my)
        .toMap

    println(named)

    val departureNames = constr.flatMap { c =>
      if (c.name.matches("departure.*")) Some(c.name)
      else None
    }

    departureNames.map(named(_)).foldLeft(1L)(_ * _)
  }

  def main(args: Array[String]) = {
    require(args.length == 1)

    val (constr, ticket, otherTickets) =
      getLines(args(0), parseTicket).get

    // Task 1
    println("Scanning error rate: " + findOutOfBoundsSum(constr, otherTickets))

    // Task 2
    println("Departure prod: " + determFields(constr, ticket, otherTickets))
  }

}
