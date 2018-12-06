
object ADTTest{
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  abstract class LPair
  case class LP(l1: List, l2: List) extends LPair

  def head(l: List): Int = {
    l match {
      case Cons(x, _) => x
      case Nil() => error("head on Nil")
    }
  }
  def tail(l: List): List = {
    l match {
      case Cons(_, t) => t
      case Nil() => error("tail on Nil error")
    }
  }

  def split(l: List): LPair = {
    l match {
      case Cons(h1, Cons(h2, t)) =>
        val rec: LPair = split(t);
        rec match {
          case LP(rec1, rec2) =>
            LP(Cons(h1, rec1), Cons(h2, rec2))
        }
      case _ =>
        LP(l, Nil())
    }
  }

  def foo(): List = { Cons(5, Cons(42, Nil())) }

//  val l: List = Cons(3, Cons(4, Cons(5, Nil())));
//  Std.printInt(head(l));
//  Std.printInt(head(tail(l)));
//
//  val h: Int =
//    l match {
//      case Cons(_, t) =>
//        t match {
//          case Cons(_, t2) =>
//            t2 match {
//              case Cons(x, Cons(i, _)) => x
//            }
//        }
//    };
//
//  Std.printInt(h)

  foo()
}
