package shapeless;

// Semicolons required
// Emtpy argument list required for toString()
// No procedure syntax!
// Path prefix required for println
// No infix constuctors
// In the REPL package prefix required for :: (to override scala.::) and (more
// weirdly HNil) irrespective of imports
// No tuple syntax
// Views apply to single values
// Can't have both view bounds and upper bounds
// Type members with type args can't be abstract
// error is in Predef

trait HList;

final case class ::[H, T](head : H, tail : T) extends HList {
  def ::[H](h : H) = shapeless.::(h, this);
  override def toString() = head.toString()+" :: "+tail.toString()
}

sealed trait HNil extends HList {
  def ::[H](h : H) = shapeless.::(h, this);
  override def toString() = "HNil"
}

object HNil extends HNil;

abstract class Ops[L, M] {
  type Out;
  def revPref(m: M): Out
}

object HList {
  def view[M](l: HNil) = new Ops[HNil, M] {
    type Out = M;
    def revPref(m: M): Out = m
  }

  def view[H, M, T <% Ops[T, ::[H, M]]](l: ::[H, T]): Ops[::[H, T], M] = new Ops[::[H, T], M] {
    val tlOps: Ops[T, ::[H, M]] = l.tail;
    type Out = tlOps.Out;
    def revPref(m: M): Out = tlOps.revPref(::(l.head, m))
  }
}

object Demo {
  type ISB = ::[Int, ::[String, ::[Boolean, HNil]]];
  type BSI = ::[Boolean, ::[String, ::[Int, HNil]]];

  def typed[T](t: => T) = ();

  def main(args: Array[String]): Unit = {
    val l = 23 :: "foo" :: true :: HNil;
    typed[ISB](l);
    System.out.println(l);

    val ops: Ops[ISB, HNil] = l;
    val m = ops.revPref(HNil);
    //typed[BSI](m);
    System.out.println(m)
  }
}
