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

final case class ::[+H, +T](head : H, tail : T) extends HList {
  def ::[H](h : H) = shapeless.::(h, this);
  override def toString() = head.toString()+" :: "+tail.toString()
}

sealed trait HNil extends HList {
  def ::[H](h : H) = shapeless.::(h, this);
  override def toString() = "HNil"
}

object HNil extends HNil;

trait Reverse[L, M, Out] {
  def revAcc(m: M): Out;
}

object HList {
  def view[M](l: HNil):
    Reverse[HNil, M, M] =
      new Reverse[HNil, M, M] {
        def revAcc(m: M): M = m
      }

  def view[H, M, T <% Reverse[T, ::[H, M], Out], Out](l: ::[H, T]):
    Reverse[::[H, T], M, Out] =
      new Reverse[::[H, T], M, Out] {
        val tlReverse: Reverse[T, ::[H, M], Out] = l.tail;
        def revAcc(m: M): Out = tlReverse.revAcc(::(l.head, m))
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

    val ops: Reverse[ISB, HNil, BSI] = l;
    val m = ops.revAcc(HNil);
    //val m = l.revAcc(HNil);
    typed[BSI](m);
    System.out.println(m);
  }
}
