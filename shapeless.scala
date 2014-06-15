package shapeless;

// Semicolons required
// Emtpy argument list required for toString()
// No procedure syntax!
// Path prefix required for println
// No infix constuctors
// In the REPL package prefix required for :: (to override scala.::) and (more
// weirdly HNil) irrespective of imports

trait HList;

final case class ::[+H, +T <: HList](head : H, tail : T) extends HList {
  def ::[H](h : H) = shapeless.::(h, this);
  override def toString() = head.toString()+" :: "+tail.toString()
}

sealed trait HNil extends HList {
  def ::[H](h : H) = shapeless.::(h, this);
  override def toString() = "HNil"
}

object HNil extends HNil;

object Demo {
  def main(args: Array[String]): Unit = {
    val l = 23 :: "foo" :: true :: HNil;
    System.out.println(l)
  }
}
