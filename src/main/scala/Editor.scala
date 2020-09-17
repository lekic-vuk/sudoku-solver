package main.scala

trait Editor {
  type Action[S] = Editor => (S, Editor)
  def unit[S](s: S): Action[S] =
    action => (s, action)
    
  def map[A, B](a: Action[A])(f: A => B): Action[B] = 
    edt => {
      val (s, edt1) = a(edt)
      (f(s), edt1)
    }
    
  def map2[A, B, C](a1: Action[A], a2: Action[B])(f: (A, B) => C): Action[C] =
    edt => {
      val (s1, edt1) = a1(edt)
      val (s2, edt2) = a2(edt1)
      (f(s1, s2), edt2)
    }
    
  def sequence[A](fs: List[Action[A]]): Action[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
}