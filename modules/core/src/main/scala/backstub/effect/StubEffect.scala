package backstub.effect

trait StubEffect[F[+_, +_]]:
  def unit[T](t: => T): F[Nothing, T]

  def flatMap[E, EE >: E, T, T2](fa: F[E, T])(f: T => F[EE, T2]): F[EE, T2]

  private[backstub]
  def flatMapNotFailing[EE, T, T2](fa: F[Nothing, T])(f: T => F[EE, T2]): F[EE, T2] =
    flatMap(fa)(f)


object StubEffect:
  trait Mono[F[+_]]:
    def unit[T](t: => T): F[ T]
  
    def flatMap[E, EE >: E, T, T2](fa: F[T])(f: T => F[T2]): F[T2]
  
    private[backstub]
    def flatMapNotFailing[EE, T, T2](fa: F[T])(f: T => F[T2]): F[T2] =
      flatMap(fa)(f)


