package backstub

import backstub.effect.StubEffect
import cats.effect.IO

trait CatsEffectStubs extends Stubs:
  given StubEffect.Mono[IO] = new StubEffect.Mono[IO]:
    def unit[T](t: => T): IO[T] = IO(t)
    def flatMap[E, EE >: E, T, T2](fa: IO[T])(f: T => IO[T2]): IO[T2] = fa.flatMap(f)

