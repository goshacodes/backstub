package backstub

import zio.*

trait ZIOStubs extends Stubs:
  given effect.StubEffect[IO] with
    def unit[T](t: => T): UIO[T] = ZIO.succeed(t)
    def flatMap[E, EE >: E, T, T2](fa: IO[E, T])(f: T => IO[EE, T2]): IO[EE, T2] = fa.flatMap(f)
