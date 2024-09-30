package backstub

import effect.StubEffect

trait Stubs:
  final given stubs: CreatedStubs = CreatedStubs()

  final def resetStubs(): Unit = stubs.clearAll()

  final def resetStubsIO[F[+_, +_]: StubEffect]: F[Nothing, Unit] =
    summon[StubEffect[F]].unit(resetStubs())
