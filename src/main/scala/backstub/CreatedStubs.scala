package backstub

import java.util.concurrent.atomic.AtomicReference

private[backstub] class CreatedStubs:
  private val stubs: AtomicReference[List[Stub[Any]]] = new AtomicReference(Nil)

  def bind[T](stub: Stub[T]): Stub[T] =
    stubs.updateAndGet(stub :: _)
    stub

  def clearAll(): Unit =
    stubs.updateAndGet { stubs =>
      stubs.foreach(_.clear()); stubs
    }
