package backstub

trait Stubs:
  given stubs: CreatedStubs = CreatedStubs()

  def resetStubs(): Unit = stubs.clearAll()
