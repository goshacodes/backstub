package backstub.internal

private[backstub] type UntupleOne[T <: Tuple] = T match
  case head *: EmptyTuple => head
  case _                  => T