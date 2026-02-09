import PredictableFlow.Core.Collector

namespace Flow.Core

/-! # Flows Typeclass

The `Flows` typeclass defines the core interface for stream types that can be collected.
Both cold `Flow` and hot `SharedFlow` implement this interface, enabling shared operators.
-/

/-- Typeclass for stream types that can be collected.

    Any type `F : Type → Type` that implements `Flows` can:
    - Be collected with an action via `collect`
    - Be collected with an explicit Collector via `collectWith`

    This enables operators to work uniformly across Flow and SharedFlow.
-/
class Flows (F : Type → Type) where
  collect : F α → (α → IO Unit) → IO (IO Unit)
  map : F α → (α → β) → IO (F β)
  flush : F α → IO Unit
  filter : F α → (α → Bool) → IO (F α)
  combine : F α → F β → IO (F (Sum α β))
  forEach : F α → (α → IO Unit) → IO Unit :=
    fun flow f => do
      let _ ← collect flow f
      flush flow
  toList : F α → IO (List α) :=
    fun flow => do
      let list ← IO.mkRef ([] : List α)
      forEach flow fun a => do
        list.modify (· ++ [a])
      flush flow
      list.get

class Chooses (F : Type → Type) where
  choose : F α → (α → Option β) → IO (F β)

end Flow.Core
