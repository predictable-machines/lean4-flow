namespace Flow.Internal

abbrev Program (ψ σ ε α : Type) : Type :=
  ReaderT ψ (ExceptT ε (StateT σ IO)) α

instance : Coe ε (Program ψ σ ε α) where
  coe e := fun _ s => pure (.error e, s)

instance : MonadLift (ExceptT ε IO) (Program ψ σ ε) where
  monadLift ioEx := fun _ s => do
    (·, s) <$> ioEx

instance : MonadLift (ReaderT ψ (ExceptT ε IO)) (Program ψ σ ε) where
  monadLift ioEx := fun c s => do
    (·, s) <$> ioEx c

namespace Program

def run
    (program : Program ψ σ ε α)
    (config : ψ)
    (state : σ)
    : IO ((Except ε α) × σ) :=
  StateT.run (program config) state

def discardState
    (program : Program ψ σ' ε α)
    (state : σ')
    : Program ψ σ ε α := do
  let c ← MonadReader.read
  let (result, _) ← (StateT.run (program c) state : IO _)
  result

end Program

end Flow.Internal
