namespace Flow.Internal

/-- A UUID represented as exactly 16 bytes -/
structure Uuid where
  bytes : { arr : ByteArray // arr.size = 16 }

namespace Uuid

instance : BEq Uuid where
  beq a b := a.bytes.val == b.bytes.val

private def hexChar (n : UInt8) : Char :=
  if n < 10 then Char.ofNat (n.toNat + 48)
  else Char.ofNat (n.toNat + 87)

private def byteToHex (b : UInt8) : String :=
  let hi := b >>> 4
  let lo := b &&& 0x0F
  String.ofList [hexChar hi, hexChar lo]

def toString (uuid : Uuid) : String :=
  let b := uuid.bytes.val
  let hex (i : Nat) : String := byteToHex (b.get! i)
  hex 0 ++ hex 1 ++ hex 2 ++ hex 3 ++ "-" ++
  hex 4 ++ hex 5 ++ "-" ++
  hex 6 ++ hex 7 ++ "-" ++
  hex 8 ++ hex 9 ++ "-" ++
  hex 10 ++ hex 11 ++ hex 12 ++ hex 13 ++ hex 14 ++ hex 15

instance : ToString Uuid where
  toString := Uuid.toString

instance : Repr Uuid where
  reprPrec u _ := repr u.toString

instance : Hashable Uuid where
  hash u := hash u.bytes.val

private def randomByte : IO UInt8 := do
  let b ← IO.rand 0 255
  return b.toUInt8

def v4 : IO Uuid := do
  let b0 ← randomByte
  let b1 ← randomByte
  let b2 ← randomByte
  let b3 ← randomByte
  let b4 ← randomByte
  let b5 ← randomByte
  -- Set version to 4 (bits 4-7 of byte 6)
  let b6 := ((← randomByte)&&& 0x0F) ||| 0x40
  let b7 ← randomByte
  -- Set variant to RFC 4122 (bits 6-7 of byte 8)
  let b8 := ((← randomByte) &&& 0x3F) ||| 0x80
  let b9 ← randomByte
  let b10 ← randomByte
  let b11 ← randomByte
  let b12 ← randomByte
  let b13 ← randomByte
  let b14 ← randomByte
  let b15 ← randomByte
  let bytes := ByteArray.mk
    #[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15]
  return ⟨⟨bytes, rfl⟩⟩

end Uuid

end Flow.Internal
