namespace Psychopomp

private def inRange (n a b : Nat) : Bool := decide (a ≤ n ∧ n ≤ b)

def Char.visualWidth (c : Char) : Nat :=
  let n := c.val.toNat
  if n == 0 then 0
  else if
       inRange n 0x0300 0x036F
    || inRange n 0x0483 0x0489
    || inRange n 0x0591 0x05BD
    || n == 0x05BF
    || inRange n 0x05C1 0x05C2
    || inRange n 0x05C4 0x05C5
    || n == 0x05C7
    || inRange n 0x0610 0x061A
    || inRange n 0x064B 0x065F
    || n == 0x0670
    || inRange n 0x06D6 0x06DC
    || inRange n 0x06DF 0x06E4
    || inRange n 0x06E7 0x06E8
    || inRange n 0x06EA 0x06ED
    || n == 0x0711
    || inRange n 0x0730 0x074A
    || inRange n 0x07A6 0x07B0
    || inRange n 0x07EB 0x07F3
    || inRange n 0x0816 0x0819
    || inRange n 0x081B 0x0823
    || inRange n 0x0825 0x0827
    || inRange n 0x0829 0x082D
    || inRange n 0x0859 0x085B
    || inRange n 0x08D4 0x08E1
    || inRange n 0x08E3 0x0902
    || n == 0x093A
    || n == 0x093C
    || inRange n 0x0941 0x0948
    || n == 0x094D
    || inRange n 0x0951 0x0957
    || inRange n 0x0962 0x0963
    || inRange n 0x200B 0x200F             -- ZWSP, ZWJ, ZWNJ, LRM, RLM
    || inRange n 0x202A 0x202E
    || inRange n 0x2060 0x2064
    || inRange n 0x2066 0x206F
    || inRange n 0xFE00 0xFE0F             -- Variation Selectors
    || n == 0xFEFF                         -- BOM / ZWNBSP
    || inRange n 0xE0100 0xE01EF           -- Variation Selectors Supplement
  then 0
  -- East Asian Wide / Fullwidth and major emoji blocks.
  else if
       inRange n 0x1100 0x115F             -- Hangul Jamo
    || inRange n 0x2E80 0x303E             -- CJK Radicals, Kangxi, Punctuation
    || inRange n 0x3041 0x33FF             -- Hiragana, Katakana, Bopomofo, Hangul Compat, CJK Symbols
    || inRange n 0x3400 0x4DBF             -- CJK Unified Ideographs Extension A
    || inRange n 0x4E00 0x9FFF             -- CJK Unified Ideographs
    || inRange n 0xA000 0xA4CF             -- Yi Syllables
    || inRange n 0xAC00 0xD7A3             -- Hangul Syllables
    || inRange n 0xF900 0xFAFF             -- CJK Compatibility Ideographs
    || inRange n 0xFE30 0xFE4F             -- CJK Compatibility Forms
    || inRange n 0xFF00 0xFF60             -- Fullwidth Forms
    || inRange n 0xFFE0 0xFFE6             -- Fullwidth signs
    || inRange n 0x1F300 0x1F5FF           -- Misc Symbols & Pictographs
    || inRange n 0x1F600 0x1F64F           -- Emoticons
    || inRange n 0x1F680 0x1F6FF           -- Transport & Map
    || inRange n 0x1F900 0x1F9FF           -- Supplemental Symbols & Pictographs
    || inRange n 0x1FA70 0x1FAFF           -- Symbols & Pictographs Extended-A
    || inRange n 0x20000 0x2FFFD           -- CJK Extension B+ + Supplementary Ideographs
    || inRange n 0x30000 0x3FFFD           -- CJK Extension G/H
  then 2
  else 1

def String.visualWidth (s : String) : Nat :=
  s.toList.foldl (fun acc c => acc + Char.visualWidth c) 0

end Psychopomp
