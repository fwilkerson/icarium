{- | ULID identifiers. Spec: https://github.com/ulid/spec

Randomness comes from 'randomRIO'. Under GHC, splitmix seeds its global
generator through a C shim over the OS CSPRNG (@SecRandomCopyBytes@ on macOS,
@getentropy@ elsewhere); the clock-based fallback visible in its @Init.hs@ is
CPP-gated to non-GHC compilers, which is easy to misread. That distinction
matters because dispatch runs concurrent icarium processes: a clock-seeded
generator would hand identical ids to two processes starting in the same
millisecond.

Callers compare ids to tell which entity is newer. That works across CLI
invocations because startup is ~20ms against a 1ms timestamp field, so no
two invocations share a timestamp. It does not hold within one process: ids
minted in the same millisecond order randomly. A path that mints several ids
per process needs the spec's monotonic factory, which is not implemented.
-}
module Icarium.Id (
    newId,
    encodeUlid,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomRIO)

newId :: IO Text
newId = do
    ms <- floor . (* 1000) <$> getPOSIXTime
    encodeUlid ms <$> randomRIO (0, randMask)

{- | Render a millisecond timestamp and 80 bits of randomness as a ULID.

Both arguments are masked to their field widths rather than rejected, so the
result is always 26 characters.
-}
encodeUlid :: Integer -> Integer -> Text
encodeUlid ms rand =
    T.pack [digit ((payload `shiftR` (5 * i)) .&. 0x1f) | i <- [25, 24 .. 0]]
  where
    payload = ((ms .&. tsMask) `shiftL` 80) .|. (rand .&. randMask)
    -- masked to 5 bits above, so the index is always within the alphabet
    digit d = T.index alphabet (fromInteger d)

tsMask :: Integer
tsMask = (1 `shiftL` 48) - 1

randMask :: Integer
randMask = (1 `shiftL` 80) - 1

-- Crockford base32: I, L, O and U are omitted so IDs survive transcription.
alphabet :: Text
alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
