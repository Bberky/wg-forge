module WgForge.Cidr (
  networkAddress,
  broadcastAddress,
  rangeBase,
  rangeMask,
) where

import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.IP (AddrRange, IPv4, addrRangePair, fromIPv4w, mlen, toIPv4w)
import Data.Word (Word32)

-- | Retrieve the network address (aka. subnet address) of a CIDR range.
--   This is the lowest address in the range, where all host bits are zero.
networkAddress :: AddrRange IPv4 -> IPv4
networkAddress range = toIPv4w (fromIPv4w (rangeBase range) .&. rangeMask range)

-- | Retrieve the broadcast address of a CIDR range.
--   This is the highest address in the range, where all host bits are one.
broadcastAddress :: AddrRange IPv4 -> IPv4
broadcastAddress range = toIPv4w (base .|. hostBits)
 where
  base = fromIPv4w (rangeBase range) .&. rangeMask range
  hostBits = complement (rangeMask range)

-- | Retrieve the base address of a CIDR range.
rangeBase :: AddrRange IPv4 -> IPv4
rangeBase = fst . addrRangePair

-- | Retrieve the subnet mask of a CIDR range.
rangeMask :: AddrRange IPv4 -> Word32
rangeMask range = complement (1 `shiftL` (32 - mlen range) - 1)
