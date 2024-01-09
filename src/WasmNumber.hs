module WasmNumber (buildNumber) where
import Data.Binary (Word8)
import Data.Bits (Bits(shiftR, (.&.), (.|.)))

buildWords :: Int -> [Word8]
buildWords 0 = []
buildWords nb = rmLastB : buildWords shifted
    where
        rmLastB = (.&.) (fromIntegral nb :: Word8) 0x7f
        shifted = shiftR nb 7

-- https://en.wikipedia.org/wiki/LEB128
buildNumber:: Int -> [Word8]
buildNumber nb = setHighestByte
    where
        sepBySeven = buildWords nb
        tailBitSet = map (.|. 128) (tail sepBySeven)
        setHighestByte = head sepBySeven : tailBitSet