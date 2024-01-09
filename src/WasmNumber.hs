module WasmNumber (buildNumber, decodeNumber) where
import Data.Binary (Word8, Word32)
import Data.Bits (Bits(shiftR, shiftL, (.&.), (.|.)))

buildWords :: Int -> [Word8]
buildWords 0 = []
buildWords nb = rmLastB : buildWords shifted
    where
        rmLastB = (.&.) (fromIntegral nb :: Word8) 0x7f
        shifted = shiftR nb 7

-- https://en.wikipedia.org/wiki/LEB128
buildNumber :: Int -> [Word8]
buildNumber nb = setHighestByte
    where
        sepBySeven = buildWords nb
        tailBitSet = map (.|. 128) (tail sepBySeven)
        setHighestByte = head sepBySeven : tailBitSet

decodeWord :: Word8 -> Int -> Word32
decodeWord byte it = w
    where
        withoutHigh = shiftR byte 1
        w :: Word32
        w = shiftL (fromIntegral withoutHigh) (8 * it)

decodeNumber :: [Word8] -> (Int, Int)
decodeNumber bytes = (fromIntegral word, stolenBytes)
    where
        nbBytes = takeWhile (\b -> (b .&. 128) /= 0) bytes
        stolenBytes = length nbBytes
        word = foldl (\acc (it, b) -> acc + decodeWord b it) 0 (zip [0..stolenBytes] nbBytes)