module WasmNumber (buildNumber, buildWords, decodeNumber, buildString, buildStringU) where
import Data.Binary (Word8, Word32)
import Data.Bits (Bits(shiftR, shiftL, (.&.), (.|.)))
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString as B

buildWords :: Int -> [Word8]
buildWords 0 = []
buildWords nb = rmLastB : buildWords shifted
    where
        rmLastB = (fromIntegral nb :: Word8) .&. 0x7f
        shifted = shiftR nb 7

-- https://en.wikipedia.org/wiki/LEB128
buildNumber :: Int -> [Word8]
buildNumber 0 = [0x00]
buildNumber nb = reverse setHighestByte
    where
        sepBySeven = reverse $ buildWords nb
        tailBitSet = map (.|. 128) (tail sepBySeven)
        setHighestByte = head sepBySeven : tailBitSet

decodeWord :: Word8 -> Int -> Word32
decodeWord byte it = w
    where
        withoutHigh = byte .&. 0x7F
        w :: Word32
        w = shiftL (fromIntegral withoutHigh) (7 * it)

decodeNumber :: [Word8] -> Int
decodeNumber [] = 0
decodeNumber bytes = fromIntegral word
    where
        nbBytes = takeWhile (\b -> (b .&. 128) /= 0) bytes
        stolenBytes = length nbBytes
        addLast = nbBytes ++ [bytes !! stolenBytes]
        word = foldl (\acc (it, b) -> acc + decodeWord b it) 0 (zip [0..stolenBytes] addLast)

buildString :: String -> [Word8]
buildString str = B.unpack (B.pack (map c2w str)) ++ [0x00]

buildStringU :: String -> [Word8]
buildStringU str = B.unpack (B.pack (map c2w str))
