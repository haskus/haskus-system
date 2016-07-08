
import Criterion.Main
import ViperVM.Format.Binary.Bits.Reverse
import ViperVM.Format.Binary.Word

main :: IO ()
main = do
   let 
      w8  = 0x37               :: Word8
      w16 = 0x3547             :: Word16
      w32 = 0x3547ea87         :: Word32
      w64 = 0x3547ea8712345678 :: Word64
   defaultMain
      [ bgroup "Reverse bits in Word8"
         [ bench "Obvious way"                           $ whnf reverseBitsObvious w8
         , bench "4 64-bit operations, no division"      $ whnf reverseBits4Ops    w8
         , bench "3 64-bit operations, modulus division" $ whnf reverseBits3Ops    w8
         , bench "Lookup table"                          $ whnf reverseBitsTable   w8
         , bench "7 no 64-bit operations, no division"   $ whnf reverseBits7Ops    w8
         , bench "5LgN operations, no division"          $ whnf reverseBits5LgN    w8
         , bench "Currently selected algorithm"          $ whnf reverseBits        w8
         ]
      , bgroup "Reverse bits in Word16"
         [ bench "Obvious way"                           $ whnf (                reverseBitsObvious) w16
         , bench "4 64-bit operations, no division"      $ whnf (liftReverseBits reverseBits4Ops)    w16
         , bench "3 64-bit operations, modulus division" $ whnf (liftReverseBits reverseBits3Ops)    w16
         , bench "Lookup table"                          $ whnf (liftReverseBits reverseBitsTable)   w16
         , bench "7 no 64-bit operations, no division"   $ whnf (liftReverseBits reverseBits7Ops)    w16
         , bench "5LgN operations, no division"          $ whnf (                reverseBits5LgN)    w16
         , bench "Currently selected algorithm"          $ whnf (                reverseBits)        w16
         ]
      , bgroup "Reverse bits in Word32"
         [ bench "Obvious way"                           $ whnf (                reverseBitsObvious) w32
         , bench "4 64-bit operations, no division"      $ whnf (liftReverseBits reverseBits4Ops)    w32
         , bench "3 64-bit operations, modulus division" $ whnf (liftReverseBits reverseBits3Ops)    w32
         , bench "Lookup table"                          $ whnf (liftReverseBits reverseBitsTable)   w32
         , bench "7 no 64-bit operations, no division"   $ whnf (liftReverseBits reverseBits7Ops)    w32
         , bench "5LgN operations, no division"          $ whnf (                reverseBits5LgN)    w32
         , bench "Currently selected algorithm"          $ whnf (                reverseBits)        w32
         ]
      , bgroup "Reverse bits in Word64"
         [ bench "Obvious way"                           $ whnf (                reverseBitsObvious) w64
         , bench "4 64-bit operations, no division"      $ whnf (liftReverseBits reverseBits4Ops)    w64
         , bench "3 64-bit operations, modulus division" $ whnf (liftReverseBits reverseBits3Ops)    w64
         , bench "Lookup table"                          $ whnf (liftReverseBits reverseBitsTable)   w64
         , bench "7 no 64-bit operations, no division"   $ whnf (liftReverseBits reverseBits7Ops)    w64
         , bench "5LgN operations, no division"          $ whnf (                reverseBits5LgN)    w64
         , bench "Currently selected algorithm"          $ whnf (                reverseBits)        w64
         ]
      ]

