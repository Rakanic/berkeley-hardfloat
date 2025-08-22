
/*============================================================================

This Chisel source file is part of a pre-release version of the HardFloat IEEE
Floating-Point Arithmetic Package, by John R. Hauser (with some contributions
from Yunsup Lee and Andrew Waterman, mainly concerning testing).

Copyright 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017 The Regents of the
University of California.  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions, and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions, and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the University nor the names of its contributors may
    be used to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE
DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=============================================================================*/

package hardfloat

import chisel3._

object rawFloatFromFN {
  def apply(_expWidth: Int, _sigWidth: Int, in: Bits) = {
    val sign = in(_expWidth + _sigWidth - 1)
    val _expIn = in(_expWidth + _sigWidth - 2, _sigWidth - 1)
    val _fractIn = in(_sigWidth - 2, 0)

    val isFP4 = (_expWidth + _sigWidth) == 4

    val (expIn, fractIn, expWidth, sigWidth, adjustedSubnormExp) =
      if (_expWidth + _sigWidth == 8) { 
        if (_expWidth == 4)
          (((in(_expWidth + _sigWidth - 2, 0).andR).asUInt ## _expIn) + 
          Mux(_expIn.andR && _fractIn.andR, 0.U, 8.U), _fractIn, 5, _sigWidth, true.B)
        else
          (_expIn, _fractIn ## 0.U(1.W), _expWidth, 4, false.B)
      } else if (_expWidth + _sigWidth == 4) {
        ((0.U(1.W) ## _expIn) + 2.U, _fractIn, 3, 2, false.B)
      } else {
          (_expIn, _fractIn, _expWidth, _sigWidth, false.B)
      }

    val isZeroExpIn = (_expIn === 0.U)
    val isZeroFractIn = (_fractIn === 0.U)

    val normDist = countLeadingZeros(fractIn)

    val subnormFract = 
      if (!isFP4) {
        (fractIn << normDist) (sigWidth - 3, 0) << 1
      } else {
        0.U((sigWidth - 1).W)
      }

    val adjustedExp =
      if (!isFP4) {
        Mux(isZeroExpIn,
          normDist ^ ((BigInt(1) << (expWidth + 1)) - 1).U,
          expIn
        ) + ((BigInt(1) << (expWidth - 1)).U + Mux(isZeroExpIn && adjustedSubnormExp, 8.U, 0.U)
          | Mux(isZeroExpIn, 2.U, 1.U))
      } else {
        // 0b00111.U
        Mux(isZeroExpIn,
          0b00111.U(5.W),
          expIn +& ((BigInt(1) << (expWidth - 1)).U | 1.U))
      }

    val isZero = isZeroExpIn && isZeroFractIn
    val isSpecial = adjustedExp(expWidth, expWidth - 1) === 3.U

    val out = Wire(new RawFloat(expWidth, sigWidth))
    out.isNaN := isSpecial && !isZeroFractIn
    out.isInf := isSpecial && isZeroFractIn
    out.isZero := isZero
    out.sign := sign
    out.sExp := adjustedExp(expWidth, 0).zext
    out.sig :=
      0.U(1.W) ## !isZero ## Mux(isZeroExpIn, subnormFract, fractIn)
    out
  }
}

// object rawFromF8e4m3 {
//   def apply(expWidth: Int, sigWidth: Int, in: Bits) = {
//     val sign    = in(7)
//     val expIn   = in(6, 3)  // exponent field
//     val fractIn = in(2, 0)  // fraction field
//     val bias    = 7
//     val expWidthOut = 5
//     val sigWidthOut = 11
//     val biasOut = 15

//     val isZeroExpIn = (expIn === 0.U)
//     val isZero = isZeroExpIn && fractIn === 0.U
//     // E4M3 has no infinity only NaN:
//     val isNaN = expIn === "b1111".U && fractIn === "b111".U

//     // Compute the signed, unbiased exponent (sExp)
//     val sExp = Wire(SInt((expWidth + 2).W))
//     when (expIn === 0.U) {
//       // subnormal
//       sExp := (1.S - bias.S)
//     } .otherwise {
//       // normal
//       sExp := expIn.asSInt - bias.S
//     }

//     val sig = Wire(UInt((sigWidth + 1).W))
//     val normDist = countLeadingZeros(fractIn)
//     val subnormFract = (fractIn << normDist) (sigWidth - 3, 0) << 1
//     sig = 0.U(1.W) ## !isZero ## Mux(isZeroExpIn, subnormFract, fractIn)
    
//     val sExpOut = sExp + biasOut.S
//     val sigOut = sigIn << (sigWidthOut - sigWidth)

//     val out = Wire(new RawFloat(5, 11))
//     out.sign   := sign
//     out.isNaN  := false.B // E4M3 has no NaN
//     out.isInf  := isNaN
//     out.isZero := isZero
//     out.sExp   := sExpOut
//     out.sig    := sigOut

//     out
//   }
// }
