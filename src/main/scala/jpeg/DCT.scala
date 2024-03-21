package jpeg

import chisel3._
import chisel3.internal.firrtl.Width
import chisel3.util._
import java.rmi.dgc.DGC
import scala.math.round
import chisel3.experimental._
import scala.math.cos
import scala.math.Pi 

/**
  * Object for DCT
  */
object DCTChisel {
    def apply(matrixIn: Valid[Vec[Vec[SInt]]], shiftedOut: Vec[Vec[SInt]], dctOut: Valid[Vec[Vec[SInt]]]) = {
        val mod = Module(new DCTChisel)
        mod.io.in := matrixIn
        mod.io.shiftedOut := shiftedOut
        mod.io.dctOut := dctOut
        mod
    }
}

/**
  * Creates FSM states for DCT
  */
object DCTState extends ChiselEnum { 
    val loading, shifting, calculating, waiting = Value 
}

/** Performs DCT on 8x8 Matrix with scaling
  * 
  * IO
  * @param matrixIn Input matrix to perform DCT on
  * 
  * @return shiftedOut Shifted version of input matrix by -128
  * @return dctOut Resulting scaled output of DCT
  */
class DCTChisel extends Module {
    val io = IO(new Bundle {
        val in = Flipped(Valid(new Bundle {
            val matrixIn = Input(Vec(8, Vec(8, SInt(9.W))))
        }))
        val shiftedOut = Output(Vec(8, Vec(8, SInt(9.W)))) // Test output to check shiftedblock
        val dctOut = Valid(Vec(8, Vec(8, SInt(32.W))))
    })

    // Initializes registers for matrixs and Valid bit 
    val matrixInput  = Reg(Vec(8, Vec(8, SInt(9.W))))
    val shiftedBlock = Reg(Vec(8, Vec(8, SInt(9.W))))
    val matrixOutput = Reg(Vec(8, Vec(8, SInt(32.W))))
    val validOut  = RegInit(false.B)

    // Assignes outputs
    io.dctOut.valid := validOut
    io.dctOut.bits := matrixOutput
    io.shiftedOut := DontCare


    // Function to compute DCT values for each element of input matrix
    val uCount = Counter(8)
    val vCount = Counter(8)
    val iCount = Counter(8)
    val jCount = Counter(8)
    val sum = RegInit(0.S(32.W)) 
    // var sum = 0.S


    val cosTable = VecInit.tabulate(360) { i =>
        (math.cos(i * math.Pi / 180) * 100).toInt.S
    }


    // Initilizes state and defines FSM
    val state = RegInit(DCTState.waiting)
    switch(state) {
        is(DCTState.waiting) {
            when(io.in.valid) {
                matrixInput := io.in.bits.matrixIn
                state := DCTState.shifting
                validOut := false.B
            }
        }
        is(DCTState.shifting) {
            // Performs shift on input matrix to normalize it before computing DCT  
            for (i <- 0 until 8) {
                for (j <- 0 until 8) {
                    shiftedBlock(i)(j) := io.in.bits.matrixIn(i)(j) -& 128.S
                }
            }
            io.shiftedOut := shiftedBlock
            state := DCTState.calculating
            // uCount.value := 0.U
            // vCount.value := 0.U
            // iCount.value := 0.U
            // jCount.value := 0.U
        }
        is(DCTState.calculating) {
            // Assignes output matrix to calculated DCT values
            // val sum = Wire(SInt(32.W))


            val pixelValue = shiftedBlock(iCount.value)(jCount.value)
            
            val indexUI = (((2.U * iCount.value + 1.U) * uCount.value * 1125.U) / 1000.U)//.U //% 100 >= 50
            val indexVJ = (((2.U * jCount.value + 1.U) * vCount.value * 1125.U) / 1000.U)//.U

            
            val cosVal = cosTable(indexUI) * cosTable(indexVJ)//(math.cos((2 * i + 1) * u * Pi / 16) * math.cos((2 * j + 1) * v * Pi / 16) * 100).toInt.S
            // sum := sum +& pixelValue * cosVal

            when(iCount.value === 7.U && jCount.value === 7.U) {
                sum := 0.S // Initialize sum for each (u, v) iteration
                // sum = 0.S
            } .otherwise {
                sum := sum +& pixelValue * cosVal
            }

            when(uCount.value === 0.U && (vCount.value === 0.U || vCount.value === 1.U)) {
                printf("Sum: %d cos: %d, cosVJ: %d, indexVJ: %d, u: %d, v: %d, i: %d, j: %d\n", sum, cosVal, cosTable(indexVJ), indexVJ, uCount.value,vCount.value,iCount.value,jCount.value)
            }

            when(uCount.value < 7.U) {
                // v loop
                when(vCount.value === 7.U && iCount.value === 7.U && jCount.value === 7.U) {
                    // Reset v and increment u
                    vCount.value := 0.U
                    iCount.value := 0.U
                    jCount.value := 0.U
                    uCount.inc()
                }.otherwise {
                    // i loop
                    when(iCount.value === 7.U && jCount.value === 7.U) { //iCount.value < 7.U && jCount.value < 7.U
                        // Reset i and increment v
                        iCount.value := 0.U
                        jCount.value := 0.U
                        vCount.inc()
                    }.otherwise {
                        when(jCount.value === 7.U) {
                            jCount.value := 0.U
                            when(iCount.value =/= 7.U) {
                                iCount.inc()
                            }

                            
                            // printf("HEREE TEST j: %d\n\n", jCount.value)
                            
                        }.otherwise {
                        // Reset j and increment i
                            // jCount.value := 0.U
                            // iCount.inc()
                            //printf("HEREE TEST j: %d\n\n", jCount.value)
                            jCount.inc()
                        }
                    }
                }
            }


            //val alphaU = if (u == 0) (1.0 / math.sqrt(2)) * 100 else 100
            //val alphaV = if (v == 0) (1.0 / math.sqrt(2)) * 100 else 100
            val alphaU = Mux((uCount.value === 0.U), 7.S, 10.S) //if (uCount.value == 0.U) 70 else 100
            val alphaV = Mux((vCount.value === 0.U), 7.S, 10.S) //if (vCount.value == 0.U) 70 else 100
            val scaledSum = (alphaU * alphaV * sum / 4.S)
            matrixOutput(uCount.value)(vCount.value) := scaledSum


            //Print content of matrixOutput when it's in the waiting state
            when(1.U === 0.U) {
                printf("u: %d, v: %d, i: %d, j: %d\n\n", uCount.value,vCount.value,iCount.value,jCount.value)
                printf("Content of matrixOutput in calc state:\n")
                for (i <- 0 until 8) {
                    for (j <- 0 until 8) {
                        printf("%d ", matrixOutput(i)(j))
                    }
                    printf("\n")
                }
                printf("\n")
            }
            // matrixOutput := DCT(shiftedBlock)
            state := DCTState.waiting
            validOut := true.B
        }
    }
}



            // when(uCount.value < 8.U){
            //     when(vCount.value < 8.U) {
            //         sum = 0.S
            //         //var i = 0
            //         //var j = 0
            //         when(iCount.value < 8.U) {
            //             when(jCount.value < 8.U) {
            //                 // val cosVal = 1.U//(math.cos((2 * i + 1) * u * Pi / 16) * math.cos((2 * j + 1) * v * Pi / 16) * 100).toInt.S
            //                 sum = sum +& shiftedBlock(iCount.value)(jCount.value) * 1.U//cosVal
            //                 //j = j + 1
            //             }
            //             //i = i + 1
            //         }
            //         alphaU = if (uCount.value == 0.U) (1.0 / math.sqrt(2)) * 100.0 else 100
            //         alphaV = if (vCount.value == 0.U) (1.0 / math.sqrt(2)) * 100.0 else 100  
            //         // val scaledSum = (alphaU.toInt.S * alphaV.toInt.S * sum / 4.S) 
            //         matrixOutput(uCount.value)(vCount.value) := (alphaU.toInt.S * alphaV.toInt.S * sum / 4.S) 
            //         v = v + 1
            //     }
            //     u = u + 1
            // } .otherwise {
            //     // matrixOutput := DCT(shiftedBlock)
            //     state := DCTState.waiting
            //     validOut := true.B
            // }