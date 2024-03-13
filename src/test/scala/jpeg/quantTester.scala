package jpeg

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

// class QuantizationTest extends AnyFlatSpec with ChiselScalatestTester {
//     def doQuantizationTest(data: Seq[Seq[Int]], qt: Int): Unit = {
//         val p = JpegParams(8, 8, qt)
//         val quantTable = p.getQuantTable
//         test(new Quantization(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//             dut.io.in.valid.poke(true.B)
//             dut.io.in.ready.expect(true.B)
//             dut.io.state.expect(QuantState.idle)

//             for (r <- 0 until p.numRows) {
//                 for (c <- 0 until p.numCols) {
//                     dut.io.in.bits.data(r)(c).poke(data(r)(c).S)
//                 }
//             } 

//             for (r <- 0 until p.numRows) {
//                 for (c <- 0 until p.numCols) {
//                     dut.io.in.bits.quantTable(r)(c).poke(quantTable(r)(c).S)
//                 }
//             }
//             dut.clock.step()
//             dut.io.in.ready.expect(false.B)
//             dut.io.out.valid.expect(false.B)
//             dut.clock.step(64)

//             val jpegEncoder = new jpegEncode(false, List.empty, 0)
//             val expected = jpegEncoder.quantization(data, quantTable)
//             dut.io.state.expect(QuantState.idle)

//             /* 
//                 For Testing purposes, prints out both the expected and actual results
//              */
//             // println("scala expected:")
//             // val expectedArray: Seq[Seq[Int]] = expected
//             // for {
//             // row <- expectedArray
//             // } {
//             // val rowString = row.mkString("\t")
//             // println(rowString)
//             // }

//             // println("Chisel actual:")
//             // val bitsArray: Vec[Vec[SInt]] = dut.io.out.bits
//             // for {
//             // row <- bitsArray
//             // } {
//             // val rowString = row.map(_.peek()).mkString("\t")
//             // println(rowString)
//             // }


//             for (r <- 0 until p.numRows) {
//                 for (c <- 0 until p.numCols) {
//                     dut.io.out.bits(r)(c).expect(expected(r)(c).S)
//                 }
//             }
//             dut.io.state.expect(QuantState.idle)
//         }
//     }

//     behavior of "Quantization"
    
//     it should "correctly quantize in1 with qt1" in {
//         val data = jpeg.QuantizationData.in1
//         val qtChoice = 1
//         doQuantizationTest(data, qtChoice)
//     }

//     it should "correctly quantize in1 with qt2" in {
//         val data = jpeg.QuantizationData.in1
//         val qtChoice = 2
//         doQuantizationTest(data, qtChoice)
//     }

//     it should "correctly quantize in2 with qt1" in {
//         val data = jpeg.QuantizationData.in2
//         val qtChoice = 1
//         doQuantizationTest(data, qtChoice)
//     }

//     it should "correctly quantize in2 with qt2" in {
//         val data = jpeg.QuantizationData.in2
//         val qtChoice = 2
//         doQuantizationTest(data, qtChoice)
//     }

//     it should "correctly quantize in3 with qt1" in {
//         val data = jpeg.QuantizationData.in3
//         val qtChoice = 1
//         doQuantizationTest(data, qtChoice)
//     }

//     it should "correctly quantize in3 with qt2" in {
//         val data = jpeg.QuantizationData.in3
//         val qtChoice = 2
//         doQuantizationTest(data, qtChoice)
//     }
// }

// class QuantizationDecodeTest extends AnyFlatSpec with ChiselScalatestTester {
//     def doQuantizationDecodeTest(data: Seq[Seq[Int]], qt: Int): Unit = {
//         val p = JpegParams(8, 8, qt)
//         val quantTable = p.getQuantTable
//         test(new QuantizationDecode(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//             dut.io.in.valid.poke(true.B)
//             dut.io.in.ready.expect(true.B)
//             dut.io.state.expect(QuantState.idle)

//             for (r <- 0 until p.numRows) {
//                 for (c <- 0 until p.numCols) {
//                     dut.io.in.bits.data(r)(c).poke(data(r)(c).S)
//                 }
//             } 

//             for (r <- 0 until p.numRows) {
//                 for (c <- 0 until p.numCols) {
//                     dut.io.in.bits.quantTable(r)(c).poke(quantTable(r)(c).S)
//                 }
//             }
//             dut.clock.step()
//             dut.io.in.ready.expect(false.B)
//             dut.io.out.valid.expect(false.B)
//             dut.clock.step(64)

//             val jpegEncoder = new jpegEncode(false, List.empty, 0)
//             val expected = jpegEncoder.quantizationDecode(data, quantTable)
//             dut.io.state.expect(QuantState.idle)

//             /* 
//                 For Testing purposes, prints out both the expected and actual results
//              */
//             // println("scala expected:")
//             // val expectedArray: Seq[Seq[Int]] = expected
//             // for {
//             // row <- expectedArray
//             // } {
//             // val rowString = row.mkString("\t")
//             // println(rowString)
//             // }

//             // println("Chisel actual:")
//             // val bitsArray: Vec[Vec[SInt]] = dut.io.out.bits
//             // for {
//             // row <- bitsArray
//             // } {
//             // val rowString = row.map(_.peek()).mkString("\t")
//             // println(rowString)
//             // }


//             for (r <- 0 until p.numRows) {
//                 for (c <- 0 until p.numCols) {
//                     dut.io.out.bits(r)(c).expect(expected(r)(c).S)
//                 }
//             }
//             dut.io.state.expect(QuantState.idle)
//         }
//     }

//     behavior of "QuantizationDecode"

//     it should "correctly undo quantize in1 with qt1" in {
//         val data = jpeg.QuantizationDecodeData.in1
//         val qtChoice = 1
//         doQuantizationDecodeTest(data, qtChoice)
//     }

//     it should "correctly undo quantize in1 with qt2" in {
//         val data = jpeg.QuantizationDecodeData.in1
//         val qtChoice = 2
//         doQuantizationDecodeTest(data, qtChoice)
//     }

//     it should "correctly undo quantize in2 with qt1" in {
//         val data = jpeg.QuantizationDecodeData.in2
//         val qtChoice = 1
//         doQuantizationDecodeTest(data, qtChoice)
//     }

//     it should "correctly undo quantize in2 with qt2" in {
//         val data = jpeg.QuantizationDecodeData.in2
//         val qtChoice = 2
//         doQuantizationDecodeTest(data, qtChoice)
//     }

//     it should "correctly undo quantize in3 with qt1" in {
//         val data = jpeg.QuantizationDecodeData.in3
//         val qtChoice = 1
//         doQuantizationDecodeTest(data, qtChoice)
//     }

//     it should "correctly quantize in3 with qt2" in {
//         val data = jpeg.QuantizationDecodeData.in3
//         val qtChoice = 1
//         doQuantizationDecodeTest(data, qtChoice)
//     }
// }

