package jpeg

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

object ZigZagParseData {
    val in2x2 = Seq(Seq(1,2),
                    Seq(3,4))

    val in3x3 = Seq(Seq(1,2,6),
                    Seq(3,5,7),
                    Seq(4,8,9))
    
    val in4x4 = Seq(Seq(10,11,12,13),
                    Seq(14,15,16,17),
                    Seq(18,19,20,21),
                    Seq(22,23,24,25))

    val in8x8 = Seq(Seq(10, 11, 12, 13, 14, 15, 16, 17),
                    Seq(18, 19, 20, 21, 22, 23, 24, 25),
                    Seq(26, 27, 28, 29, 30, 31, 32, 33),
                    Seq(34, 35, 36, 37, 38, 39, 40, 41),
                    Seq(42, 43, 44, 45, 46, 47, 48, 49),
                    Seq(50, 51, 52, 53, 54, 55, 56, 57),
                    Seq(58, 59, 60, 61, 62, 63, 64, 65),
                    Seq(66, 67, 68, 69, 70, 71, 72, 73))

    val out2x2 = Seq(1, 2, 3, 4)
    val out3x3 = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val out4x4 = Seq(10, 11, 14, 18, 15, 12, 13, 16, 19, 22, 23, 20, 17, 21, 24, 25)
    val out8x8 = Seq(10, 11, 18, 26, 19, 12, 13, 20, 27, 34, 42, 35, 28, 21, 14, 15, 22, 29, 36, 43, 50, 58, 51, 44, 37, 30, 23, 16, 17, 24, 31, 38, 45, 52, 59, 66, 67, 60, 53, 46, 39, 32, 25, 33, 40, 47, 54, 61, 68, 69, 62, 55, 48, 41, 49, 56, 63, 70, 71, 64, 57, 65, 72, 73)
}

object RLEData {
    val in1 = Seq(1, 1, 1, 2, 2, 3, 3, 3, 3)
    val in2 = Seq(5, 5, 5, 5, 3, 3, 1, 1, 1)
    val in3 = Seq(4, 4, 4, 4, 4)
    val in4 = Seq(1, 2, 3, 4 ,5)
    
    val out1 = Seq(3, 1, 2, 2, 4, 3)
    val out2 = Seq(4, 5, 2, 3, 3, 1)
    val out3 = Seq(5, 4)
    val out4 = Seq(1, 1, 1, 2, 1, 3, 1, 4, 1, 5)
}

object deltaData {
    val in1 = Seq(1, 3, 6, 10)
    val in2 = Seq(10, 7, 4, 2)
    val in3 = Seq(5, 5, 5, 5)
    val in4 = Seq.empty[Int]
    val in5 = Seq(100)

    val out1 = Seq(1, 2, 3, 4)
    val out2 = Seq(10, -3, -3, -2)
    val out3 = Seq(5, 0, 0, 0)
    val out4 = Seq.empty[Int]
    val out5 = Seq(100)

}

class ZigZagParseTester extends AnyFlatSpec with ChiselScalatestTester {
    it should "Zig Zag 2x2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in2x2) == ZigZagParseData.out2x2)
    }

    it should "Zig Zag 3x3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in3x3) == ZigZagParseData.out3x3)
    }

    it should "Zig Zag 4x4" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in4x4) == ZigZagParseData.out4x4)
    }

    it should "Zig Zag 8x8" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.zigzagParse(ZigZagParseData.in8x8) == ZigZagParseData.out8x8)
    }
}

class RLETester extends AnyFlatSpec with ChiselScalatestTester {
    it should "RLE test 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in1) == RLEData.out1)
    }

    it should "RLE test 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in2) == RLEData.out2)
    }

    it should "RLE test 3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in3) == RLEData.out3)
    }

    it should "RLE test no dupes" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.RLE(RLEData.in4) == RLEData.out4)
    }

}

class deltaTester extends AnyFlatSpec with ChiselScalatestTester {
    it should "delta test 1" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in1) == deltaData.out1)
    }

    it should "delta test 2" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in2) == deltaData.out2)
    }

    it should "delta test 3" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in3) == deltaData.out3)
    }

    it should "delta test empty" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in4) == deltaData.out4)
    }

    it should "delta test single elem" in {
        val jpegEncoder = new jpegEncode(false, List.empty, 0)
        assert(jpegEncoder.delta(deltaData.in5) == deltaData.out5)
    }

}