/*

package dspblocks.fft

import chisel3._
import chisel3.experimental._
import dspblocks.numbers._
import dspblocks.numbers.implicits._

case class CordicParams(
    k: Int,
    l: Int,
    R: Double) {

}




// TODO: Make parameters more case class-y
class CORDIC[T <: Data:RealBits]() extends Module {

}

object CordicConstants
{
  val ROTATION = Bool(true)
  val VECTORING = Bool(false)
  val D_POS = Bool(true)
  val D_NEG = Bool(false)
}

// Chisel Verilog generator values 
// Following code equivalent to setting constants/generics for the Verilog that's generated
// Parameters for generated implementation
// k = bitwidth of X, Y vectors
// l = bitwidth of Z (angle)
// R = unfolding factor

case class CordicParams(k: Int, l: Int, R: Double) {

  // Set number of total iterations = max (k, l) as determined by # of bits of X/Y, Z
  val iterations = max(k,l)

  // Set number of stages = Ceiling [# iterations * R] 
  // R = 0 -> no unfolding (1 stage) & R = 1 -> complete unfolding (iterations # of stages)
  val stages = if (R>0) (ceil(iterations*R).toInt) else (1)

  // Iterations per stage (whole number > 0; round down)
  val local_iterations = floor(iterations/stages).toInt
  
  // Leftover iterations 
  // want to distribute iterations as evenly as possible (difference of 1 max) 
  val leftover_iterations = iterations % stages

}

// -- Define functionality of actual CORDIC modules --

// INPUT/OUTPUT Coordinates and rotation mode data
// Takes X,Y coordinates as signed integers (2's complement) with bitwidth k
// Takes Z angle as signed integer with bitwidth l (modulo 2Pi)
// Mode = either ROTATION or VECTORING determines if D_POS or D_NEG based off of Z/Y sign

class CordicData(implicit params: CordicParams) extends Bundle() {
  val x = SInt(width = params.k)
  val y = SInt(width = params.k)
  val z = SInt(width = params.l)
  val mode = Bool()
  override def clone: this.type = { new CordicData().asInstanceOf[this.type]; }
}

// IO consists of coordinates, mode, and valid bit
// Both global CORDIC module and stages are passed their own 
// x, y, z, mode, valid (in and out versions)

class CordicIo(implicit params: CordicParams)  extends Bundle() {

  // Add valid bit (in.valid) to CordicData, access with in.bits.x, etc.
  // data from previous block (input x, y, z, mode, valid)
  val in = new ValidIO(new CordicData()).flip

  // data from this block (output x, y, z, mode, valid)
  val out = new ValidIO(new CordicData())
}

// A single rotation stage that starts when ready (valid = true) and input is valid, then after
// a number of iterations produces valid output.
// offset is the starting iteration (rotation number - 1) to indicate shift/angle constant value 
// First rotation handles x0, y0, z0, arctan[1]                                 
// Iterations is the number of local stage iterations to perform until valid output
// Each stage should contain HW to do one rotation per cycle; would have to register values if 
// each stage needs to handle multiple rotations
// For example, CordicStage(offset=2, iterations=3) should generate the HW to do the following
// (timing varies with register placement):               
// - In the same cycle (1st) the input is valid, perform the rotation of bitshift 2 (14.036 degrees)
// - In the next cycle (2nd), perform the rotation of bitshift 3 (7.125 degrees) on the previous result
// - In the next cycle (3rd), perform the rotation of bitshift 4 (3.576 degrees) on the previous result
// - In the next cycle (4th), output should be valid (so the next stage can perform rotation of bitshift 5)
//   and computation will start again if the input is valid

class CordicStage(offset: Int, iterations: Int)(implicit params: CordicParams) extends Module {
  val io = new CordicIo()
  
  // x(i+1) = x(i)-y(i)*d(i)*2^(-i)
  // y(i+1) = y(i)+x(i)*d(i)*2^(-i)
  // z(i+1) = z(i)-d(i)*arctan[2^(-i)] where arctan[...] = angle LUT
  // divide by 2^n corresponds to shifting right by n
  // For ROTATION: d(i)=-1 if z(i) < 0, +1 otherwise
  // For VECTORING: d(i)=+1 if y(i) < 0, -1 otherwise
  // Check MSB for sign

  // Count keeps track of state. If reach max count -> start at 0
  // Stay at "state" 0 until valid seen 
  val count = Reg(init= UInt(0,width = UInt(iterations-1).getWidth))
  count := Mux((count === UInt(iterations-1))||((count === UInt(0)) && !io.in.valid),UInt(0),count+UInt(1))

  // Valid should only be high for 1 clk cycle
  // so only update following the last count (state)
  // --> next block sees Valid high on its count 0
  // If only 1 iteration/stage, just register input valid as output 
  val valid = Reg(init=Bool(false))
  if (iterations > 1) {
    valid := Mux(count === UInt(iterations-1),Bool(true),Bool(false))
  }
  else {
    valid := io.in.valid
  }
  io.out.valid := valid

  // Register mode on first count
  // Mode needs to be registered for pipelining in case
  // stage isn't done processing previous data/mode when a new mode comes in
  val mode = Reg(init=CordicConstants.VECTORING)
  when (count === UInt(0)) {
    mode := io.in.bits.mode
  }
  io.out.bits.mode := mode    

  // When performing calculations, current __ is = input __ during first count
  // but is = the registered value in the block during all subsequent counts
  // since the values are always updated on the next clk cycle after 
  // a condition is satisfied
                            
  val currentMode = Mux(count===UInt(0),io.in.bits.mode,mode)

  val x = Reg(init=SInt(0,width=params.k))
  val y = Reg(init=SInt(0,width=params.k))
  val z = Reg(init=SInt(0,width=params.l))
  
  val currentX = Mux(count===UInt(0),io.in.bits.x,x)
  val currentY = Mux(count===UInt(0),io.in.bits.y,y)
  val currentZ = Mux(count===UInt(0),io.in.bits.z,z)

  
  // Need to offset which shift is being done depending on which iteration the stage
  // is currently processing
  val y2i = currentY >> (UInt(offset)+count)
  val x2i = currentX >> (UInt(offset)+count)
  
  // Make an angle LUT to map count to arctan value (mod 2pi)
  // Note that the + offset is taken care of directly in the mapping, rather than by logic  
  val angles = (0 until iterations).map(i => SInt((atan(pow(2,-(i+offset)))/(2*Pi)*pow(2,params.l)).toInt))
  val angleLUT = Vec(angles)
  val angleLUTout = angleLUT(count)

  // Determines to check the MSB of Z or Y when in ROTATION/VECTORING mode to determine sign
  val dsel = ((currentMode===CordicConstants.ROTATION) && !(currentZ(params.l-1)))||((currentMode===CordicConstants.VECTORING) && (currentY(params.k-1)))

  // Chooses - second term or + second term, based off of dsel (rather than multiplying by -1)
  // The negatives in +/-d are "grouped together"
  val dy2i = Mux(dsel,-y2i,y2i)
  val dx2i = Mux(dsel,x2i,-x2i)
  val dangle = Mux(dsel,-angleLUTout,angleLUTout)

  // feeds the computation to output (registered)
  y := currentY+dx2i
  x := currentX+dy2i
  z := currentZ+dangle
  
  io.out.bits.x := x
  io.out.bits.y := y
  io.out.bits.z := z

}

// CORDIC wrapper - passes in data for CordicStage(s) to process
// Each CORDIC computation should take 'params.iterations' cycles

class Cordic(paramsIn: CordicParams) extends Module {
  implicit val params = paramsIn
  val io = new CordicIo()

  // For 16-bit Z, Pi/6 rad = Pi/6/(2*Pi)*2^16                                       
  // In rotation mode, if Pi/2<Z<3Pi/2 (in 0 to 2Pi scheme), need additional rotation by 180deg for Cordic to work
  // In vectoring mode, if X<0 (quadrant 2/3), need additional rotation by 180deg for Cordic to work, 
  // since you're rotating back to positive X axis
  // Cordic can do max rotation of +/-Pi/2
  // x' = -x
  // y' = -y
  // z' = z-Pi
  // Correction done without registers
  // If don't need extra rotation, just pass input to output

  // Each stage must have the mode input, since while it's processing one input, a new input with 
  // a different mode could be input into the overall CORDIC block

  val x = SInt(width = params.k)
  val y = SInt(width = params.k)
  val z = SInt(width = params.l)

  val selPreRotate = (UInt(io.in.bits.z)>UInt(pow(2,params.l-2).toInt))&&(UInt(io.in.bits.z)<UInt(3*pow(2,params.l-2).toInt)&&(io.in.bits.mode===CordicConstants.ROTATION))||((io.in.bits.mode===CordicConstants.VECTORING)&&(io.in.bits.x(params.k-1)))

  // Note that for -Pi, overflow is OK, b/c -Pi and +Pi take you to the same place
  when (selPreRotate){
    x := -io.in.bits.x
    y := -io.in.bits.y
    z := io.in.bits.z-SInt(pow(2,params.l-1).toInt)
  }
  .otherwise {
    x := io.in.bits.x
    y := io.in.bits.y
    z := io.in.bits.z
  }

  val iterations = Array.fill(params.stages)(0.toInt)
  val offsets = Array.fill(params.stages)(0.toInt)

  // distribute "leftover iterations" properly when global iterations 
  // doesn't divide # of stages evenly (extras given to fron stages)
  // each stage starts at offset based off of how many iterations have been previously done
  // Initial stage has offset 0 from default value
  for (i <- 0 until params.stages) {
    offsets(i) = iterations.sum
    if (i < params.leftover_iterations) {
      iterations(i) = (params.local_iterations+1)
    }
    else {
      iterations(i) = (params.local_iterations)
    }

    print ("stage ");
    print (i);
    print(" iteration ");
    print((iterations(i)));
    print(" offset ");
    print((offsets(i)));
    print ("\n");
  }

  // each stage doesn't have same iterations, offset value
  val stages = (0 until params.stages).map(i => Module(new CordicStage(offset = offsets(i), iterations=iterations(i))))

  // assign first stage global inputs
  stages(0).io.in.bits.x := x
  stages(0).io.in.bits.y := y
  stages(0).io.in.bits.z := z
  stages(0).io.in.bits.mode := io.in.bits.mode
  stages(0).io.in.valid := io.in.valid

  // assign outputs of current stage to inputs of next stage
  for (i <- 0 until (params.stages-1)) {
    // Wire up ports of the CORDIC stages
    stages(i).io.out <> stages(i+1).io.in
  }

  // assign last stage outputs to global outputs
  stages(params.stages-1).io.out <> io.out
  
}

// Debug: make test | tee log | grep "FAILURE"
// The tester applied an input and checks the output "iterations" cycles later.


class CordicTester(c: Cordic) extends Tester(c) {
    
  // Amount of error accepted without failing test case (Modify if needed)
  val error = 8

  //---------- TEST HELPER FUNCTIONS ----------//

  // Gain depends on number of iterations
  val An = (0 to c.params.iterations).map(i => sqrt(1+pow(2,-2*i))).reduceLeft(_ * _)


  // Convert double to fixed-point with f fractional bits
  def doubleToBigInt(in: Double, f: Int): BigInt = { (in*pow(2,f)).toInt }

  // Convert radians to modulo 2*pi with w bits
  def radiansToBigInt(in: Double, w: Int): BigInt = { ((in/(2*Pi))*pow(2,w)).toInt }

  // Generate bits for a test vector that will be applied to input and
  // expected at output.
  // 'f' is number of fractional bits in fixed-point representation
  def testData(x: Double, y: Double, f: Int, z: Double, mode: Bool): Array[BigInt] = {
      Array(doubleToBigInt(x, f), doubleToBigInt(y, f), radiansToBigInt(z, c.params.l),
      mode.litValue())
  }

  // Tester expects unsigned BigInt, so convert all signed to unsigned.
  def testDataUnsigned(data: Array[BigInt], bw: Array[Int]): Array[BigInt] = {
      (data, bw).zipped.map((data: BigInt, bw: Int) =>
      data & ((1<<bw)-1)) }

  // Want results as signed BigInt, so convert all unsigned to signed.
  def testDataSigned(data: Array[BigInt], bw: Array[Int]): Array[BigInt] = {
      (data, bw).zipped.map((data: BigInt, bw: Int) =>
      if(data >= (1<<(bw-1))) (data-(1<<bw)) else data) }

  // Check output bits against expected data
  def checkTestData(result: Array[BigInt], expected: Array[BigInt]): Boolean = {
    var isClose = true
    for(i <- 0 until expected.length) {
      val expLow = expected(i) - error
      val expHigh = expected(i) + error
      val msg = "CHECK " + i + " <- " + expLow + " <= " + result(i) + " <= " + expHigh
      if((result(i) >= expLow) && (result(i) <= expHigh))
      {
        println(msg + " PASS");
      } else {
        println(msg + " FAIL");
        isClose = false
      }
    }
    isClose
  }
  
  //---------- GENERATE TEST VECTORS ----------//

  // Polar (r, angle) to rectangular (X, Y):
  // x=r/An, y=0, z=angle -> x=X, y=Y, z=0
  //val rangeRotation = Range(-90,90,30)
  val rangeRotation = Range(-180,180,1)
  val testsRotation = for(i <- rangeRotation) yield {
    (("Testing rotation with r=1, angle=" + i + " degrees"),
     testData(1/An, 0, c.params.k-3, toRadians(i), ROTATION),
     testData(cos(toRadians(i)), sin(toRadians(i)), c.params.k-3, 0, ROTATION))
  }

  // Rectangular (X, Y) to polar (r, angle):
  // x=X/An, y=Y/An, z=0 -> x=r, y=0, z=angle
  //val rangeVectoring = Range(-90,90,30)
  val rangeVectoring = Range(-180,180,1)
  val testsVectoring = for(i <- rangeVectoring) yield {
    (("Testing vectoring with x=cos(" + i + "), y=sin(" + i + ")"),
     testData(cos(toRadians(i))/An, sin(toRadians(i))/An, c.params.k-3, 0, VECTORING),
     testData(1, 0, c.params.k-3, toRadians(i), VECTORING))
  }
  
  val tests = testsRotation ++ testsVectoring
  
  //---------- RUN TESTS ----------//

  // Bitwidths for test values
  val bw = Array(c.params.k, c.params.k, c.params.l, 1)

  // Iterate over tests
  for(test <- tests) {
    println(test._1)
    // Set input data
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits, testDataUnsigned(test._2, bw))

    // Step forward 'iterations' cycles
    step(1)
    poke(c.io.in.valid, 0)
    for(i <- 0 until c.params.iterations) { step(1) }

    // Check output data
    expect(c.io.out.valid, 1)
    ok &&= checkTestData(testDataSigned(peek(c.io.out.bits), bw),
    testDataSigned(test._3, bw))

    // Clear outputs
    reset()
  }
}

}


*/