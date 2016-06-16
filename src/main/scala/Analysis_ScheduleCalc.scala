package FFT
import ChiselDSP._

object ScheduleCalc {
  def apply(radPow:List[List[Int]],radOrder:List[List[Int]],maxStages:Int) {
    val stages = MemoryAccess.Stages_PrevStages(radPow,radOrder,maxStages).map(_.unzip._1)
    val clks = stages.map(stageRad => {
        val usedStages = stageRad.filter(_ != 0)
        val fftn = usedStages.product
        // #Stages * Pipelining_Between_Mem_access + Sum (N/Radix_currentStage/#BFs)
        // Note that pipelining = PE butterfly pipelines + memory read out register delay
        val cycles = usedStages.foldLeft(0)((accum,rad) => accum + fftn/rad)
        val rad2cycles = usedStages.foldLeft(0)((accum,rad) => {
          // Make hack (do 2 rad-2 in parallel) easier by only optimizing when the # of radix 2 BFs is even
          // Also need rad 4 or higher b/c otherwise there's no bank 3
          // TODO: Get rid of rad5 condition
          val newRad = if (rad == 2 && usedStages.contains(4) && !usedStages.contains(5)){rad * 2} else rad
          accum + fftn/newRad
        })
        val pipeDelay = Params.getDelays.pe + Params.getDelays.memReadAtoD
        val totalCycles = rad2cycles + usedStages.length * pipeDelay
        val maxAllowedCycles = Params.getIO.clkRatio*fftn
        if (totalCycles > maxAllowedCycles)
          Error("FFT  = " + fftn + " output will be incorrect with current IO/Calculation clock ratio. Requires " +
          totalCycles + " cycles.")
        if ((maxAllowedCycles - rad2cycles) < pipeDelay)
          Warn("FFT  = " + fftn + " uses up at least " + Params.getIO.clkRatio + "N - stall delay cycles ")
        // Returns values assuming 0 pipeline delay
        List(fftn,cycles,rad2cycles)
      })
    // TODO: Generic analysis folder needed
    try{
      Data2D2File(clks,"build/analysis/cycleCount.csv")
    }  
  }
}