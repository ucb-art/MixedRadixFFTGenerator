package FFT
import ChiselDSP._

object ScheduleCalc {
	def apply(radPow:List[List[Int]],radOrder:List[List[Int]],maxStages:Int) {
		val stages = MemoryAccess.Stages_PrevStages(radPow,radOrder,maxStages).map(_.unzip._1)
		val clks = stages.map(stageRad => {
				val usedStages = stageRad.filter(_ != 0)
				val fftn = usedStages.product
				// #Stages * Pipelining_Between_Mem_access + Sum (N/Radix_currentStage/ #BFs)
				// Note that pipelining = butterfly pipelines + sequential memory read delay
				val cycles = usedStages.foldLeft(0)((accum,rad) => accum + fftn/rad)
				val rad2cycles = usedStages.foldLeft(0)((accum,rad) => {
					// Make hack (do 2 rad-2 in parallel) easier by only optimizing when the # of radix 2 BFs is even
					// Also need rad 4 or higher b/c otherwise there's no bank 3
					val newRad = if (rad == 2 && usedStages.contains(4)){rad * 2} else rad
					accum + fftn/newRad
				})
				List(fftn,cycles,rad2cycles)
			})
		val cycleCount = clks.map( n => n.mkString(",")).mkString("\n")
    scala.tools.nsc.io.File("cycleCount.csv").writeAll(cycleCount)
    println(cycleCount)
	}
}