object Elaborate extends App {
  val firtoolOptions = Array(
    "--lowering-options=" + List(
      // make yosys happy
      // see https://github.com/llvm/circt/blob/main/docs/VerilogGeneration.md
      "disallowLocalVariables",
      "disallowPackedArrays",
      "locationInfoStyle=wrapInAtSquareBracket"
    ).reduce(_ + "," + _)
  )
  circt.stage.ChiselStage.emitSystemVerilogFile(new gcd.GCD(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new alu.Alu(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new pc.ProgramCounter, args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new lsu.MemLoadStoreUnit(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new registers.RegisterFiles(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new scheduler.Scheduler(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new fetcher.Fetcher(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new decoder.Decoder(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new core.Core(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new dispatch.Dispatch(), args, firtoolOptions)
  circt.stage.ChiselStage.emitSystemVerilogFile(new dcr.DeviceControlRegister(), args, firtoolOptions)
//   circt.stage.ChiselStage.emitSystemVerilogFile(new controller.Controller(), args, firtoolOptions)
}
