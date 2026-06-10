package xs.utils.stage

import chisel3.stage.phases.Convert
import circt.stage.ChiselStage
import firrtl.AnnotationSeq
import firrtl.options.{Dependency, Phase, PhaseManager}
import firrtl.ir.{Block, Connect, DefModule, IntWidth, NoInfo, Reference, SubField, UIntLiteral}
import firrtl.stage.FirrtlCircuitAnnotation

class InitializeHwaPorts extends Phase {
  override def prerequisites = Seq.empty
  override def optionalPrerequisites: Seq[Dependency[Phase]] = Seq(Dependency[Convert])
  override def optionalPrerequisiteOf = Seq.empty
  override def invalidates(a: Phase) = false

  private val pattern = ".*hwa_[0-9_]*".r

  private def doDefModules(in: Seq[DefModule]): Seq[DefModule] = {
    in.map({
      case mm@firrtl.ir.Module(_, _, _, _, ports, body) =>
        val hwaPorts = ports.filter(p => pattern.matches(p.name)).map(_.name)
        if(hwaPorts.isEmpty) {
          mm
        } else {
          val hwaStms = hwaPorts.map(h => Connect(NoInfo, SubField(Reference(h), "cond"), UIntLiteral(0, IntWidth(1))))
          val newBody = Block(hwaStms ++ body.asInstanceOf[firrtl.ir.Block].stmts)
          mm.copy(body = newBody)
        }
      case x => x
    })
  }

  private def doFirrtlCircuitAnnotation(in: FirrtlCircuitAnnotation): FirrtlCircuitAnnotation = {
    val mods = doDefModules(in.circuit.modules)
    FirrtlCircuitAnnotation(in.circuit.copy(modules = mods))
  }

  def transform(annotations: AnnotationSeq): AnnotationSeq = {
    val as = annotations.map {
      case a: FirrtlCircuitAnnotation => doFirrtlCircuitAnnotation(a)
      case x => x
    }
    AnnotationSeq(as)
  }
}

class XsStage extends ChiselStage {
  override def run(annotations: AnnotationSeq): AnnotationSeq = {
    val pm = new PhaseManager(
      targets = Seq(
        Dependency[chisel3.stage.phases.AddImplicitOutputFile],
        Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
        Dependency[chisel3.stage.phases.AddSerializationAnnotations],
        Dependency[chisel3.stage.phases.Convert],
        Dependency[xs.utils.stage.InitializeHwaPorts],
        Dependency[chisel3.stage.phases.AddDedupGroupAnnotations],
        Dependency[circt.stage.phases.AddImplicitOutputFile],
        Dependency[circt.stage.phases.CIRCT]
      ),
      currentState = Seq(
        Dependency[firrtl.stage.phases.AddDefaults],
        Dependency[firrtl.stage.phases.Checks]
      )
    )
    pm.transform(annotations)
  }
}
