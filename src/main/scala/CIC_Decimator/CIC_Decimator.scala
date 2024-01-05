// Finitie impulse filter
package cic_decimator
import config._
import config.{CicConfig}

import java.io.File

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.{log2Ceil}
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation

import dsptools._
import dsptools.numbers.DspComplex

class CIC_DecimatorIO(resolution: Int, gainBits: Int) extends Bundle {
  val in = new Bundle {
    val clockslow = Input(Clock())
    val integscale = Input(UInt(gainBits.W))
    val integshift = Input(UInt(log2Ceil(resolution).W))
    val iptr_A = Input(DspComplex(SInt(resolution.W), SInt(resolution.W)))
  }
  val out = new Bundle {
    val Z = Output(DspComplex(SInt(resolution.W), SInt(resolution.W)))
  }
}

class CIC_Decimator(config: CicConfig) extends Module {
    val io = IO(new CIC_DecimatorIO(resolution=config.resolution, gainBits=config.gainBits))
    val data_reso = config.resolution
    val calc_reso = config.resolution * 2

    //Integrators
    val integregs = RegInit(VecInit(Seq.fill(config.order + 1)(DspComplex.wire(0.S(calc_reso.W), 0.S(calc_reso.W)))))
    for (i<- 0 to config.order) {
      if (i <= 0) {
        integregs(i) := io.in.iptr_A
      } else {
        integregs(i).real := integregs(i - 1).real + integregs(i).real
        integregs(i).imag := integregs(i - 1).imag + integregs(i).imag
      }
    }

    withClock (io.in.clockslow){
        // Registers for sampling rate reduction
        val slowregs = RegInit(VecInit(Seq.fill(config.order + 1)(DspComplex.wire(0.S(calc_reso.W), 0.S(calc_reso.W)))))
        val minusregs = RegInit(VecInit(Seq.fill(config.order + 1)(DspComplex.wire(0.S(calc_reso.W), 0.S(calc_reso.W)))))
        for (i <- 0 to config.order) {
          if (i <= 0) {
              slowregs(i).real := integregs(config.order).real * io.in.integscale << io.in.integshift 
              slowregs(i).imag := integregs(config.order).imag * io.in.integscale << io.in.integshift
              minusregs(i) := slowregs(i)
          } else {
              slowregs(i).real := slowregs(i - 1).real - minusregs(i - 1).real
              slowregs(i).imag := slowregs(i - 1).imag - minusregs(i - 1).imag
              minusregs(i) := slowregs(i)
          }
        }
        io.out.Z.real := slowregs(config.order).real(calc_reso - 1, calc_reso - data_reso).asSInt
        io.out.Z.imag := slowregs(config.order).imag(calc_reso - 1, calc_reso - data_reso).asSInt
    }
}



/** Generates verilog or sv*/
object CIC_Decimator extends App with OptionParser {
  // Parse command-line arguments
  val (options, arguments) = getopts(default_opts, args.toList)
  printopts(options, arguments)

  val config_file = options("config_file")
  val target_dir = options("td")
  var cic_config: Option[CicConfig] = None
  CicConfig.loadFromFile(config_file) match {
    case Left(config) => {
      cic_config = Some(config)
    }
    case Right(err) => {
      System.err.println(s"\nCould not load FIR configuration from file:\n${err.msg}")
      System.exit(-1)
    }
  }

  // Generate verilog
  val annos = Seq(ChiselGeneratorAnnotation(() => new CIC_Decimator(config=cic_config.get)))
  //(new ChiselStage).execute(arguments.toArray, annos)
  val sysverilog = (new ChiselStage).emitSystemVerilog(
    new CIC_Decimator(config=cic_config.get),
     
    //args
    Array("--target-dir", target_dir))
}



/** Module-specific command-line option parser */
trait OptionParser {
  // Module specific command-line option flags
  val available_opts: List[String] = List(
      "-config_file",
      "-td"
  )

  // Default values for the command-line options
  val default_opts : Map[String, String] = Map(
    "config_file"->"cic-config.yml",
    "td"->"verilog/"
  )

  /** Recursively parse option flags from command line args
   * @param options Map of command line option names to their respective values.
   * @param arguments List of arguments to parse.
   * @return a tuple whose first element is the map of parsed options to their values 
   *         and the second element is the list of arguments that don't take any values.
   */
  def getopts(options: Map[String, String], arguments: List[String]) : (Map[String, String], List[String]) = {
    val usage = s"""
      |Usage: ${this.getClass.getName.replace("$","")} [-<option> <argument>]
      |
      | Options
      |     -config_file        [String]  : Generator YAML configuration file name. Default "fir-config.yml".
      |     -td                 [String]  : Target dir for building. Default "verilog/".
      |     -h                            : Show this help message.
      """.stripMargin

    // Parse next elements in argument list
    arguments match {
      case "-h" :: tail => {
        println(usage)
        sys.exit()
      }
      case option :: value :: tail if available_opts contains option => {
        val (newopts, newargs) = getopts(
            options ++ Map(option.replace("-","") -> value), tail
        )
        (newopts, newargs)
      }
      case argument :: tail => {
        val (newopts, newargs) = getopts(options, tail)
        (newopts, argument.toString +: newargs)
      }
      case Nil => (options, arguments)
    }
  }

  /** Print parsed options and arguments to stdout */
  def printopts(options: Map[String, String], arguments: List[String]) = {
    println("\nCommand line options:")
    options.nonEmpty match {
      case true => for ((k,v) <- options) {
        println(s"  $k = $v")
      }
      case _ => println("  None")
    }
    println("\nCommand line arguments:")
    arguments.nonEmpty match {
      case true => for (arg <- arguments) {
        println(s"  $arg")
      }
      case _ => println("  None")
    }
  }
}

