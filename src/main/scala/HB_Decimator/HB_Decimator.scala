// Finitie impulse filter
package hb_decimator
import config._
import config.{HbConfig}

import java.io.File

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import chisel3.stage.ChiselGeneratorAnnotation

import dsptools._
import dsptools.numbers.DspComplex

class HB_DecimatorIO(resolution: Int, gainBits: Int) extends Bundle {
  val in = new Bundle {
    val clock_low = Input(Clock())
    val scale  = Input(UInt(gainBits.W))
    val iptr_A = Input(DspComplex(SInt(resolution.W), SInt(resolution.W)))
  }
  val out = new Bundle {
    val Z = Output(DspComplex(SInt(resolution.W), SInt(resolution.W)))
  }
}

class HB_Decimator(config: HbConfig) extends Module {
    val io = IO(new HB_DecimatorIO(resolution=config.resolution, gainBits=config.gainBits))
    val data_reso = config.resolution
    val calc_reso = config.resolution * 2

    val inregs = RegInit(VecInit(Seq.fill(2)(DspComplex.wire(0.S(data_reso.W), 0.S(data_reso.W))))) //registers for sampling rate reduction
    
    inregs.foldLeft(io.in.iptr_A) {(prev, next) => next := prev; next} //The last "next" is the return value that becomes the prev

    //The half clock rate domain
    withClock (io.in.clock_low){
        val slowregs  = RegInit(VecInit(Seq.fill(2)(DspComplex.wire(0.S(data_reso.W), 0.S(data_reso.W))))) //registers for sampling rate reduction
        (slowregs, inregs).zipped.map(_ := _)

        val sub1coeffs = config.H.indices.filter(_ % 2 == 0).map(config.H(_)) //Even coeffs for Fir1
        println(sub1coeffs)

        val tapped1 = sub1coeffs.reverse.map(tap => slowregs(0) * tap)
        val registerchain1 = RegInit(VecInit(Seq.fill(tapped1.length + 1)(DspComplex.wire(0.S(calc_reso.W), 0.S(calc_reso.W)))))

        for ( i <- 0 until tapped1.length) {
            if (i == 0) {
                registerchain1(i + 1) := tapped1(i)
            } else {
                registerchain1(i + 1) := DspComplex.wire(registerchain1(i).real + tapped1(i).real, registerchain1(i).imag + tapped1(i).imag)
            }
        }

        val subfil1 = registerchain1(tapped1.length)
        
        val sub2coeffs=config.H.indices.filter(_ % 2 == 1).map(config.H(_)) //Odd coeffs for Fir 2
        println(sub2coeffs)

        val tapped2 = sub2coeffs.reverse.map(tap => slowregs(1) * tap)
        val registerchain2 = RegInit(VecInit(Seq.fill(tapped2.length + 1)(DspComplex.wire(0.S(calc_reso.W), 0.S(calc_reso.W)))))

        for ( i <- 0 until tapped2.length) {
            if (i == 0) {
                registerchain2(i + 1) := tapped2(i)
            } else {
                registerchain2(i + 1) := DspComplex.wire(registerchain2(i).real + tapped2(i).real, registerchain2(i).imag + tapped2(i).imag)
            }
        }

        val subfil2 = registerchain2(tapped2.length)
        
        val outreg = RegInit(DspComplex.wire(0.S(data_reso.W), 0.S(data_reso.W)))

        outreg.real := ((subfil1.real + subfil2.real) * io.in.scale)(calc_reso - 1, calc_reso - data_reso).asSInt
        outreg.imag := ((subfil1.imag + subfil2.imag) * io.in.scale)(calc_reso - 1, calc_reso - data_reso).asSInt

        io.out.Z := outreg
    }
}



/** Generates verilog or sv*/
object HB_Decimator extends App with OptionParser {
  // Parse command-line arguments
  val (options, arguments) = getopts(default_opts, args.toList)
  printopts(options, arguments)

  val config_file = options("config_file")
  val target_dir = options("td")
  var hb_config: Option[HbConfig] = None
  HbConfig.loadFromFile(config_file) match {
    case Left(config) => {
      hb_config = Some(config)
    }
    case Right(err) => {
      System.err.println(s"\nCould not load FIR configuration from file:\n${err.msg}")
      System.exit(-1)
    }
  }

  // Generate verilog
  val annos = Seq(ChiselGeneratorAnnotation(() => new HB_Decimator(config=hb_config.get)))
  //(new ChiselStage).execute(arguments.toArray, annos)
  val sysverilog = (new ChiselStage).emitSystemVerilog(
    new HB_Decimator(config=hb_config.get),
     
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
    "config_file"->"fir-config.yml",
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

