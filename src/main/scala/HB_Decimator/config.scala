// See LICENSE_AALTO.txt for license details

package hb_decimator.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

/** HB parameter case class
  */
case class HbGeneric(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:	      Int,
  gainBits:           Int
)

case class HbTaps(
  H:                  Seq[Double]
)

case class HbConfig(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  H:		      Seq[Int],
  gainBits:           Int
)

object HbConfig {
  implicit val hbConfigFormat = yamlFormat3(HbGeneric)

  implicit val dHFormat = yamlFormat1(HbTaps)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class HbConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new HbConfigParseException(msg) }

    /** Abort program execution and print out the reason */
    def panic() = {
      System.err.println(msg)
      System.exit(-1)
    }
  }

  /** parse legal syntax version from config yaml AST */
  private[config] def parseSyntaxVersion(yamlAst: YamlValue): Either[BigInt,Error] = {
    // get version number as an integer
    val version: BigInt = yamlAst.asYamlObject.fields.get(YamlString("syntax_version")) match {
      case Some(version) => version match {
        case maybeDecimal: YamlNumber => maybeDecimal.asInstanceOf[YamlNumber].value.toBigIntExact match {
          case Some(integer) => integer
          case None => return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
        }
        case _ => return return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
      }
      case None => return Right(Error("Missing required top-level key: `syntax_version`."))
    }
    if (syntaxVersion != version)
      return Right(Error(s"Unsupported syntax version: $version.\n- Supported versions: $syntaxVersion"))
    Left(version)
  }

  def loadFromFile(filename: String): Either[HbConfig, Error] = {
    println(s"\nLoading hb configuration from file: $filename")
    var fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(filename)
      fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val yamlAst = fileString.parseYaml
    val syntaxVersion = parseSyntaxVersion(yamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse HbConfig from YAML AST
    val generic = yamlAst.convertTo[HbGeneric]
    val taps = yamlAst.convertTo[HbTaps]

    val config = new HbConfig(generic.syntax_version, generic.resolution, taps.H.map(_ * (math.pow(2, generic.resolution - 1) / 2 - 1)).map(_.toInt), generic.gainBits)

    println("resolution:")
    println(config.resolution)

    println("taps:")
    println(config.H)

    println("gainBits:")
    println(config.gainBits)

    Left(config)
  }
}
