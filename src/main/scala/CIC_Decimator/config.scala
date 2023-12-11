// See LICENSE_AALTO.txt for license details

package cic_decimator.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

case class CicConfig(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  order:              Int,
  gainBits:           Int
)

object CicConfig {
  implicit val CicConfigFormat = yamlFormat4(CicConfig.apply)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class CicConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new CicConfigParseException(msg) }

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

  def loadFromFile(filename: String): Either[CicConfig, Error] = {
    println(s"\nLoading cic configuration from file: $filename")
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

    // Parse CicConfig from YAML AST
    val cic_config = yamlAst.convertTo[CicConfig]

    val config = new CicConfig(cic_config.syntax_version, cic_config.resolution, cic_config.order, cic_config.gainBits)

    println("resolution:")
    println(config.resolution)

    println("order:")
    println(config.order)

    println("gainBits:")
    println(config.gainBits)

    Left(config)
  }
}
