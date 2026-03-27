package synthesis

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

trait VerboseLogSink {
  def log(message: String): Unit
}

object VerboseLogSink {
  object NoOp extends VerboseLogSink {
    override def log(message: String): Unit = ()
  }

  final class FileSink(path: java.nio.file.Path, truncate: Boolean) extends VerboseLogSink {
    private val normalizedPath = path.toAbsolutePath.normalize()
    private val parent = normalizedPath.getParent
    if (parent != null) Files.createDirectories(parent)
    if (truncate) {
      Files.write(
        normalizedPath,
        Array.emptyByteArray,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    }

    override def log(message: String): Unit = this.synchronized {
      val line = message + "\n"
      Files.write(
        normalizedPath,
        line.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    }
  }

  def file(path: String, truncate: Boolean = true): VerboseLogSink =
    new FileSink(Paths.get(path), truncate)
}

object DebugLogger {
  private val logPath = Paths.get("/home/youch/tolin/.cursor/debug-2c8067.log")
  private val sessionId = "2c8067"

  private def esc(s: String): String =
    Option(s).getOrElse("").flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    }

  def log(location: String,
          message: String,
          dataJson: String,
          runId: String,
          hypothesisId: String): Unit = {
    try {
      val dir = logPath.getParent
      if (dir != null) Files.createDirectories(dir)
      val line =
        s"""{"sessionId":"${esc(sessionId)}","runId":"${esc(runId)}","hypothesisId":"${esc(hypothesisId)}","location":"${esc(location)}","message":"${esc(message)}","data":$dataJson,"timestamp":${System.currentTimeMillis()}}""" + "\n"
      Files.write(
        logPath,
        line.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.APPEND
      )
    } catch {
      case _: Throwable => ()
    }
  }
}
