package srt

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import Configuration._
import srt.domain._
import srt.domain.Vector
import srt.domain.Scene

object Configuration {
  val WIDTH = 800
  val HEIGHT = 800
  val DIFFUSE_COEFFICIENT = 0.9
  val AMBIENT_COEFFICIENT = 0.1
  val TRACING_DEPTH = 5
  val SPECULAR_COEFFICIENT_POWER = 50
}

object Main {
  val scene = {
    val cameraPosition = Vector(500, HEIGHT * 2.3, -3000)
    Scene(
      Camera(
        position = cameraPosition,
        direction = (Vector(500, 0, 1200) - cameraPosition).normalize,
        up = Vector(0, 1, 0).normalize,
        planeDistance = 1800
      ),
      List(
        Plane(Vector(0, 1, 0), 0, Textured(Texture.load("textures/chessboard.png"), 3, 3, 0.25, 0.7)),
        Sphere(Vector(500, 200, 400), 200, Solid(Color.white, 0.9, 0.7))
      ),
      (for (x <- 0.to(1000, 200)) yield Light(Vector(x, HEIGHT * 1.5, 400), Color.white)).toList :+
      Light(cameraPosition, Color.white)
    )
  }

  def main(args: Array[String]) {
    args.toList match {
      case output :: Nil =>
        val progress = Some(new ConsoleProgress(HEIGHT * WIDTH))
        Image.write(scene.render(progress), new File(s"$output.png"))
      case _ => sys exit 1
    }
  }
}

object Image {
  def write(data: Seq[Seq[Color]], file: File) {
    val img = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB)
    for {
      x <- 0 until WIDTH
      y <- 0 until HEIGHT
    } img.setRGB(x, y, data(x)(y).toInt)
    ImageIO.write(img, "png", file)
  }
  def read(file: File) = {
    val img = ImageIO.read(file)
    for (y <- 0 until img.getHeight) yield for (x <- 0 until img.getWidth) yield Color(img.getRGB(x, y))
  }
}

trait Progress {
  val max: Int
  def output(percent: Int)

  var state = 0
  val step = max / 100
  def advance(by: Int = 1) = {
    state = state + by
    if (state % step == 0) output(state / step)
  }
}

class ConsoleProgress(override val max:Int) extends Progress {
  def output(percent: Int) = {
    print(s"\r\033[Oprogress : $percent %")
    if (percent == 100) println()
  }
}
