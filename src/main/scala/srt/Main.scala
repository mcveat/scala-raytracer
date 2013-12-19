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
}

object Main {
  def main(args: Array[String]) {
    args.toList match {
      case output :: Nil =>
        val scene = Scene(
          Camera(Vector(WIDTH / 2, HEIGHT / 2, -4000)),
          List(
            Sphere(Vector(100, 100, 100), 50, Color.green),
            Sphere(Vector(300, 300, 300), 100, Color.blue),
            Sphere(Vector(600, 600, 600), 200, Color.red)
          ),
          Light(Vector(-200, -200, -200))
        )
        ImageWriter.write(scene.render, new File(s"$output.png"))
      case _ => sys exit 1
    }
  }
}

object ImageWriter {
  def write(data: Seq[Seq[Color]], file: File) {
    val img = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB)
    for {
      x <- 0 until WIDTH
      y <- 0 until HEIGHT
    } img.setRGB(x, y, data(x)(y).toInt)
    ImageIO.write(img, "png", file)
  }
}

