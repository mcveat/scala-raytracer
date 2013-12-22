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
  val TRACING_DEPTH = 1
  val SPECULAR_COEFFICIENT_POWER = 50
}

object Main {
  val scene = {
    val cameraPosition = Vector(WIDTH * 2, HEIGHT * 1.1, -4000)
    Scene(
      Camera(
        position = cameraPosition,
        direction = (Vector(0, 200, 2000) - cameraPosition).normalize,
        up = Vector(0, 1, 0).normalize,
        planeDistance = 3000
      ),
      List(
        Plane(Vector(0, 1, 0), 0, Material(Color.white, 0.3, 0.7)),
        Sphere(Vector(200, 200, 200), 200, Material(Color.green, 0.3, 0.7)),
        Sphere(Vector(600, 200, 200), 200, Material(Color.blue, 0.3, 0.7)),
        Sphere(Vector(600, 200, 600), 200, Material(Color.red, 0.3, 0.7)),
        Sphere(Vector(200, 200, 600), 200, Material(Color.white, 0.3, 0.7 ))
      ),
      Light(Vector(WIDTH * 2, HEIGHT * 2, -4000), Color.white)
    )
  }

  def main(args: Array[String]) {
    args.toList match {
      case output :: Nil => ImageWriter.write(scene.render, new File(s"$output.png"))
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
