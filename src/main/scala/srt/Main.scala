package srt

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

object Main {
  val WIDTH = 400
  val HEIGHT = 400

  def main(args: Array[String]) {
    args.toList match {
      case output :: Nil => write(trace, output)
      case _ => sys exit 1
    }
  }

  def trace = for (x <- 0 until WIDTH) yield for (y <- 0 until HEIGHT) yield x * y

  def write(data: Seq[Seq[Int]], output: String) {
    val img = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB)
    for {
      x <- 0 until WIDTH
      y <- 0 until HEIGHT
    } img.setRGB(x, y, data(x)(y))
    ImageIO.write(img, "png", new File(s"$output.png"))
  }
}
