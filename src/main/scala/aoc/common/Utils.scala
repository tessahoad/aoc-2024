package aoc.common

trait Utils:
    def loadResource(name: String): List[String] =
      val resourcePath = this.getClass.getPackageName.replace('.', '/') + "/" + name
      val source = scala.io.Source.fromResource(resourcePath)
      val lines = source.getLines().toList
      source.close()
      lines

