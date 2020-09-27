/*
 * Homework Description:
 * Add additional 2D shapes such as triangle and square.
 *
 * In addition to the 2D shapes classes, add also 3D shapes classes
 * (origin, point, sphere, cube, cuboid, 3D triangle - you can add
 * others if you think they are a good fit).
 *
 * Add method `area` to 2D shapes.
 *
 * Add methods `surfaceArea` and `volume` to 3D shapes.
 *
 * If some of the implementation involves advanced math, it is OK
 * to skip it (leave unimplemented), the primary intent of this
 * exercise is modelling using case classes and traits, and not math.
 */

/*
 * Homework Notes.
 *
 * 1. Took the trait ( Movable ) definition solution idea from:
 * https://github.com/jenovs/evo-scala-bootcamp-homework/blob/master/src/main/scala/ClassesAndTraits/Homework.scala
 *
 * Find it better to have them all in one place instead of overwritten within each class.
 *
 * 2. All shapes are implemented in 3D because a 2D version of a shape is a simplified 3D version.
 *
 * 3. Need to look into code duplication and how to fix that.
 *
 * 4. It's assumed that 2D shapes have no height.
 *
 * 5. 3D triangle: the name is too generic. Could be tetrahedron, triangular prism or other options where one of the areas is a triangle.
 * Tetrahedron and triangular prism included.
 */

package basic

object HomeWork2 {

  sealed trait Shape extends Located with Bounded with Movable with Calculations

  sealed trait Located {
    def x: Double
    def y: Double
    def z: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
    def minZ: Double
    def maxZ: Double
  }

  sealed trait Movable {
    def move(x: Double, y: Double, z: Double): Shape =
      this match {
        case Point(px, py, pz) => Point(px + x, py + y, pz + z)
        case Circle(center, radius) =>
          Circle(Point(center.x + x, center.y + y, center.z + z), radius)
        case Sphere(center) =>
          Sphere(
            Circle(
              Point(center.x + x, center.y + y, center.z + z),
              center.radius
            )
          )
        case Triangle(p1, p2, p3) => ???
        case Square(startingPoint, squareWidth) =>
          Square(
            Point(
              startingPoint.x + x,
              startingPoint.y + y,
              startingPoint.z + z
            ),
            squareWidth
          )
        case Rectangle(startingPoint, width, length) =>
          Rectangle(
            Point(
              startingPoint.x + x,
              startingPoint.y + y,
              startingPoint.z + z
            ),
            width,
            length
          )
        case Cube(base, height)              => ???
        case Cuboid(base, height)            => ???
        case Tetrahedron(base, topPoint)     => ???
        case TriangularPrism(base, topPoint) => ???
      }
  }

  // area is a list so it can be applied to 3D shapes as well.
  sealed trait Calculations {
    def area(): List[Double] =
      this match {
        case Point(px, py, pz)                       => List(0.00)
        case Circle(center, radius)                  => List(math.Pi * math.pow(radius, 2))
        case Sphere(center)                          => List(4 * math.Pi * math.pow(center.radius, 2))
        case Triangle(p1, p2, p3)                    => List(???)
        case Square(startingPoint, squareWidth)      => List(???)
        case Rectangle(startingPoint, width, length) => List(???)
        case Cube(base, height)                      => List(???)
        case Cuboid(base, height)                    => List(???)
        case Tetrahedron(base, topPoint)             => List(???)
        case TriangularPrism(base, topPoint)         => List(base.area()).flatten
      }

    def surfaceArea: Double =
      this match {
        case Point(px, py, pz)                       => 0.00
        case Circle(center, radius)                  => (math.Pi * math.pow(radius, 2)) * 2
        case Sphere(center)                          => 4 * math.Pi * math.pow(center.radius, 2)
        case Triangle(p1, p2, p3)                    => ???
        case Square(startingPoint, squareWidth)      => ???
        case Rectangle(startingPoint, width, length) => ???
        case Cube(base, height)                      => ???
        case Cuboid(base, height)                    => ???
        case Tetrahedron(base, topPoint)             => ???
        case TriangularPrism(base, topPoint)         => ???
      }

    def volume: Double =
      this match {
        case Point(px, py, pz)      => 0.00
        case Circle(center, radius) => math.Pi * math.pow(radius, 2)
        case Sphere(center) =>
          4.00 / 3.00 * math.Pi * math.pow(center.radius, 3)
        case Triangle(p1, p2, p3)                    => ???
        case Square(startingPoint, squareWidth)      => ???
        case Rectangle(startingPoint, width, length) => ???
        case Cube(base, height)                      => ???
        case Cuboid(base, height)                    => ???
        case Tetrahedron(base, topPoint)             => ???
        case TriangularPrism(base, topPoint)         => ???
      }
  }

  final case class Point(x: Double, y: Double, z: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
  }

  // There are several types of triangles possible, leaving the calculations undone because the homework doesn't require that.
  final case class Triangle(p1: Point, p2: Point, p3: Point) extends Shape {
    override val x = ???
    override val y = ???
    override val z = ???

    override def minX: Double = ???
    override def maxX: Double = ???
    override def minY: Double = ???
    override def maxY: Double = ???
    override def minZ: Double = ???
    override def maxZ: Double = ???
  }

  final case class Square(startingPoint: Point, squareWidth: Double)
      extends Shape {
    override val x = startingPoint.x
    override val y = startingPoint.y
    override val z = startingPoint.z

    override def minX: Double = x
    override def maxX: Double = x + squareWidth
    override def minY: Double = y
    override def maxY: Double = y + squareWidth
    override def minZ: Double = z
    override def maxZ: Double = z
  }

  final case class Rectangle(
      startingPoint: Point,
      width: Double,
      length: Double
  ) extends Shape {
    override val x = startingPoint.x
    override val y = startingPoint.y
    override val z = startingPoint.z

    override def minX: Double = x
    override def maxX: Double = x + width
    override def minY: Double = y
    override def maxY: Double = y + length
    override def minZ: Double = z
    override def maxZ: Double = z
  }

  final case class Circle(
      center: Point,
      radius: Double
  ) extends Shape {
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z
    override def maxZ: Double = z
  }

  final case class Cube(base: Square, height: Double) extends Shape {
    override val x = base.x
    override val y = base.y
    override val z = base.z

    override def minX: Double = base.minX
    override def maxX: Double = base.maxX
    override def minY: Double = base.minY
    override def maxY: Double = base.maxY
    override def minZ: Double = base.minZ
    override def maxZ: Double = z + height
  }

  final case class Cuboid(base: Rectangle, height: Double) extends Shape {
    override val x = base.x
    override val y = base.y
    override val z = base.z

    override def minX: Double = base.minX
    override def maxX: Double = base.maxX
    override def minY: Double = base.minY
    override def maxY: Double = base.maxY
    override def minZ: Double = base.minZ
    override def maxZ: Double = z + height
  }

  final case class Tetrahedron(base: Triangle, topPoint: Point) extends Shape {
    override val x = base.x
    override val y = base.y
    override val z = base.z

    override def minX: Double = base.minX
    override def maxX: Double = base.maxX
    override def minY: Double = base.minY
    override def maxY: Double = base.maxY
    override def minZ: Double = base.minZ
    override def maxZ: Double = topPoint.maxZ
  }

  final case class TriangularPrism(base: Rectangle, topPoint: Point)
      extends Shape {
    override val x = base.x
    override val y = base.y
    override val z = base.z

    override def minX: Double = base.minX
    override def maxX: Double = base.maxX
    override def minY: Double = base.minY
    override def maxY: Double = base.maxY
    override def minZ: Double = base.minZ
    override def maxZ: Double = topPoint.maxZ
  }

  final case class Sphere(
      center: Circle
  ) extends Shape {
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z
    override def minX: Double = center.minX
    override def maxX: Double = center.maxX
    override def minY: Double = center.minY
    override def maxY: Double = center.maxY
    override def minZ: Double = z - center.radius
    override def maxZ: Double = z + center.radius
  }
}
