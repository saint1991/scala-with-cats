package com.github.saint1991.cats.chapter1

sealed trait Json
final case class JsonObject(self: Map[String, Json]) extends Json
final case class JsonString(self: String) extends Json
final case class JsonNumber(self: Double) extends Json
case object JsonNull extends Json

// type class
trait JsonWriter[T] {
  def write(value: T): Json
}

object JsonWriter {
  // implicit type class instances
  implicit val stringJsonWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String): Json = JsonString(value)
  }

  // implicit scope is searched recursively
  implicit def optionJsonWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    (value: Option[A]) => value match {
      case None => JsonNull
      case Some(v) => writer.write(v)
    }
}

object JsonSyntax {
  implicit class JsonConverters[T](val self: T) extends AnyVal {
    def toJson()(implicit writer: JsonWriter[T]): Json = writer.write(self)
  }
}
