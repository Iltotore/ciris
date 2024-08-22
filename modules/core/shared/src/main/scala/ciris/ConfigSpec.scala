package ciris

import cats.InvariantSemigroupal
import cats.Show
import cats.syntax.semigroupal._
import ciris.{Secret => SecretValue}
import ciris.{UseOnceSecret => UseOnceSecretValue}
import cats.data.NonEmptyList
import cats.effect.kernel.Async

/**
  * High-level config representation. Can be used for introspection, doc derivation...
  */
sealed trait ConfigSpec[A] {

  import ConfigSpec._

  def secret(implicit show: Show[A]): ConfigSpec[SecretValue[A]] = Secret(this, show)

  def useOnceSecret(implicit ev: A <:< Array[Char]): ConfigSpec[UseOnceSecretValue] = UseOnceSecret(this, ev)

  def default(value: A): ConfigSpec[A] = Default(this, value)

  def or(right: ConfigSpec[A]): ConfigSpec[A] = {
    def getAlternatives(spec: ConfigSpec[A]) = spec match {
      case Alternatives(alternatives) => alternatives
      case _                          => NonEmptyList.one(spec)
    }

    Alternatives(getAlternatives(this) ::: getAlternatives(right))
  }

  def as[B](implicit codec: ConfigCodec[A, B]): ConfigSpec[B] = Codec(this, codec)

  /**
    * Move default values to the leaves of this spec.
    *   
    * @return a new [[ConfigSpec]] equivalent to this one but with default values at its leaves
    */
  lazy val loweredDefaults: ConfigSpec[A] = loweredDefaultsRec(None)

  protected def loweredDefaultsRec(defaultValue: Option[A]): ConfigSpec[A]

  def toConfigValue[F[_]]: ConfigValue[F, A]

  def load[F[x]](implicit F: Async[F]): F[A] = toConfigValue[F].load
}

object ConfigSpec {
  sealed trait Leaf[A] extends ConfigSpec[A] {
    override protected def loweredDefaultsRec(defaultValue: Option[A]): ConfigSpec[A] =
      defaultValue.fold[ConfigSpec[A]](this)(Default(this, _))
  }

  case class Pure[A](value: A) extends Leaf[A] {
    override def toConfigValue[F[_]]: ConfigValue[F,A] = ConfigValue.loaded(ConfigKey("pure value"), value)
  }
  case class Environment(key: String) extends Leaf[String] {
    override def toConfigValue[F[_]]: ConfigValue[F, String] = ciris.env(key)
  }
  case class Property(key: String) extends Leaf[String] {
    override def toConfigValue[F[_]]: ConfigValue[F, String] = ciris.prop(key)
  }
  case class Secret[A](spec: ConfigSpec[A], show: Show[A]) extends ConfigSpec[SecretValue[A]] {
    override def toConfigValue[F[_]]: ConfigValue[F,SecretValue[A]] = spec.toConfigValue[F].secret(show)

    override protected def loweredDefaultsRec(defaultValue: Option[SecretValue[A]]): ConfigSpec[SecretValue[A]] =
      Secret(spec.loweredDefaultsRec(defaultValue.map(_.value)), show)
  }
  case class UseOnceSecret[A](spec: ConfigSpec[A], ev: A <:< Array[Char]) extends ConfigSpec[UseOnceSecretValue] {
    override def toConfigValue[F[_]]: ConfigValue[F,UseOnceSecretValue] = spec.toConfigValue[F].useOnceSecret(ev)

    override protected def loweredDefaultsRec(defaultValue: Option[UseOnceSecretValue]): ConfigSpec[UseOnceSecretValue] =
      defaultValue.fold[ConfigSpec[UseOnceSecretValue]](this)(Default(this, _))
  }
  case class Default[A](spec: ConfigSpec[A], defaultValue: A) extends ConfigSpec[A] {
    override def toConfigValue[F[_]]: ConfigValue[F,A] = spec.toConfigValue[F].default(defaultValue)

    override protected def loweredDefaultsRec(defaultValueParam: Option[A]): ConfigSpec[A] =
      spec.loweredDefaultsRec(defaultValueParam.orElse(Some(defaultValue)))
  }
  case class Alternatives[A](alternatives: NonEmptyList[ConfigSpec[A]]) extends ConfigSpec[A] {
    override def toConfigValue[F[_]]: ConfigValue[F,A] = alternatives.map(_.toConfigValue[F]).reduceLeft(_ or _)

    override protected def loweredDefaultsRec(defaultValue: Option[A]): ConfigSpec[A] =
      Alternatives(alternatives.map(_.loweredDefaultsRec(defaultValue)))
  }
  case class IsoMap[A, B](spec: ConfigSpec[A], f: A => B, g: B => A) extends ConfigSpec[B] {
    override def toConfigValue[F[_]]: ConfigValue[F,B] = spec.toConfigValue[F].map(f)

    override protected def loweredDefaultsRec(defaultValue: Option[B]): ConfigSpec[B] =
      IsoMap(spec.loweredDefaultsRec(defaultValue.map(g)), f, g)
  }

  case class Codec[A, B](spec: ConfigSpec[A], codec: ConfigCodec[A, B]) extends ConfigSpec[B] {
    private lazy val decoder = ConfigDecoder.instance(codec.decode)

    override def as[C](implicit codecC: ConfigCodec[B,C]): ConfigSpec[C] =
      Codec(spec, codec.imapEither(codecC.decode)(codecC.encode)) 

    override def toConfigValue[F[_]]: ConfigValue[F,B] = spec.toConfigValue[F].as(decoder)

    override protected def loweredDefaultsRec(defaultValue: Option[B]): ConfigSpec[B] =
      Codec(spec.loweredDefaultsRec(defaultValue.map(codec.encode)), codec)
  }

  case class Product[A, B](specA: ConfigSpec[A], specB: ConfigSpec[B]) extends ConfigSpec[(A, B)] {
    override def toConfigValue[F[_]]: ConfigValue[F,(A, B)] = specA.toConfigValue[F].product(specB.toConfigValue[F])

    override protected def loweredDefaultsRec(defaultValue: Option[(A, B)]): ConfigSpec[(A, B)] =
      defaultValue.fold(this)(v => Product(specA.default(v._1), specB.default(v._2)))
  }

  def env(key: String): ConfigSpec[String] = Environment(key)
  def prop(key: String): ConfigSpec[String] = Property(key)
  def oneOf[A](alternatives: NonEmptyList[ConfigSpec[A]]): ConfigSpec[A] = Alternatives(alternatives)

  implicit val invariantSemigroupalForSpec: InvariantSemigroupal[ConfigSpec] = new InvariantSemigroupal[ConfigSpec] {

    override def imap[A, B](fa: ConfigSpec[A])(f: A => B)(g: B => A): ConfigSpec[B] = fa match {
      case IsoMap(spec, f0, g0) => IsoMap(spec, f0 andThen f, g0 compose g)
      case _ => IsoMap(fa, f, g)
    }

    override def product[A, B](fa: ConfigSpec[A], fb: ConfigSpec[B]): ConfigSpec[(A, B)] = Product(fa, fb)
  }
}
