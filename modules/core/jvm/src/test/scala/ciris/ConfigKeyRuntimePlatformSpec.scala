/*
 * Copyright 2017-2022 Viktor Lövgren
 *
 * SPDX-License-Identifier: MIT
 */

package ciris

import cats.syntax.all._
import java.nio.charset.Charset
import java.nio.file.Path
import munit.ScalaCheckSuite
import org.scalacheck.Prop.forAll

final class ConfigKeyRuntimePlatformSpec extends ScalaCheckSuite with Generators {
  property("ConfigKey.file") {
    forAll { (path: Path, charset: Charset) =>
      val description = ConfigKey.file(path, charset).description
      description === s"file at $path with charset $charset"
    }
  }
}
