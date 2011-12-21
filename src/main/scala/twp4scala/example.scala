package twp4scala

import protocol.tfs._

package object example {
  implicit def pastrh(str: String) = new {
    def ! : Path = str.split("/")
  }
}