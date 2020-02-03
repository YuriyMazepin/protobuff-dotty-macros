package mazepin
package proto

import com.google.protobuf.{CodedOutputStream, CodedInputStream}

object api {
  trait Prepare {
    val size: Int
    def write(os: CodedOutputStream): Unit
  }
    
  trait MessageCodec[A] {
    def prepare(a: A): Prepare
    def read(is: CodedInputStream): A
  }
  
  def encode[A: MessageCodec](a: A)(given c: MessageCodec[A]): Array[Byte] = {
    val p = c.prepare(a)
    val bytes = new Array[Byte](p.size)
    val os = CodedOutputStream.newInstance(bytes)
    p.write(os)
    bytes
  }
    
  def decode[A: MessageCodec](bytes: Array[Byte])(given c: MessageCodec[A]): A = {
    val is = CodedInputStream.newInstance(bytes)
    c.read(is)
  }
}