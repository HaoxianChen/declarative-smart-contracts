package datalog

sealed abstract class Type {
  def name: String
  override def toString: String = name
}
case class UnitType() extends Type {
  val name:String = "Unit"
}
case class AnyType() extends Type {
  val name: String = "Any"
}
case class SymbolType(name: String) extends Type
case class NumberType(name: String) extends Type
case class BooleanType() extends Type {
  val name: String = "bool"
}
sealed abstract class CompoundType extends Type
case class MapType(key: Type, value: Type) extends CompoundType {
  def name: String = s"mapping($key=>$value)"
}
case class StructType(name: String, members: List[Parameter]) extends CompoundType
case class ArrayType(elementType: Type) extends CompoundType {
  val name: String = s"$elementType[]"
}

object Type {
  def apply(name: String): Type = name match {
    case "int" => integerType
    case "uint" => uintType
    case "address" => addressType
    case _ => SymbolType(name)
  }
  val addressType: Type = SymbolType("address")
  val integerType: Type = NumberType("int")
  val uintType: Type = NumberType("uint")
}

