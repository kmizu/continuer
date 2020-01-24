package com.github.kmizu.continuer

import java.util

import scala.jdk.CollectionConverters._
import com.github.kmizu.continuer.Type._
import com.github.kmizu.continuer.TypedAst._
import com.github.kmizu.continuer.RecordEnvironment
import scala.jdk.CollectionConverters._

import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Interpreter extends Processor[TypedAst.Program, Value] {interpreter =>
  type Continuation = (Value) => Nothing
  type ParamContinuation = (List[Value]) => Nothing
  def reportError(message: String): Nothing = {
    throw InterpreterException(message)
  }

  def findMethod(self: AnyRef, name: String, params: Array[Value]): MethodSearchResult = {
    val selfClass = self.getClass
    val nameMatchedMethods = selfClass.getMethods.filter {
      _.getName == name
    }
    val maybeUnboxedMethod = nameMatchedMethods.find { m =>
      val parameterCountMatches = m.getParameterCount == params.length
      val parameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches = (m.getParameterTypes zip parameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{m =>
      m.setAccessible(true)
      UnboxedVersionMethodFound(m)
    }
    val maybeBoxedMethod = {
      nameMatchedMethods.find{m =>
        val parameterCountMatches = m.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val boxedParameterTypesMatches = (m.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && boxedParameterTypesMatches
      }
    }.map{m =>
      m.setAccessible(true)
      BoxedVersionMethodFound(m)
    }
    maybeUnboxedMethod.orElse(maybeBoxedMethod).getOrElse(NoMethodFound)
  }

  def findConstructor(target: Class[_], params: Array[Value]): ConstructorSearchResult = {
    val constructors = target.getConstructors
    val maybeUnboxedConstructor = constructors.find{c =>
      val parameterCountMatches = c.getParameterCount == params.length
      val unboxedParameterTypes = Value.classesOfValues(params)
      val parameterTypesMatches  = (c.getParameterTypes zip unboxedParameterTypes).forall{ case (arg, param) =>
        arg.isAssignableFrom(param)
      }
      parameterCountMatches && parameterTypesMatches
    }.map{c =>
      UnboxedVersionConstructorFound(c)
    }
    val maybeBoxedConstructor = {
      constructors.find{c =>
        val parameterCountMatches = c.getParameterCount == params.length
        val boxedParameterTypes = Value.boxedClassesOfValues(params)
        val parameterTypesMatches  = (c.getParameterTypes zip boxedParameterTypes).forall{ case (arg, param) =>
          arg.isAssignableFrom(param)
        }
        parameterCountMatches && parameterTypesMatches
      }
    }.map { c =>
      BoxedVersionConstructorFound(c)
    }
    maybeUnboxedConstructor.orElse(maybeBoxedConstructor).getOrElse(NoConstructorFound)
  }

  object BuiltinEnvironment extends RuntimeEnvironment(None) {
    define("substring"){ case List(ObjectValue(s:String), begin: BoxedInt, end: BoxedInt) =>
      ObjectValue(s.substring(begin.value, end.value))
    }

    define("at") { case List(ObjectValue(s:String), index: BoxedInt) =>
      ObjectValue(s.substring(index.value, index.value + 1))
    }

    define("matches") { case List(ObjectValue(s: String), ObjectValue(regex: String)) =>
      BoxedBoolean(s.matches(regex))
    }

    define("println") { case List(param) =>
      println(param)
      param
    }

    define("printlnError") { case List(param) =>
      Console.err.println(param)
      param
    }

    define("sleep"){ case List(milliseconds: BoxedInt) =>
      Thread.sleep(milliseconds.value)
      UnitValue
    }

    define("assert") { case List(BoxedBoolean(condition)) =>
      ???
    }

    defineValue("save")(SaveFunction)

    defineValue("continue")(ContinueFunction)

    define("assertResult") { case List(a: Value) =>
      NativeFunctionValue{
        case List(b: Value) => ???
      }
    }

    define("head") { case List(ObjectValue(list: java.util.List[_])) =>
      Value.toKlassic(list.get(0).asInstanceOf[AnyRef])
    }
    define("tail") { case List(ObjectValue(list: java.util.List[_])) =>
      Value.toKlassic(list.subList(1, list.size()))
    }
    define("cons") { case List(value: Value) =>
      NativeFunctionValue{ case List(ObjectValue(list: java.util.List[_])) =>
        val newList = new java.util.ArrayList[Any]
        var i = 0
        newList.add(Value.fromKlassic(value))
        while(i < list.size()) {
          newList.add(list.get(i))
          i += 1
        }
        Value.toKlassic(newList)
      }
    }
    define("size") { case List(ObjectValue(list: java.util.List[_])) =>
      BoxedInt(list.size())
    }
    define("isEmpty") { case List(ObjectValue(list: java.util.List[_])) =>
      BoxedBoolean(list.isEmpty)
    }
    define("ToDo") { case Nil => ??? }
    define("url") { case List(ObjectValue(value: String)) =>
      ObjectValue(new java.net.URL(value))
    }
    define("uri") { case List(ObjectValue(value: String)) =>
      ObjectValue(new java.net.URL(value).toURI)
    }
    define("desktop") { case Nil =>
      ObjectValue(java.awt.Desktop.getDesktop())
    }
    defineValue("null")(
      ObjectValue(null)
    )
    defineValue("unit") {
      UnitValue
    }
  }

  object BuiltinRecordEnvironment extends RecordEnvironment() {
    define("Point")(
      "x" -> TInt,
      "y" -> TInt
    )
  }

  object BuiltinModuleEnvironment extends ModuleEnvironment() {
    private final val LIST= "List"
    private final val MAP = "Map"
    private final val SET = "Set"
    enter(LIST) {
      define("head") { case List(ObjectValue(list: java.util.List[_])) =>
        Value.toKlassic(list.get(0).asInstanceOf[AnyRef])
      }
      define("tail") { case List(ObjectValue(list: java.util.List[_])) =>
        Value.toKlassic(list.subList(1, list.size()))
      }
      define("cons") { case List(value: Value) =>
        NativeFunctionValue { case List(ObjectValue(list: java.util.List[_])) =>
          val newList = new java.util.ArrayList[Any]
          var i = 0
          newList.add(Value.fromKlassic(value))
          while (i < list.size()) {
            newList.add(list.get(i))
            i += 1
          }
          Value.toKlassic(newList)
        }
      }
      define("remove") { case List(ObjectValue(self: java.util.List[_])) =>
        NativeFunctionValue{ case List(a: Value) =>
          val newList = new java.util.ArrayList[Any]
          for(v <- self.asScala) {
            newList.add(v)
          }
          newList.remove(Value.fromKlassic(a))
          ObjectValue(newList)
        }
      }
      define("size") { case List(ObjectValue(list: java.util.List[_])) =>
        BoxedInt(list.size())
      }
      define("isEmpty") { case List(ObjectValue(list: java.util.List[_])) =>
        BoxedBoolean(list.isEmpty)
      }
    }
  }

  def toList(row: Type): List[(String, Type)] = row match {
    case tv@TVariable(_) => sys.error("cannot reach here")
    case otherwise => throw TyperPanic("Unexpected: " + otherwise)
  }

  case class EvaluationResult(value: Value) extends Exception

  final def interpret(program: TypedAst.Program): Value = {
    try {
      interpreter.evaluate(program.block, env = BuiltinEnvironment, moduleEnv = BuiltinModuleEnvironment){v =>
        throw EvaluationResult(v)
      }
    }catch {
      case e: EvaluationResult => e.value
    }
  }

  private def evaluate(node: TypedNode)(c: Continuation): Value = {
    evaluate(node, BuiltinEnvironment)(c)
  }

  private def performFunctionInternal(func: TypedNode, params: List[TypedNode], env: RuntimeEnvironment)(c: Continuation): Nothing = {
    performFunction(TypedAst.FunctionCall(TDynamic, NoLocation, func, params), env)(c)
  }

  private def performFunction(node: TypedAst.FunctionCall, env: RuntimeEnvironment)(c: Continuation): Nothing = node match {
    case TypedAst.FunctionCall(type_, location, function, params) =>
      evaluate(function, env) { v =>
        v match {
          case FunctionValue(TypedAst.FunctionLiteral(type_, location, fparams, optionalType, proc), cenv) =>
            val local = new RuntimeEnvironment(cenv)
            (fparams zip params).foreach { case (fp, ap) =>
              evaluate(ap, env){v =>
                local(fp.name) = v
                c(v)
              }
            }
            evaluate(proc, local){v =>
              c(v)
            }
          case SaveFunction =>
            c(ContinuationValue(c))
          case ContinueFunction =>
            evaluate(params.head, env){c =>
              c.asInstanceOf[ContinuationValue].f(UnitValue)
            }
          case NativeFunctionValue(body) =>
            def evalParams(params: List[TypedNode], actualParams: List[Value])(c: ParamContinuation): Nothing = params match {
              case p::pRest =>
                evaluate(p, env){v =>
                  evalParams(pRest, v::actualParams)(c)
                }
              case Nil =>
                val trueActualParams = actualParams.reverse
                if (body.isDefinedAt(trueActualParams)) {
                  c(actualParams.reverse)
                } else {
                  reportError("parameters are not matched to the function's arguments")
                }
            }
            evalParams(params, Nil){params =>
              c(body(params))
            }
          case _ =>
            reportError("unknown error")
        }
      }
  }

  private def evaluate(node: TypedNode, env: RuntimeEnvironment, recordEnv: RecordEnvironment = BuiltinRecordEnvironment, moduleEnv: ModuleEnvironment = BuiltinModuleEnvironment)(c: Continuation): Nothing = {
    def evalRecursive(node: TypedNode)(c: Continuation): Nothing = {
      node match {
        case TypedAst.Block(type_, location, expressions) =>
          val local = new RuntimeEnvironment(Some(env))
          def evalBlock(block :List[TypedNode])(c: Continuation): Nothing = block match {
            case e :: Nil =>
              evalRecursive(e){v =>
                c(v)
              }
            case e::es =>
              evalRecursive(e){v =>
                evalBlock(es)(c)
              }
            case Nil =>
              c(UnitValue)
          }
          evalBlock(expressions)(c)
        case TypedAst.IfExpression(type_, location, condition, pos, neg) =>
          evalRecursive(condition){v =>
            v match {
              case BoxedBoolean(true) => evalRecursive(pos)(c)
              case BoxedBoolean(false) => evalRecursive(neg)(c)
              case _ => reportError("type error")
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.AND2, lhs, rhs) =>
          evalRecursive(lhs){lv =>
            lv match {
              case BoxedBoolean(true) => evalRecursive(rhs){rv => c(rv)}
              case BoxedBoolean(false) => c(BoxedBoolean(false))
              case _ => reportError("type error")
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.BAR2, lhs, rhs) =>
          evalRecursive(lhs) { lv =>
            lv match {
              case BoxedBoolean(false) => evalRecursive(rhs) { rv => c(rv) }
              case BoxedBoolean(true) => c(BoxedBoolean(true))
              case _ => reportError("type error")
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.EQUAL, left, right) =>
          evalRecursive(left){lval =>
            evalRecursive(right){rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedBoolean(lval == rval))
                case (BoxedBoolean(lval), BoxedBoolean(rval)) => c(BoxedBoolean(lval == rval))
                case (BoxedBoolean(lval), ObjectValue(rval:java.lang.Boolean)) => c(BoxedBoolean(lval == rval.booleanValue()))
                case (ObjectValue(lval:java.lang.Boolean), BoxedBoolean(rval)) => c(BoxedBoolean(lval.booleanValue() == rval))
                case (ObjectValue(lval), ObjectValue(rval)) => c(BoxedBoolean(lval == rval))
                case _ => reportError("comparation must be done between same types")
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.LESS_THAN, left, right) =>
          evalRecursive(left){lval =>
            evalRecursive(right){rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedBoolean(lval < rval))
                case _ => reportError("comparation must be done between numeric types")
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.GREATER_THAN, left, right) =>
          evalRecursive(left){lval =>
            evalRecursive(right){rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedBoolean(lval > rval))
                case _ => reportError("comparation must be done between numeric types")
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.LESS_OR_EQUAL, left, right) =>
          evalRecursive(left){lval =>
            evalRecursive(right){rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedBoolean(lval <= rval))
                case _ => reportError("comparation must be done between numeric types")
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.GREATER_EQUAL, left, right) =>
          evalRecursive(left){lval =>
            evalRecursive(right){rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedBoolean(lval >= rval))
                case _ => reportError("comparation must be done between numeric types")
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.ADD, left, right) =>
          evalRecursive(left) { lval =>
            evalRecursive(right) { rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedInt(lval + rval))
                case (ObjectValue(lval: String), rval) => c(ObjectValue(lval + rval))
                case (lval, ObjectValue(rval: String)) => c(ObjectValue(lval.toString + rval))
                case _ => reportError("arithmetic operation must be done between the same numeric types")
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.SUBTRACT, left, right) =>
          evalRecursive(left) { lval =>
            evalRecursive(right) { rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedInt(lval - rval))
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.MULTIPLY, left, right) =>
          evalRecursive(left) { lval =>
            evalRecursive(right) { rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedInt(lval * rval))
              }
            }
          }
        case TypedAst.BinaryExpression(type_, location, Operator.DIVIDE, left, right) =>
          evalRecursive(left) { lval =>
            evalRecursive(right) { rval =>
              (lval, rval) match {
                case (BoxedInt(lval), BoxedInt(rval)) => c(BoxedInt(lval / rval))
              }
            }
          }
        case TypedAst.MinusOp(type_, location, operand) =>
          evalRecursive(operand) { v =>
            v match {
              case BoxedInt(value) => c(BoxedInt(-value))
              case _ => reportError("- cannot be applied to non-integer value")
            }
          }
        case TypedAst.PlusOp(type_, location, operand) =>
          evalRecursive(operand) { v =>
            v match {
              case BoxedInt(value) => c(BoxedInt(+value))
              case _ => reportError("- cannot be applied to non-integer value")
            }
          }
        case TypedAst.IntNode(type_, location, value) =>
          c(BoxedInt(value))
        case TypedAst.StringNode(type_, location, value) =>
          c(ObjectValue(value))
        case TypedAst.BooleanNode(type_, location, value) =>
          c(BoxedBoolean(value))
        case TypedAst.Id(type_, location, name) =>
          c(env(name))
        case TypedAst.LetDeclaration(type_, location, vr, optVariableType, value, body, immutable) =>
          evalRecursive(value){v =>
            env(vr) = v
            evalRecursive(body){bv =>
              c(bv)
            }
          }
        case TypedAst.Capture(type_, location, variable, variableType, body) =>
          lazy val captured: Continuation = {_ =>
            env(variable) = ContinuationValue(captured)
            evalRecursive(body){v =>
              c(v)
            }
          }
          captured(UnitValue)
        case TypedAst.Assignment(type_, location, vr, value) =>
          evalRecursive(value){v =>
            val k: Value = env.set(vr, v)
            c(k)
          }
        case literal@TypedAst.FunctionLiteral(type_, location, _, _, _) =>
          c(FunctionValue(literal, Some(env)))
        case TypedAst.LetFunctionDefinition(type_, location, name, body, expression) =>
          evalRecursive(expression){v =>
            env(name) = FunctionValue(body, Some(env)): Value
            c(v)
          }
        case call@TypedAst.FunctionCall(type_, location, function, params) =>
          performFunction(call, env)(c)
        case TypedAst.ValueNode(value) =>
          c(value)
        case otherwise => sys.error(s"cannot reach here: ${otherwise}")
      }
    }
    evalRecursive(node)(c)
  }

  override final val name: String = "Interpreter"

  override final def process(input: TypedAst.Program): Value = {
    interpret(input)
  }
}
