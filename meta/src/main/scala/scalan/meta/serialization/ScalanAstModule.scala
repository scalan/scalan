package scalan.meta.serialization

import java.time.Instant
import com.fasterxml.jackson.databind.Module.SetupContext
import com.fasterxml.jackson.databind.module.{SimpleSerializers, SimpleDeserializers}
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.core._
import com.fasterxml.jackson.databind.jsontype.{TypeSerializer, TypeDeserializer}
import scalan.meta.ScalanAst
import scalan.meta.ScalanAst.STpePrimitive
import scalan.meta.ScalanAst.STpePrimitives

object ScalanAstModule extends Module {
  override def getModuleName: String = "ScalanAst"

  override def setupModule(context: SetupContext): Unit = {
    val serializers = new SimpleSerializers
    serializers.addSerializer(classOf[STpePrimitive], new JsonSerializer[STpePrimitive] {
      override def serialize(value: STpePrimitive, gen: JsonGenerator, serializers: SerializerProvider): Unit = {
        gen.writeStringField("ty", value.name)
        //        gen.writeStringField("name", value.name)
      }

      override def serializeWithType(value: STpePrimitive,
                                     gen: JsonGenerator,
                                     serializers: SerializerProvider,
                                     typeSer: TypeSerializer): Unit = {
        val id = typeSer.typeId(value, JsonToken.START_OBJECT)
        typeSer.writeTypePrefix(gen, id)
        serialize(value, gen, serializers) // call your customized serialize method
        id.wrapperWritten = !gen.canWriteTypeId();
        typeSer.writeTypeSuffix(gen, id)
      }
    })
    val deserializers = new SimpleDeserializers
    deserializers.addDeserializer(classOf[STpePrimitive], new JsonDeserializer[STpePrimitive] {
      override def deserialize(p: JsonParser, ctxt: DeserializationContext): STpePrimitive = {
        val node: TreeNode = p.getCodec.readTree(p)
        val value = node.get("ty").asToken().asString()
        //        p.nextValue()
        //        val tyName = p.getText()
        STpePrimitives(value)
      }

      //      override def deserializeWithType(p: JsonParser,
      //                                       ctxt: DeserializationContext,
      //                                       deser: TypeDeserializer): AnyRef = {
      //        val name = p.nextFieldName()
      //        val tyName = p.nextTextValue()
      //        STpePrimitives(tyName)
      //      }
    })
    context.addSerializers(serializers)
    context.addDeserializers(deserializers)
  }

  override def version(): Version = new Version(0, 4, 0, "SNAPSHOT", "group", "artifact")
}

