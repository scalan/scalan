package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Relate annotated element with an external type or method given by `value` property.
* For example WArray related to Array is annotated as @External("Array")
* This annotation is used to separate wrapper Entity from user defined virtualized Entity.
* See isW */
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface External {
   String value() default "";  // default value interpreted as "external name is equal to annotated element name"
}
