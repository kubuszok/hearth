# Source Utilities

Source utilities are simple utilities exposing:

 - enclosing method name
 - line number
 - current file
 - current file's name

as implicits/givens.

!!! example "`Source` utilities"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep com.kubuszok::hearth::{{ hearth_version() }}
    import hearth.source.*

    println(summon[MethodName])
    println(summon[Line])
    println(summon[File])
    println(summon[FileName])
    println(summon[Location])
    ```
