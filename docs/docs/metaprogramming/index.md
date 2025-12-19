
# Metaprogramming

The oxygen library heavily relies on macros.
With Scala-3 came two core concepts: `Macros` and `Mirrors`.
All the popular "typeclass derivation" libraries like `shapeless`, `magnolia`, and `kittens` primarily use `Mirrors`.

!!! danger "Hot Take"

    With all the respect for the team that works on scala, I strongly believe that `Mirrors` are a giant mistake, and should `never` be used.

The core reasons for this bold statement are as follows:

1. You have almost zero control over what code is generated.
2. You have no control over what is actually inlined, and what is not.
   This means that if the compiler can not verify something at compile-time, it's just going to convert it to a fallible run-time operation instead, without any warning.
3. If you want to do anything remotely complicated, you are going to need to go into the land of macros anyway to be able to access what you need,
   and you're better off staying in that world than living 50-50 in macros and inline defs.
4. If you aren't doing anything remotely complicated, you're still better off doing things like typeclass derivation in raw macros, and you know `exactly` what will be generated every time,
   and can optimize things `knowing` that those optimizations will happen at compile-time, and not be suddenly shifted to run-time because the compiler couldn't figure it out.

While this all sounds very negative, there is very positive news. Macros exist!
The scala 3 macro system is extremely powerful, and lets you do some really amazing things.
While the idea of metaprogramming using raw macros can sound very daunting, it shouldn't be, as generating code using macros feels just like writing normal scala code.

While `Scala 3 Macros` are amazing, there is one major downside to them. In my opinion, the API to work with these metaprogramming concepts is... very unfortunate.
All the types are nested within the `Quotes.reflect.*` instance you are given, and it makes splitting things into separate functions/files a living nightmare,
and on top of it, the IDE (at least IntelliJ) has no clue what's going on.

## Where Oxygen Comes In

Oxygen has 2 libraries to help with this, `oxygen-quoted` and `oxygen-meta`.

[Oxygen Quoted](quoted.md) is a zero-dependency lightweight wrapper around the `Quotes.reflect` interface, which lifts all the types out of `Quotes.reflect` into top-level definitions.
Aka: `oxygen.quoted.Term` instead of `scala.quoted.Quotes#reflect#Term`.
Its goal is to wrap the quotes API as verbatim as possible (with a few extra little helpers here and there), in order to make these reflect types accessible.

[Oxygen Meta](meta.md) is where it really gets fun. Here, you have helpers which assist in doing common metaprogramming tasks.
I have a [YouTube series](https://www.youtube.com/playlist?list=PLdN_zFyLBVR8jQFNsBzwdPKxIZhD033oa) which shows how you can efficiently derive type-classes in Scala 3 using macros.
What `Mirrors` give you is a black magic box where you use special compile-time functions to extract things like field names and types in order to access what you want.  
What `Oxygen Meta` gives you is a raw-macro variant where you can work with these concepts just like normal types:
```scala
trait ProductGeneric[A] {

  def name: String
  def tpe: Type[A]
  def fields: Seq[Field[?]]
  def annotations: Annotations

  trait Field[B] {
    def name: String
    def tpe: Type[B]
    def annotations: Annotations
  }

}
```

Read more about what you can do with these libraries in their respective sub-pages!
