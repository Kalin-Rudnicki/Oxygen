
# Oxygen

You can't survive without oxygen, and you can't survive as a scala dev without the `Oxygen` library.

`Oxygen` has everything from helpful little extension methods `.some`/`.asRight`,
to a full-blown HTTP client+server,
and even a web-framework that lets you write your FE in scala too!



# Project Highlights



## oxygen-core

<span style="color: red; font-size: 50pt">TODO</span>

### Description

<span style="color: red; font-size: 50pt">TODO</span>

### Highlights & Examples

- [todo](modules/core/src/test/scala/oxygen/core/examples/___.scala) <span style="color: red; font-size: 50pt">TODO</span>

### Dependencies

- external:
    - [izumi-reflect](https://github.com/zio/izumi-reflect)
      - ZIO dependency that has nothing to do with ZIO. As the readme describes, this is simply a lightweight improvement over `scala-reflect`'s `TypeTag`.
      - This is added as a dependency because having a good representation of a type is of fundamental importance to the scala ecosystem.
      - `Oxygen` adds nothing to the `izumi-reflect` model. In fact, it dumbs it down. The goal is for a more practical usage of being able to show types, not to know everything about a type.
- internal:
    - No other `oxygen` dependencies, this is the root of the `oxygen` ecosystem.



## oxygen-test

### Description

- This is a lightweight wrapper around `zio-test`.
- Added for the purpose of better integrating with the `oxygen` ecosystem (logging, types, etc.)

### Highlights & Examples

- [todo](modules/core/src/main/scala/oxygen/predef/core.scala) <span style="color: red; font-size: 50pt">TODO</span>

### Dependencies

- external:
    - [external-lib](https://www.github.com/account/repo) <span style="color: red; font-size: 50pt">TODO</span>
- internal:
    - [oxygen-core](#oxygen-core)
