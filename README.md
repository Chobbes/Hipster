# Hipster
A Haskell MIPS EDSL. Mostly for "educational purposes" (TM). More seriously the goal of this is to explore assembly DSLs in Haskell, and see how they're put together and what makes them tick. Can these be useful? Probably! One of the main goals is to allow for composable abstractions of assembly code, and automatic register allocation / spilling. In theory this could be more general and support multiple backends (after all there is overlap with LLVM's functionality), but an additional goal is to be able to generate readable, and documented assembly code. Because of this, it's beneficial to stick to one target assembly language, at least for experimentation!

# Labels
We want to be able to create named labels for use in the assembly code in order to aid with readability. Naively this would cause problems with creating abstractions as any named labels within an abstraction (say a loop), would be problematic since if the abstraction is used multiple times the label will be repeated. To get around this issue we allow labels to be allocated with a name, but a unique number may be appended to the end when the labels are actually compiled.

Keeping track of procedure names, and allowing the assembly lanuage to use this for prefixes to labels may also be ideal.

# Comments
We want the user to be able to write well commented assembly code. This means we are going to have to have comments at the label level, and at the assembly level. Additionally some formatting primitives must be available.
