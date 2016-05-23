#Welcome to the Current SLAC compiler.

Slac OFF

# Compiler Extension lab 7
Generics: Type-Parametric Programming
Implement Typed classes.
## Basic Description
implement single type generics.
## Programs highlighting the user of the features
Containers.
Linked Lists.
Stacks.
Queues.
## sketch of changes to phases
LinkedList[T1] is an abstract type.
T exists only in class if its exclared in T
Generated on the fly, stored in a list.

AbstractClass.classes is a LIST of ClassDecls, each evaluated when generated,
by substituting in the INT for T1 everywhere.

## ideas for extension
Multi type generics.
