happy-plus-alex
===============

Template for quickly and easily combining happy and alex

The documentation for happy and alex is technically correct, but it does a poor
job of explaining to new users how to easily combine happy and alex. It turns
out that combining them is actually quite simple and natural. This repository
gives you straight forward sample code for a simple expression language.

The key insight is that almost everyone that wants to combine happy and alex
wants to use a monad and wants to have alex track source position. Therefore,
we should ask alex to generate a monadic parser and tell happy to use it.
