Bang
====

An <b>E</b>mbedded <b>D</b>omain <b>S</b>pecific <b>L</b>anguage for writing drum machine patterns in Haskell.

Todos:
- <s>Move notes to rational durations</s>
- **Prove stuff about the associativity of operators and draw inspiration from the polymorphic media paper
- Add more instruments.

Ideas:
- Instead of deriving an Eq instance, define your own. (i.e. `Rest 0 _ >> m == m`)