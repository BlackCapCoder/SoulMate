# Reflection

These are some reflections around the loop. I assume no IO as it is not required for turing-completeness


* ~~If the loop contains no `:` it must be total.~~ This is true neither for `JZ` or `JNZ`:
  1. JZ:  `[]`
  2. JNZ: `&[&]`
* If the loop contains no `,`, `#` is equivalent to `pop` in the context of totality
* It is theoretically possible to convert non-layered loops to the pogo problem.
