# SoulMate

SoulMate is an esoteric programming language where you have two stack machines cooperating to achieve turing-completeness. In case you don't know, a stack machine alone is not turing-complete, but two or more cooperating is.

Besides stack manipulation operations, the machines support the NAnd gate, with which you can simulate all the other logic operations. The machines work on the bit-level.

SoulMate currently support the following operations:

| Symbol | Description                                                                                                    |
|--------|----------------------------------------------------------------------------------------------------------------|
| `&`    | Pops two bits off the stack, then pushes the result of the NAnd operation                                      |
| `0`    | Pushes a `0` bit                                                                                               |
| `:`    | Duplicates the topmost element on the stack                                                                    |
| `/`    | Swaps the two topmost elements on the stack                                                                    |
| `$`    | Deletes the topmost element on the stack                                                                       |
| `,`    | Give focus to the other stack machine such that all proceeding operations will be executed on the second stack |
| `#`    | Pops the topmost element off the stack and push it to the other machine's stack                                |

*At the moment SoulMate is not turing-complete, because there is no way to loop.*

## IO

Initially, both stacks are empty. If either machine attempts to pop from an empty stack, a single bit is read from STDIN. If no input is available on STDIN you get a zero instead.

Once the program terminates, both stacks are concatenated and the resulting sequence of bits are written to STDOUT.
