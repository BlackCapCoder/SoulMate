# SoulMate

SoulMate is an esoteric programming language where you have two stack machines cooperating to achieve turing-completeness. In case you don't know, a stack machine alone is not turing-complete, but two or more cooperating is.

Besides stack manipulation operations, the machines support the NAnd gate, with which you can simulate all the other logic operations. The machines work on the bit-level.

SoulMate currently support the following operations:

| Symbol | Description                                                                                                    |
|--------|----------------------------------------------------------------------------------------------------------------|
| `&`    | Pops two bits off the stack, then pushes the result of the NAnd operation                                      |
| `:`    | Duplicates the topmost element on the stack                                                                    |
| `/`    | Swaps the two topmost elements on the stack                                                                    |
| `,`    | Give focus to the other stack machine such that all proceeding operations will be executed on the second stack |
| `#`    | Pops the topmost element off the stack and push it to the other machine's stack                                |

*At the moment SoulMate is not turing-complete, because there is no way to loop.*

## IO

Initially, both stacks are empty. If either machine attempts to pop from an empty stack, a single bit is read from STDIN. If no input is available on STDIN you get a zero instead.

Once the program terminates the bits of the active stack are written to STDOUT.

## byte version

Operations that work on bytes implemented in terms of bits can be found in the `8` folder. So, for instance, if you wanted the first 3 bytes of the input in reverse order on the stack, you could just go:
``` bash
(cd 8; cat swap pass swap toggle pass toggle swap) > code
echo abc | stack exec nand-exe -- code
```
