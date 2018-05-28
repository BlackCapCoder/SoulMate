# SoulMate

SoulMate is an esoteric programming language where you have two stack machines cooperating to achieve turing-completeness. In case you don't know, a stack machine alone is not turing-complete, but two or more cooperating is.

Besides stack manipulation operations, the machines support the NAND gate, with which you can simulate all of the other logic operations. The machines work on the bit-level.

SoulMate currently support the following operations:

| Symbol | Description                                                                                                   |
|--------|---------------------------------------------------------------------------------------------------------------|
| `:`    | Duplicates the topmost element on the stack                                                                   |
| `/`    | Swaps the two topmost elements on the stack                                                                   |
| `,`    | Give focus to the other stack machine such that all proceeding operations will be executed on the other stack |
| `#`    | Pops the topmost element off the stack, then pushes it on to the other machine's stack                        |
| `&`    | Pops two bits off the stack, then pushes the result of the NAND operation                                     |
| `[`    | Pop a bit, if zero, jump past matching bracket                                                                |
| `]`    | Jump to matching bracket                                                                                      |

## IO

Initially, both stacks are empty. If either machine attempts to pop from an empty stack, a single bit is read from STDIN. If no input is available on STDIN you get a zero instead.

Once the program terminates the bits on the active stack are written to STDOUT.

## The assembler

Because SoulMate is an unreadable mess, I decided to create an assembler. The assembler has the following syntax:

```
# Relative or absolute path to file to import
# lib/8bit
# lib/gates/mux
# comma, works, too

sectionName {
  regular SoulMate code
  code code code
  indentaion doesn't actually matter
}

anotherSection {
  Use a dollar sign followed by a section name to
  paste it into the code, like this: $sectionName
  code code $sectionName code code
}

withParams lorem ipsum {
  $lorem : $lorem / [$ipsum]
}

example {
  $withParams { This is lorem }
              { This is ipsum }
}
```
