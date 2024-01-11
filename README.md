# APL Interpreter

An implementation of [APL](https://en.wikipedia.org/wiki/APL_(programming_language)), written in Haskell.

## Supported Syntax
- All primitives, except ⌹, ⌺, and ⌸
    - Functions
        - (Monadic): ⍎⍕
        - (Dyadic): <≤≥>=⍲⍱∧∨⊤⊥∪∩⍷/⌿\⍀
        - (Ambivalent): +-×÷⍳⍴\*⍟⌊⌈⊢⊣|≡≢○!?⍉⌽⊖∊⍋⍒~≠⊃⊂⊆,⍪⌷⍸↓↑
    - Operators
        - (Monadic): ⍨¨∘./⌿\⍀
        - (Dyadic): ⍤⍥∘⍣@.
- Direct Functions (dfns) (Lambda Functions)
- Direct Operators (dops) (Higher-Order Lambda Functions)
- Assignment
    - Modified Assignment (x+←1)
    - Selective Assignment (((⌽x)←y); (w x←y z); x[i;j;k]←y)
- [Tacit](https://aplwiki.com/wiki/Tacit_programming) (forks and atops)
- [Axis Specification](https://aplwiki.com/wiki/Function_axis) (⌽[2]x)
- Array Subscripting
- [Stranding](https://aplwiki.com/wiki/Strand_notation)
- I/O with (⎕←) and ⍞
- [Quad Names](https://aplwiki.com/wiki/Quad_name) (⎕IO)
- Misc. Primitive Glyphs: ¯⍬⋄⍝

## User Interface
- Configurable REPL ([Haskeline](https://hackage.haskell.org/package/haskeline-0.8.2.1/docs/System-Console-Haskeline.html))
- Pretty-Printing for Arrays (boxing) and Derived Functions (trees)
- `-v` option for printing syntax trees before evaluation
