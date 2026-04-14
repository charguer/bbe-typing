# Augmented Language Constructs

This file lists the constructs of the augmented language using paper-style meta-syntax.

Meta-variables:

- `t`, `t1`, `t2`, ..., `tn` range over terms.
- `b`, `b1`, `b2`, ..., `bn` range over binding boolean expressions.
- `p`, `p1`, `p2`, ..., `pn` range over patterns.
- `C` ranges over constructors.
- `f`, `g` range over functions, predicates, or inversors depending on context.
- `L` ranges over labels.

## Term Constructs

| Construct | Example |
|---|---|
| variable | `x` |
| wildcard term | `__` |
| constant term | `c` |
| unit term | `()` |
| boolean term | `true` |
| tuple term | `(t1, ..., tn)` |
| constructor term | `C` |
| unary constructor term | `C t` |
| tupled constructor term | `C (t1, ..., tn)` |
| let-binding | `let x = t1 in t2` |
| anonymous let-binding | `let _ = t1 in t2` |
| recursive let-binding | `let rec f = t1 in t2` |
| type-annotated let-binding | `let x : ty = t1 in t2` |
| function | `fun x1 ... xn -> t` |
| function with typed arguments | `fun (x1 : ty1) ... (xn : tyn) -> t` |
| function by cases | <code>function p1 -&gt; t1 &#124; ... &#124; pn -&gt; tn</code> |
| application | `t0 t1 ... tn` |
| sequence | `t1; t2` |
| term annotation | `(t : ty)` |
| local abstract type | `fun (type a) -> t` |
| ordinary conditional | `if t1 then t2 else t3` |
| conditional without `else` | `if t1 then t2` |
| native match | <code>match t0 with p1 -&gt; t1 &#124; ... &#124; pn -&gt; tn</code> |
| native guarded match case | <code>match t0 with p1 when t1 -&gt; t2 &#124; ...</code> |
| ordinary while-loop | `while t1 do t2 done` |
| `assert false` | `assert false` |
| external declaration | `external f : ty = "prim"` |

## Binding Boolean Expressions

| Construct | Example |
|---|---|
| `is` test | `t @_is p` |
| conjunction | `b1 && b2` |
| disjunction | <code>b1 &#124;&#124; b2</code> |
| negation | `not b` |
| BBE conditional | `if b then t1 else t2` |
| BBE loop guard | `while b do t done` |

## Pattern Constructs

| Construct | Example |
|---|---|
| pattern variable | `??x` |
| wildcard | `__` |
| constant pattern | `c` |
| tuple pattern | `(p1, ..., pn)` |
| nullary constructor pattern | `C` |
| constructor pattern | `C p` |
| tupled constructor pattern | `C (p1, ..., pn)` |
| annotated pattern | `(p : ty)` |
| guarded pattern | `p @_when b` |
| pattern conjunction | `p1 && p2` |
| pattern disjunction | <code>p1 &#124;&#124; p2</code> |
| pattern negation | `not p` |
| predicate pattern | `g` |
| view pattern | `f p` |
| multi-argument view pattern | `f p1 ... pn` |

## Switch And Match Constructs

| Construct | Example |
|---|---|
| switch | `__switch [ __case (b1 @_then t1); ...; __case (bn @_then tn) ]` |
| switch case | `__case (b @_then t)` |
| case separator | `b @_then t` |
| labeled switch | `__switch "L" [ __case (b1 @_then t1); ...; __case (bn @_then tn) ]` |
| unlabeled switch | `__switch [ __case (b1 @_then t1); ...; __case (bn @_then tn) ]` |
| labeled custom match | `__match "L" t0 [ __case (p1 @_then t1); ...; __case (pn @_then tn) ]` |
| unlabeled custom match | `__match t0 [ __case (p1 @_then t1); ...; __case (pn @_then tn) ]` |

## Label-Based Control Constructs

| Construct | Example |
|---|---|
| labeled function | `fun[@label "L"] x -> t` |
| labeled `if` | `if[@label "L"] b then t1 else t2` |
| labeled native match | <code>match[@label "L"] t0 with p1 -&gt; t1 &#124; ... &#124; pn -&gt; tn</code> |
| labeled while-loop | `while[@label "L"] b do t done` |
| `next` | `__next "L"` |
| block | `__block "L" t` |
| exit | `__exit "L" t` |
| return | `__return "L" t` |
| break | `__break "L"` |
| continue | `__continue "L"` |
| raw next exception | `raise (Exn_Next "L")` |
| raw exit exception | `raise (Exn_Exit ("L", t))` |

## Partial Application With Holes

| Construct | Example |
|---|---|
| one-hole partial application | `?!(f t1 __ t2)` |
| multi-hole partial application | `?!(f __ t2 __ ... __ tn)` |

## Typical Derived Forms

| Construct | Example |
|---|---|
| tuple destructuring test | `t @_is (p1, ..., pn)` |
| constructor destructuring test | `t @_is C (p1, ..., pn)` |
| guarded destructuring test | `t @_is (p @_when b)` |
| nested pattern test | `t @_is C (p1, C' (p2, p3), p4)` |
| chained BBE test | `(t1 @_is p1) && (t2 @_is p2)` |
| alternative BBE test | <code>(t1 @_is p1) &#124;&#124; (t2 @_is p2)</code> |
| switch over BBEs | `__switch [ __case (t1 @_is p1 @_then t2); __case (true @_then t3) ]` |
| match over patterns | `__match t0 [ __case (C p1 @_then t1); __case (__ @_then t2) ]` |

## Notes

- The notation is schematic: for example, `(p1, ..., pn)` means a tuple of patterns.
- Likewise, `(t1, ..., tn)` means a tuple of terms.
- Likewise, <code>p1 -&gt; t1 &#124; ... &#124; pn -&gt; tn</code> means a finite list of match or function branches.
- Likewise, `f p1 ... pn` means a view-pattern application with one or more pattern arguments.
- The construct `b @_then t` is only used inside `__case`, and is not accepted by the parser otherwise
- The custom constructs `__match` and `__switch` can be written without a label, in which case the generated code would forward any exception, including labeled interruptions.
