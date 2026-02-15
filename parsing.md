## Informations on AST representations

terms
---

$t @_is $p
parsed by ocaml as: Pexp_app (Pexp_app (Pexp_var "@_is") $t) $p <!-- not sure of this one, check later anyway -->
in our ast: Trm_bbe_is (t, p)


At top level

let _ =
let x =
let x : t =
<!-- type a = .. -->

The notation $t$ denotes the translation of t in respectively OCaml's parse tree, or our AST. 

variable: 
x
parsed by OCaml as : Pexp_ident "x"
in our ast: Trm_var "x"

let-bindings: 
let x = t1 in t2
parsed by OCaml as : Pexp_let Nonrec ((Ppat_var "x", $t1$), $t2$)
in our ast: Trm_let ({bind = Some ("x", None); body = $t1$}, $t2$)

<!-- Different depending on the usage :
let _ =
let x =
let x : T = -->

<!-- LATER : add OCaml representation for every construct -->

[let _ = ...] is parsed as: Trm_let ({bind = None; body = ...} ... )
[let x : T = ... ] is parsed as: Trm_let ({bind = Some ("x", Some T); body = ... } ...)

function definitions:
let f x1 ... xn = t
in our ast: Trm_let ({bind = Some ("f", None); 
                      body = Trm_funs ([("x1", None); ...; ("xn", None)], $t$)}, ...)

function application: 
f t1 ... tn
in our ast: Trm_apps ($f$, [$t1$, ..., $tn$])

conditional: 
if b then t1 else t2
in our ast: Trm_if ($b$, $t1$, $t2$)

switch: 
switch [
  case b1 @_then t1;
  ...
  case bn @_then tn
]
in our ast:
Trm_switch [
  ($b1$, $t1$);
  ...
  ($bn$, $tn$)
]


Note: switch is equivalent to a cascade of if, that is:

  if b1 then t1 else
  ..
  if bn then tn else
  raise Switch_failure

encoding of match: 
  __match v [
    case p1 @_then t1;
    ...
    case pn @_then tn;
  ]

BBEs
---
boolean condition: t (*Native*) 
pattern matching: t @_is p 
BBE and: b1 && b2 (*Native*)
BBE or: b1 || b2 (*Native*)
BBE not: not b (*Native*)

patterns
---
pattern variables: ??x    
guarded pattern: p @_when b
intersection: p1 && p2
disjunction: p1 || p2
negation: not p
alias: p @_as x
wildcard: __ 
inversor: f (p1, ..., pn)     
inversor: f p1 pn 
predicate: g


Implementation details:

  - The infix syntax "@xxx" does not turn "xxx" into an infix binary function.
  - This means that, contrary to usual infix binary operators, the OCaml AST would represent "b1 @_then t1" as:
  ```
  Pexp_apply
    expression 
      Pexp_ident "@"
    [
      <arg>
      Nolabel
        expression 
          Pexp_ident "b1" 
      <arg>
      Nolabel
        expression 
          Pexp_apply
          expression 
            Pexp_ident "_then"
          [
            <arg>
            Nolabel
              expression 
                Pexp_ident "t1"
          ]
    ]
  ```

Syntax choices: 
  - For already existing constructs, adding label is done with attributes. (e.g., "if[@label L] b then t1 else t2" or "(if b then t1 else t2)[@label L]")
  - For new constructs (such as exit, next or block), we use a new syntax, that expect a label as argument. (e.g., '__block "L" e' where e is an OCaml expression, or '__exit "L" e')

```
"pexp_attributes": 
[
  [
    {
      "type": "string Asttypes.loc",
      "txt": "label",
      "loc": {
        "type": "Location.t",
        "loc_start": {
          "type": "Lexing.position",
          "pos_fname": "",
          "pos_lnum": 1,
          "pos_bol": 0,
          "pos_cnum": 7
        },
        "loc_end": {
          "type": "Lexing.position",
          "pos_fname": "",
          "pos_lnum": 1,
          "pos_bol": 0,
          "pos_cnum": 12
        },
        "loc_ghost": false
      }
    },
    {
      "type": "PStr",
      "structure": [
        {
          "type": "structure_item",
          "pstr_desc": {
            "type": "Pstr_eval",
            "expression": {
              "type": "expression",
              "pexp_desc": {
                "type": "Pexp_construct",
                "id_loc": {
                  "type": "Longident.t Asttypes.loc",
                  "txt": {
                    "type": "Lident",
                    "li": "L"
                  },
                  "loc": {
                    "type": "Location.t",
                    "loc_start": {
                      "type": "Lexing.position",
                      "pos_fname": "",
                      "pos_lnum": 1,
                      "pos_bol": 0,
                      "pos_cnum": 13
                    },
                    "loc_end": {
                      "type": "Lexing.position",
                      "pos_fname": "",
                      "pos_lnum": 1,
                      "pos_bol": 0,
                      "pos_cnum": 14
                    },
                    "loc_ghost": false
                  }
                },
                "expression": {
                  "type": "None"
                }
              },
              "pexp_loc": {
                "type": "Location.t",
                "loc_start": {
                  "type": "Lexing.position",
                  "pos_fname": "",
                  "pos_lnum": 1,
                  "pos_bol": 0,
                  "pos_cnum": 13
                },
                "loc_end": {
                  "type": "Lexing.position",
                  "pos_fname": "",
                  "pos_lnum": 1,
                  "pos_bol": 0,
                  "pos_cnum": 14
                },
                "loc_ghost": false
              },
              "pexp_attributes": []
            },
            "attributes": []
          },
          "pstr_loc": {
            "type": "Location.t",
            "loc_start": {
              "type": "Lexing.position",
              "pos_fname": "",
              "pos_lnum": 1,
              "pos_bol": 0,
              "pos_cnum": 13
            },
            "loc_end": {
              "type": "Lexing.position",
              "pos_fname": "",
              "pos_lnum": 1,
              "pos_bol": 0,
              "pos_cnum": 14
            },
            "loc_ghost": false
          }
        }
      ]
    }
  ]
]
```


Implementation details for labels: 

The attribute is at the "expression" level, in the same layer as Pexp_ifthenelse. 

For already existing expressions, we've made the effort to add label as an attribute as not to break usability.
But for new constructs, such as blocks, switches etc. We've decided to create a new keyword.


The same thing for while, functions, and blocks.