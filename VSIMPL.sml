structure VSIMPL =
struct
  datatype aexp =
    AConst of int
  | Var of string
  | Plus of aexp * aexp
  | Minus of aexp * aexp
  | Times of aexp * aexp
  | DividedBy of aexp * aexp

  datatype bexp =
    BConst of bool
  | Equals of aexp * aexp
  | NotEquals of aexp * aexp
  | LessThan of aexp * aexp
  | GreaterThan of aexp * aexp
  | AndAlso of bexp * bexp
  | OrElse of bexp * bexp
  | Not of bexp

  datatype cmd =
    Assign of string * aexp
  | If of bexp * cmd list * cmd list
  | While of bexp * cmd list
  | Return of aexp

  type program = string list * cmd list
end
