(* Alvaro Aleman, Michael Zuo
 * expr-func.sml
 * cs334
 *)

(* Magic constant to make datatypes print out fully *)
Control.Print.printDepth:= 1000;
Control.Print.printLength:= 1000;

type Expr = real * real -> real;

(* eval : Expr -> real*real -> real
   Evaluator for expressions in x and y
 *)
fun eval f p = f p;

(* build functions:
     Use these helper functions to generate elements of the Expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program

val buildSine = fn : ('a -> real) -> 'a -> real
              = fn : Expr -> Expr, 'a ~ real * real
val buildCosine = fn : ('a -> real) -> 'a -> real
                = fn : Expr -> Expr, 'a ~ real * real
val buildAverage = fn : ('a -> real) * ('a -> real) -> 'a -> real
                 = fn : Expr * Expr -> Expr, 'a ~ real * real
val buildTimes = fn : ('a -> real) * ('a -> real) -> 'a -> real
               = fn : Expr * Expr -> Expr, 'a ~ real * real
val buildFrob = fn : ('a * 'b -> 'c) * ('d -> 'a) * ('d -> 'b) -> 'd -> 'c
              = fn : Expr * Expr * Expr -> Expr, 'a ~ 'b ~ real, 'd ~ real * real
val buildPow = fn : ('a -> real) * ('a -> real) -> 'a -> real
             = fn : Expr * Expr -> Expr, 'a ~ real * real
val buildGlub = fn
  : (real * real -> real) * (real * real -> real) -> real * real -> real

These types are more general than they need to be because the type inference
algorithm doens't require them to be more specific.
*)
fun buildX _ (x,_) = x;
fun buildY _ (_,y) = y;
fun buildSine e p = Math.sin (Math.pi * eval e p);
fun buildCosine e p = Math.cos (Math.pi * eval e p);
fun buildAverage (e,r) p = (eval e p + eval r p) / 2.0;
(* need annotation to prevent inferred int *)
fun buildTimes (e,r) p = eval e p * eval r p : real;
(* See expr.sml for more info on these functions *)
fun buildFrob (e,r,s) p = eval e (eval r p, eval s p);
fun buildPow (e,r) p = let val res = Math.pow (abs (eval e p), eval r p)
                     in if abs res > 1.0 then 1.0/res else res end;
fun buildGlub (e,r) p = case floor (10.0*(3.0 + eval e p + eval r p)) mod 6 of
                               0 => eval e p
                             | 1 => eval r p
                             | 2 => 0.36
                             | 3 => eval e (eval r p, 0.6) * eval r p
                             | 4 => eval e (~0.36, eval r p)
                             | 5 => eval r (eval r p, eval e p)
                             | _ => ~8.0;

(* exprToString : Expr -> string
   Complete this function to convert an Expr to a string
*)
fun exprToString _ = "[Function/missile launcher, we can't tell]"

val sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())));

(************** Add Testing Code Here ***************)
val sample1 =
  buildTimes (buildSine (buildX ()),
              buildCosine (buildTimes (buildX (), buildY ())));
val sample2 = buildSine (buildAverage (buildX (), buildY ()));

exprToString sampleExpr ^ "\n";
exprToString sample1;
exprToString sample2;

eval sample2 (0.5, 0.0);
eval sampleExpr (0.1, 0.1);
