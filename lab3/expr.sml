(* Alvaro Aleman, Michael Zuo
 * expr.sml
 * cs334
 *)

(* Magic constant to make datatypes print out fully *)
Control.Print.printDepth:= 1000;
Control.Print.printLength:= 1000;

datatype Expr =
    VarX
  | VarY
  | Sine     of Expr
  | Cosine   of Expr
  | Average  of Expr * Expr
  | Times    of Expr * Expr
  | Frob     of Expr * Expr * Expr
  | Pow      of Expr * Expr
  | Glub     of Expr * Expr;


(* build functions:
     Use these helper functions to generate elements of the Expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program
*)
fun buildX _ = VarX;
fun buildY _ = VarY;
val buildSine = Sine;
val buildCosine = Cosine;
val buildAverage = Average;
val buildTimes = Times;
val buildFrob = Frob;
val buildPow = Pow
val buildGlub = Glub;

(* exprToString : Expr -> string
   Complete this function to convert an Expr to a string
*)
fun exprToString VarX = "x"
  | exprToString VarY = "y"
  | exprToString (Sine e) = "sin(pi*"^exprToString e^")"
  | exprToString (Cosine e) = "cos(pi*"^exprToString e^")"
  | exprToString (Average (e,r)) = "("^exprToString e^"+"^exprToString r^")/2"
  | exprToString (Times (e,r)) = exprToString e^"*"^exprToString r
  | exprToString (Frob (e,r,s)) =
      "frob("^exprToString e^","^exprToString r^","^exprToString s^")"
  | exprToString (Pow (e,r)) = "pow???("^exprToString e^","^exprToString r^")"
  | exprToString (Glub (e,r)) = "glub("^exprToString e^","^exprToString r^")"


(* eval : Expr -> real*real -> real
   Evaluator for expressions in x and y
 *)
fun eval VarX (x, _) = x
  | eval VarY (_, y) = y
  | eval (Sine e) p = Math.sin (Math.pi * eval e p)
  | eval (Cosine e) p = Math.cos (Math.pi * eval e p)
  | eval (Average (e,r)) p = (eval e p + eval r p) / 2.0
  | eval (Times (e,r)) p = eval e p * eval r p

  (* new functions which meet the requirement of mapping [-1,1] to [-1,1] *)
  (* frob (b -> c, a -> b, a -> b) -> a -> c, a = b = c = [-1,1] *)
  | eval (Frob (e,r,s)) p = eval e (eval r p, eval s p)
  (* we restrict to [-1,1] by inverting anything outside the range *)
  | eval (Pow (e,r)) p = let val res = Math.pow (abs (eval e p), eval r p)
                            in if abs res > 1.0 then 1.0/res else res end
  (* we have no idea what this does either *)
  | eval (Glub (e,r)) p = case floor (10.0*(3.0 + eval e p + eval r p)) mod 6 of
                               0 => eval e p
                             | 1 => eval r p
                             | 2 => 0.36
                             | 3 => eval e (eval r p, 0.6) * eval r p
                             | 4 => eval e (~0.36, eval r p)
                             | 5 => eval r (eval r p, eval e p)
                             | _ => ~8.0

	
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
