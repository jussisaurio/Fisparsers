module CGenerate

open CTokenizer
open CParser

let (<!>) func1 func2 = func1 >> Result.map func2

let rec generateCProgram (Program main) =
    match main with
        | func -> generateFunction func


and generateFunction (Func (name, stmt)) =
    match stmt with
    | (Return expr) -> generateFuncDeclaration name + generateReturnStatement expr

and generateFuncDeclaration name = let uname = "_" + name in ".globl " + uname + "\n" + uname + ":\n"

and generateReturnStatement expr = "movq " + generateExpression expr + ", %rax" + "\nret"

and generateExpression e =
    match e with
    | (Const n) -> "$" + string n


let compileCProgram = parseCProgram <!> generateCProgram