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

and generateReturnStatement expr = generateExpression expr + "\nret"

and generateExpression e =
    match e with
    | (Const n) -> "movq $" + string n + ", %rax"
    | (Unary (op,expr)) ->
        match op with
        | Negation -> generateExpression expr + "\n" + "neg %rax"
        | BitwiseComplement -> generateExpression expr + "\n" + "not %rax"
        // Is RAX 0? If yes, set ZF (zero flag) on. Set RAX = 0. Set AL (lowest 8 bits of RAX) to 1 if ZF is on.
        | LogicalNegation -> generateExpression expr + "\n" + "cmpq $0, %rax" + "\n" + "movq $0, %rax" + "\n" + "sete %al"


let compileCProgram = parseCProgram <!> generateCProgram