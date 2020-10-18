module CGenerate

open CParser

let rec generateCProgram (Program main) =
    match main with
        | func -> generateFunction func


and generateFunction (Func (name, stmt)) =
    match stmt with
    | (Return expr) -> generateFuncDeclaration name + generateReturnStatement expr

and generateFuncDeclaration name = let uname = "_" + name in ".globl " + uname + "\n" + uname + ":\n"

and generateReturnStatement expr = generateExpression expr + "\nret"

and generateUnaryOp op fct =
    let generatedFactor = generateFactor fct
    match op with
    | NegationOp -> generatedFactor + "\n" + "neg %rax"
    | BitwiseComplOp -> generatedFactor + "\n" + "not %rax"
    // Is RAX 0? If yes, set ZF (zero flag) on. Set RAX = 0. Set AL (lowest 8 bits of RAX) to 1 if ZF is on.
    | LogicalNegationOp -> generatedFactor + "\n" + "cmpq $0, %rax" + "\n" + "movq $0, %rax" + "\n" + "sete %al"

and generateFactor f =
    match f with
    | Parenthesized expr -> generateExpression expr
    | UnaryOp (u, fct) -> generateUnaryOp u fct
    | Const n -> "movq $" + string n + ", %rax"

and generateTerm (Term (factor, maybeOtherFactors)) =
    let generatedFactor = generateFactor factor
    let rest =
        match maybeOtherFactors with
        | [] -> ""
        // Left operand is in %rax. Push it to stack. Generate right operand (%rax). Pop left operand to %rcx. Multiply.
        | (MultiOp, factor2)::xs ->  "push %rax\n" + generateTerm (Term (factor2, xs)) + "\npop %rcx\n" + "imul %rcx, %rax\n"
        // Left operand is in %rax. Push it to stack. Generate right operand, and move it to %rcx. Pop left operand to %rax. Sign extend %rax to 8 bytes (%rdx+%rax). Divide 8-byte-value by %rcx.
        // %rax stores quotient, %rdx stores remainder. We can ignore the remainder because we are working with ints.
        | (DivOp, factor2)::xs ->  "push %rax\n" + generateTerm (Term (factor2, xs)) + "\nmovq %rax, %rcx\n" + "pop %rax\n" + "cqo\n" + "idivq %rcx"
    generatedFactor + "\n" + rest

and generateExpression (Expr (term, maybeOtherTerms)) =
    let generatedTerm = generateTerm term
    let rest =
        match maybeOtherTerms with
        | [] -> ""
        | (PlusOp, term2)::xs -> "push %rax\n" + generateExpression (Expr (term2, xs)) + "\npop %rcx\n" + "add %rcx, %rax\n"
        | (MinusOp, term2)::xs -> "push %rax\n" + generateExpression (Expr (term2, xs)) + "\npop %rcx\n" + "sub %rax, %rcx\n" + "movq %rcx, %rax\n"
    generatedTerm + "\n" + rest


let compileCProgram = parseCProgram >> Result.map generateCProgram