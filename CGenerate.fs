module CGenerate

open CParser

let branch f1 f2 pred x = let op = snd x in if fst x |> pred then f1 op else f2 op

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
    | NegationOp -> generatedFactor + "\n" + "neg %rax\n"
    | BitwiseComplOp -> generatedFactor + "\n" + "not %rax\n"
    // Is RAX 0? If yes, set ZF (zero flag) on. Set RAX = 0. Set AL (lowest 8 bits of RAX) to 1 if ZF is on.
    | LogicalNegationOp -> generatedFactor + "\n" + "cmpq $0, %rax" + "\n" + "movq $0, %rax" + "\n" + "sete %al\n"

and generateFactor f =
    match f with
    | Parenthesized expr -> generateExpression expr
    | UnaryOp (u, fct) -> generateUnaryOp u fct
    | Const n -> "movq $" + string n + ", %rax\n"

and add term = "push %rax\n" + generateTerm term + "\npop %rcx\n" + "add %rcx, %rax\n"
and sub term = "push %rax\n" + generateTerm term + "\npop %rcx\n" + "sub %rax, %rcx\n" + "movq %rcx, %rax\n"
// Left operand is in %rax. Push it to stack. Generate right operand (%rax). Pop left operand to %rcx. Multiply.
and mul fct = "push %rax\n" + generateFactor fct + "\npop %rcx\n" + "imul %rcx, %rax\n"
// Left operand is in %rax. Push it to stack. Generate right operand, and move it to %rcx. Pop left operand to %rax. Sign extend %rax to 16 bytes (%rdx+%rax). Divide 16-byte-value by %rcx.
and div fct = "push %rax\n" + generateFactor fct + "\nmovq %rax, %rcx\n" + "pop %rax\n" + "cqo\n" + "idivq %rcx\n"

and generateTerm (Term (factor, maybeOtherFactors)) =
    generateFactor factor + ((List.map (branch mul div ((=) MultiOp)) maybeOtherFactors) |> String.concat "\n")

and generateExpression (Expr (term, maybeOtherTerms)) =
    generateTerm term + ((List.map (branch add sub ((=) PlusOp)) maybeOtherTerms) |> String.concat "\n")


let compileCProgram = parseCProgram >> Result.map generateCProgram