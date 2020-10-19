module CGenerate

open System
open CParser

let createLabel length prefix =
    let r = Random()
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
    let sz = Array.length chars in
    "_" + prefix + "_" + String(Array.init length (fun _ -> chars.[r.Next sz]))

let rec generateCProgram (Program main) = generateFunction main

and generateFunction (Func (name, stmt)) =
    match stmt with
    | (Return expr) -> generateFuncDeclaration name + generateReturnStatement expr

and generateFuncDeclaration name = let uname = "_" + name in ".globl " + uname + "\n" + uname + ":\n"

and generateReturnStatement expr = generateExpr expr + "\nret"

and or' e2 =
    let eval2ndLabel = createLabel 10 "OR_eval2nd"
    let endLabel = createLabel 10 "OR_end"
    "cmpq $0, %rax\n"
    + "je " + eval2ndLabel + "\n"
    + "movq $1, %rax\n"
    + "jmp " + endLabel + "\n"
    + eval2ndLabel + ":\n"
    + generateExpr2 e2
    + "cmpq $0, %rax\n"
    + "movq $0, %rax\n"
    + "setne %al\n"
    + endLabel + ":\n"

and and' e3 =
    let eval2ndLabel = createLabel 10 "AND_eval2nd"
    let endLabel = createLabel 10 "AND_end"
    "cmpq $0, %rax\n"
    + "jne " + eval2ndLabel + "\n"
    + "movq $0, %rax\n"
    + "jmp " + endLabel + "\n"
    + eval2ndLabel + ":\n"
    + generateExpr3 e3
    + "cmpq $0, %rax\n"
    + "movq $0, %rax\n"
    + "setne %al\n"
    + endLabel + ":\n"

and eq e4 = "push %rax\n" + generateExpr4 e4 + "\npop %rcx\n" + "cmpq %rcx, %rax\n" + "movq $0, %rax\n" + "sete %al\n"
and neq e4 = "push %rax\n" + generateExpr4 e4 + "\npop %rcx\n" + "cmpq %rcx, %rax\n" + "movq $0, %rax\n" + "setne %al\n"

and comparePrecedence4 op e5 = "push %rax\n" + generateExpr5 e5 + "\npop %rcx\n" + "cmpq %rax, %rcx\n" + "movq $0, %rax\n" + op + " %al\n"
and gt = comparePrecedence4 "setg"
and gte = comparePrecedence4 "setge"
and lt = comparePrecedence4 "setl"
and lte = comparePrecedence4 "setle"

and add expr6 = "push %rax\n" + generateExpr6 expr6+ "\npop %rcx\n"+ "add %rcx, %rax\n"
and sub expr6 = "push %rax\n" + generateExpr6 expr6+ "\npop %rcx\n" + "sub %rax, %rcx\n" + "movq %rcx, %rax\n"

// Left operand is in %rax. Push it to stack. Generate right operand (%rax). Pop left operand to %rcx. Multiply.
and mul expr7 = "push %rax\n" + generateExpr7 expr7 + "\npop %rcx\n" + "imul %rcx, %rax\n"
// Left operand is in %rax. Push it to stack. Generate right operand, and move it to %rcx. Pop left operand to %rax. Sign extend %rax to 16 bytes (%rdx+%rax). Divide 16-byte-value by %rcx.
and div expr7 = "push %rax\n" + generateExpr7 expr7 + "\nmovq %rax, %rcx\n" + "pop %rax\n" + "cqo\n" + "idivq %rcx\n"

and branchExpr e =
    let op = match fst e with
                | OrOp -> or'
    in snd e |> op

and branchExpr2 e =
    let op = match fst e with
                | AndOp -> and'
    in snd e |> op

and branchExpr3 e =
    let op = match fst e with
                | EqOp -> eq
                | NeqOp -> neq
    in snd e |> op

and branchExpr4 e =
    let op = match fst e with
                | GtOp -> gt
                | GteOp -> gte
                | LtOp -> lt
                | LteOp -> lte
    in snd e |> op

and branchExpr5 e =
    let op = match fst e with
                | AddOp -> add
                | SubOp -> sub
    in snd e |> op

and branchExpr6 t =
    let op = match fst t with
                | MulOp -> mul
                | DivOp -> div
    in snd t |> op

and generateExpr (Expr (expr2, maybeMore)) =
    generateExpr2 expr2 + ((List.map branchExpr maybeMore) |> String.concat "\n")

and generateExpr2 (Expr2 (expr3, maybeMore)) =
    generateExpr3 expr3 + ((List.map branchExpr2 maybeMore) |> String.concat "\n")

and generateExpr3 (Expr3 (expr4, maybeMore)) =
    generateExpr4 expr4 + ((List.map branchExpr3 maybeMore) |> String.concat "\n")

and generateExpr4 (Expr4 (expr5, maybeMore)) =
    generateExpr5 expr5 + ((List.map branchExpr4 maybeMore) |> String.concat "\n")

and generateExpr5 (Expr5 (expr6, maybeMore)) =
    generateExpr6 expr6+ ((List.map branchExpr5 maybeMore) |> String.concat "\n")

and generateExpr6 (Expr6 (expr7, maybeMore)) =
    generateExpr7 expr7 + ((List.map branchExpr6 maybeMore) |> String.concat "\n")

and generateExpr7 f =
    match f with
    | Parenthesized expr -> generateExpr expr
    | UnaryOp (u, expr7) -> generateUnaryOp u expr7
    | Const n -> "movq $" + string n + ", %rax\n"

and generateUnaryOp op expr7 =
    let generatedExpr7 = generateExpr7 expr7
    match op with
    | NegationOp -> generatedExpr7 + "\n" + "neg %rax\n"
    | BitwiseComplOp -> generatedExpr7 + "\n" + "not %rax\n"
    // Is RAX 0? If yes, set ZF (zero flag) on. Set RAX = 0. Set AL (lowest 8 bits of RAX) to 1 if ZF is on.
    | LogicalNegationOp -> generatedExpr7 + "\n" + "cmpq $0, %rax" + "\n" + "movq $0, %rax" + "\n" + "sete %al\n"


let compileCProgram = parseCProgram >> Result.map generateCProgram