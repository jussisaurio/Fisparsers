module CGenerate

open System
open Parser
open CParser

type CompileError = CodeGenerationError of string | ParseError of ParseError
// TODO dont use mutable data structures
let mutable variables: Map<string, int> = Map.empty
let mutable stackIndex = -8 // 64-bit word above RBP (frame pointer)

let createLabel length prefix =
    let r = Random()
    let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
    let sz = Array.length chars in
    "_" + prefix + "_" + String(Array.init length (fun _ -> chars.[r.Next sz]))

let rec generateCProgram (Program main) =
    variables <- Map.empty
    stackIndex <- -8
    // TODO no exceptions
    try
        generateFunction main |> Ok
    with
    | :? Exception as e -> e.Message |> CodeGenerationError |> Error


and generateFunction (Func (name, blockItems)) =
    (generateFuncDeclaration name) + generateFunctionPrologue + generateBlockItems blockItems

and generateFunctionPrologue =
    "push %rbp\n" + // store frame pointer on stack
    "movq %rsp, %rbp\n" // current top o' stack saved as frame pointer

and generateFunctionEpilogue =
    "movq %rbp, %rsp\n" + // restore stack pointer to where it was before this func call
    "pop %rbp\n" + // restore frame pointer to what it was before this func call
    "ret\n" // return from function

and generateBlockItems blockItems =
    let fork b =
        match b with
        | (S s) -> generateStatement s
        | (D d) -> generateVariableDeclaration d
    let rec g bs =
        match bs with
        | [] -> "movq $0, %rax\n" + generateFunctionEpilogue
        | [lastItem] ->
            match lastItem with
            | (S (Return expr)) -> generateReturnStatement expr
            | any -> fork any + g []
        | x::xs -> fork x + g xs
    g blockItems

and generateStatement s =
    match s with
    | Return expr -> generateReturnStatement expr
    | StandaloneExp expr -> generateExpr expr
    | IfStatement (a,b,c) -> generateIfStatement a b c

and generateIfStatement test whenTrue maybeElse =
    let generatedCondition = generateExpr test
    let generatedWhenTrue = generateStatement whenTrue
    let generatedElse = Option.map generateStatement maybeElse |> Option.defaultValue ""

    let elseLabel = createLabel 10 "IF_evalElse"
    let endLabel = createLabel 10 "IF_end"

    generatedCondition +
    "cmpq $0, %rax\n"
    + "je " + elseLabel + "\n"
    + generatedWhenTrue
    + "jmp " + endLabel + "\n"
    + elseLabel + ":\n"
    + generatedElse
    + endLabel + ":\n"


and generateVariableDeclaration (VariableDeclaration (name, expr)) =
    // TODO resultify this whole thing instead of exceptions
    if variables.ContainsKey name then failwith "Cant declare twice"
    else
        let generated = (Option.map generateExpr expr |> Option.defaultValue "movq $0, %rax\n") // assign 0 by default
        // push to stack and store location
        variables <- variables.Add (name, stackIndex) // TODO dont mutate
        let assign = "push %rax\n"
        stackIndex <- stackIndex - 8 // TODO dont mutate
        generated + assign

and generateFuncDeclaration name = let uname = "_" + name in ".globl " + uname + "\n" + uname + ":\n"

and generateReturnStatement expr = generateExpr expr + generateFunctionEpilogue

and or' e2 =
    let eval2ndLabel = createLabel 10 "OR_eval2nd"
    let endLabel = createLabel 10 "OR_end"
    "cmpq $0, %rax\n"
    + "je " + eval2ndLabel + "\n"
    + "movq $1, %rax\n"
    + "jmp " + endLabel + "\n"
    + eval2ndLabel + ":\n"
    + generateExpr3 e2
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
    + generateExpr4 e3
    + "cmpq $0, %rax\n"
    + "movq $0, %rax\n"
    + "setne %al\n"
    + endLabel + ":\n"

and eq e4 = "push %rax\n" + generateExpr5 e4 + "\npop %rcx\n" + "cmpq %rcx, %rax\n" + "movq $0, %rax\n" + "sete %al\n"
and neq e4 = "push %rax\n" + generateExpr5 e4 + "\npop %rcx\n" + "cmpq %rcx, %rax\n" + "movq $0, %rax\n" + "setne %al\n"

and comparePrecedence5 op e5 = "push %rax\n" + generateExpr6 e5 + "\npop %rcx\n" + "cmpq %rax, %rcx\n" + "movq $0, %rax\n" + op + " %al\n"
and gt = comparePrecedence5 "setg"
and gte = comparePrecedence5 "setge"
and lt = comparePrecedence5 "setl"
and lte = comparePrecedence5 "setle"

and add expr7 = "push %rax\n" + generateExpr7 expr7+ "\npop %rcx\n"+ "add %rcx, %rax\n"
and sub expr7 = "push %rax\n" + generateExpr7 expr7+ "\npop %rcx\n" + "sub %rax, %rcx\n" + "movq %rcx, %rax\n"

// Left operand is in %rax. Push it to stack. Generate right operand (%rax). Pop left operand to %rcx. Multiply.
and mul expr8 = "push %rax\n" + generateExpr8 expr8 + "\npop %rcx\n" + "imul %rcx, %rax\n"
// Left operand is in %rax. Push it to stack. Generate right operand, and move it to %rcx. Pop left operand to %rax. Sign extend %rax to 16 bytes (%rdx+%rax). Divide 16-byte-value by %rcx.
and div expr8 = "push %rax\n" + generateExpr8 expr8 + "\nmovq %rax, %rcx\n" + "pop %rax\n" + "cqo\n" + "idivq %rcx\n"

and branchExpr2 e =
    let op = match fst e with
                | OrOp -> or'
    in snd e |> op

and branchExpr3 e =
    let op = match fst e with
                | AndOp -> and'
    in snd e |> op

and branchExpr4 e =
    let op = match fst e with
                | EqOp -> eq
                | NeqOp -> neq
    in snd e |> op

and branchExpr5 e =
    let op = match fst e with
                | GtOp -> gt
                | GteOp -> gte
                | LtOp -> lt
                | LteOp -> lte
    in snd e |> op

and branchExpr6 e =
    let op = match fst e with
                | AddOp -> add
                | SubOp -> sub
    in snd e |> op

and branchExpr7 t =
    let op = match fst t with
                | MulOp -> mul
                | DivOp -> div
    in snd t |> op

and generateExpr e =
    match e with
    | (Expr e1) -> generateExpr1 e1
    | (Assign (id, _, ex)) -> generateAssignment id ex

and generateAssignment id ex =
    let generated = generateExpr ex
    let assign =
        match variables.TryFind id with
        | None -> "Assignment to nonexistent variable " + id |> failwith
        | Some offset -> "movq %rax, " + string offset + "(%rbp)\n"
    generated + assign

and generateExpr1 e =
    match e with
    | (TernaryExpr (test, wTrue, wFalse)) -> generateTernary test wTrue wFalse
    | (Expr1 other) -> generateExpr2 other

and generateTernary test wTrue wFalse =
    let generatedCondition = generateExpr2 test
    let generatedWhenTrue = generateExpr wTrue
    let generatedWhenFalse = generateExpr1 wFalse

    let elseLabel = createLabel 10 "TERN_evalElse"
    let endLabel = createLabel 10 "TERN_end"

    generatedCondition +
    "cmpq $0, %rax\n"
    + "je " + elseLabel + "\n"
    + generatedWhenTrue
    + "jmp " + endLabel + "\n"
    + elseLabel + ":\n"
    + generatedWhenFalse
    + endLabel + ":\n"

and generateExpr2 (Expr2 (expr3, maybeMore)) =
    generateExpr3 expr3 + ((List.map branchExpr2 maybeMore) |> String.concat "\n")

and generateExpr3 (Expr3 (expr4, maybeMore)) =
    generateExpr4 expr4 + ((List.map branchExpr3 maybeMore) |> String.concat "\n")

and generateExpr4 (Expr4 (expr5, maybeMore)) =
    generateExpr5 expr5 + ((List.map branchExpr4 maybeMore) |> String.concat "\n")

and generateExpr5 (Expr5 (expr6, maybeMore)) =
    generateExpr6 expr6 + ((List.map branchExpr5 maybeMore) |> String.concat "\n")

and generateExpr6 (Expr6 (expr7, maybeMore)) =
    generateExpr7 expr7+ ((List.map branchExpr6 maybeMore) |> String.concat "\n")

and generateExpr7 (Expr7 (expr8, maybeMore)) =
    generateExpr8 expr8 + ((List.map branchExpr7 maybeMore) |> String.concat "\n")

and generateExpr8 f =
    match f with
    | Parenthesized expr -> generateExpr expr
    | UnaryOp (u, expr8) -> generateUnaryOp u expr8
    | Const n -> "movq $" + string n + ", %rax\n"
    | Ident var ->
        match variables.TryFind var with
        | None -> "Undefined variable " + var |> failwith
        | Some offset -> "movq " + string offset + "(%rbp), %rax\n"

and generateUnaryOp op expr8 =
    let generatedExpr8 = generateExpr8 expr8
    match op with
    | NegationOp -> generatedExpr8 + "\n" + "neg %rax\n"
    | BitwiseComplOp -> generatedExpr8 + "\n" + "not %rax\n"
    // Is RAX 0? If yes, set ZF (zero flag) on. Set RAX = 0. Set AL (lowest 8 bits of RAX) to 1 if ZF is on.
    | LogicalNegationOp -> generatedExpr8 + "\n" + "cmpq $0, %rax" + "\n" + "movq $0, %rax" + "\n" + "sete %al\n"


let compileCProgram = parseCProgram >> Result.mapError ParseError >> Result.bind generateCProgram