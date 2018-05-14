module Boyarlang.Compiler.Parser

open AST
open PigletWrappers
open Piglet.Parser
    
let private configurator = ParserFactory.Configure<obj>()

module Vocabulary = 
    
    let terminalParse<'T> regex (onParse : (string -> 'T)) =
        new TerminalWrapper<'T>(configurator.CreateTerminal(regex, (fun s -> box (onParse s))))

    let terminal regex =
        new TerminalWrapper<string>(configurator.CreateTerminal(regex))

    let ifKeyword = terminal "коль"
    let elseKeyword = terminal "else"
    let whileKeyword = terminal "while"
    let returnKeyword    = terminal      "return"
    let breakKeyword     = terminal      "break"
    let newKeyword       = terminal      "new"
    let sizeKeyword      = terminal      "size"
    let voidKeyword      = terminal      "void"
    let plus             = terminal      @"\+"

    let minus = terminal "-"
    let exclamation = terminal "!"
    let asterisk         = terminal      @"\*"

    let integerLiteral = terminalParse @"\d+" (fun s -> AST.IntegerLiteral (int32 s))
    let trueLiteral = terminalParse "true" (fun _ -> AST.BoolLiteral true)
    let falseLiteral = terminalParse "false" (fun _ -> AST.BoolLiteral false)
    
    let integerKeyword = terminalParse "цѣл" (fun _ -> AST.Integer)
    let floatKeyword = terminalParse "float" (fun _ -> AST.Float)
    let boolKeyword = terminalParse "bool" (fun _ -> AST.Bool)

    let name = terminalParse "[a-z_][a-z_0-9]*" id

    let semicolon = terminal ";"
    let comma = terminal ","
    let openParen        = terminal      @"\("
    let closeParen       = terminal      @"\)"
    let openCurly        = terminal      @"\{"
    let closeCurly       = terminal      @"\}"
    let openSquare       = terminal      @"\["
    let closeSquare      = terminal      @"\]"
    let percent          = terminal      "%"
    let forwardSlash     = terminal      "/"
    let singleEquals     = terminal      "="
    let doublePipes      = terminal      @"\|\|"
    let doubleEquals     = terminal      "=="
    let bangEquals       = terminal      "!="
    let openAngleEquals  = terminal      "<="
    let openAngle        = terminal      "<"
    let closeAngleEquals = terminal      ">="
    let closeAngle       = terminal      ">"
    let doubleAmpersands = terminal      "&&"
    let period           = terminal      @"\."



let nonTerminal<'T> () = new NonTerminalWrapper<'T>(configurator.CreateNonTerminal())


let everything = nonTerminal<Everything>()
let declarationList = nonTerminal<Declaration list>()
let declaration = nonTerminal<Declaration>()
let variableDeclaration = nonTerminal<VariableDeclaration>()
let functionDeclaration = nonTerminal<FunctionDeclaration>()
let typeDeclaration = nonTerminal<TypeDeclaration>()
let returnType = nonTerminal<ReturnType>()
let parameters                = nonTerminal<FunctionParameters>()
let parameter                 = nonTerminal<VariableDeclaration>()
let optionalStatementList     = nonTerminal<Statement list>()
let statementList             = nonTerminal<Statement list>()
let statement                 = nonTerminal<Statement>()
let expressionStatement       = nonTerminal<ExpressionStatement>()
let whileStatement            = nonTerminal<WhileStatement>()
let compoundStatement         = nonTerminal<CompoundStatement>()
let optionalLocalDeclarations = nonTerminal<VariableDeclaration list>()
let localDeclarations         = nonTerminal<VariableDeclaration list>()
let localDeclaration          = nonTerminal<VariableDeclaration>()
let ifStatement               = nonTerminal<IfStatement>()
let optionalElseStatement     = nonTerminal<Statement option>()
let returnStatement           = nonTerminal<Expression option>()
let breakStatement            = nonTerminal<unit>()
let expression                = nonTerminal<Expression>()
let unaryOperator             = nonTerminal<UnaryOperator>()
let optionalArguments         = nonTerminal<Arguments>()
let arguments                 = nonTerminal<Arguments>()

// Precedence

let optionalElsePrecendenceGroup = configurator.LeftAssociative()

open Vocabulary
configurator.LeftAssociative(downcast elseKeyword.Symbol) |> ignore
configurator.LeftAssociative(downcast singleEquals.Symbol) |> ignore
configurator.LeftAssociative(downcast doublePipes.Symbol) |> ignore
configurator.LeftAssociative(downcast doubleEquals.Symbol, downcast bangEquals.Symbol) |> ignore
configurator.LeftAssociative(downcast openAngleEquals.Symbol,
                             downcast openAngle.Symbol,
                             downcast closeAngleEquals.Symbol,
                             downcast closeAngle.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast doubleAmpersands.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast exclamation.Symbol,
                             downcast plus.Symbol,
                             downcast minus.Symbol)
                             |> ignore
configurator.LeftAssociative(downcast asterisk.Symbol,
                             downcast forwardSlash.Symbol,
                             downcast percent.Symbol)
                             |> ignore

everything.AddProduction(declarationList).SetReduceToFirst()

declarationList.AddProduction(declarationList, declaration).SetReduceFunction (fun a b -> a @ [b])
declarationList.AddProduction(declaration).SetReduceFunction (fun a -> [a])

declaration.AddProduction(variableDeclaration).SetReduceFunction (fun a -> AST.StaticVariableDeclaration a)
declaration.AddProduction(functionDeclaration)      .SetReduceFunction (fun a -> AST.FunctionDeclaration a)

typeDeclaration.AddProduction(voidKeyword).SetReduceFunction (fun _ -> AST.Void)
typeDeclaration.AddProduction(boolKeyword).SetReduceToFirst()
typeDeclaration.AddProduction(integerKeyword).SetReduceToFirst()
typeDeclaration.AddProduction(floatKeyword).SetReduceToFirst()

variableDeclaration.AddProduction(typeDeclaration, name, semicolon)
    .SetReduceFunction (fun a b _ -> AST.ZeroDimensionalVariableDeclaration(a, b))
variableDeclaration.AddProduction(typeDeclaration, name, openSquare, closeSquare, semicolon)
    .SetReduceFunction (fun a b _ _ _ -> AST.MultiDimensionalVariableDeclaration(a, b))

//functionDeclaration.AddProduction(typeDeclaration, name, openParen, parameters, closeParen, compoundStatement)
//    .SetReduceFunction (fun a b _ d _ f -> (a, b, d, f))

parameters.AddProduction(parameters).SetReduceToFirst()
parameters.AddProduction(voidKeyword).SetReduceFunction (fun _ -> [])

parameters.AddProduction(parameters, comma, parameter).SetReduceFunction (fun a _ c -> a @ [c])
parameters.AddProduction(parameter)                      .SetReduceFunction (fun a -> [a])

parameter.AddProduction(typeDeclaration, name)                         .SetReduceFunction (fun a b -> AST.ZeroDimensionalVariableDeclaration(a, b))
parameter.AddProduction(typeDeclaration, name, openSquare, closeSquare).SetReduceFunction (fun a b _ _ -> AST.MultiDimensionalVariableDeclaration(a, b))

optionalStatementList.AddProduction(statementList).SetReduceToFirst()
optionalStatementList.AddProduction()             .SetReduceFunction (fun () -> [])

statementList.AddProduction(statementList, statement).SetReduceFunction (fun a b -> a @ [b])
statementList.AddProduction(statement)               .SetReduceFunction (fun a -> [a])

statement.AddProduction(expressionStatement).SetReduceFunction (fun a -> AST.ExpressionStatement a)
statement.AddProduction(compoundStatement)  .SetReduceFunction (fun a -> AST.CompoundStatement a)
statement.AddProduction(ifStatement)        .SetReduceFunction (fun a -> AST.IfStatement a)
statement.AddProduction(whileStatement)     .SetReduceFunction (fun a -> AST.WhileStatement a)
statement.AddProduction(returnStatement)    .SetReduceFunction (fun a -> AST.ReturnStatement a)
statement.AddProduction(breakStatement)     .SetReduceFunction (fun a -> AST.BreakStatement)

expressionStatement.AddProduction(expression, semicolon).SetReduceFunction (fun a _ -> AST.Expression a)
expressionStatement.AddProduction(semicolon)            .SetReduceFunction (fun _ -> AST.Nop)

whileStatement.AddProduction(whileKeyword, openParen, expression, closeParen, statement)
    .SetReduceFunction (fun a b c d e -> (c, e))

compoundStatement.AddProduction(openCurly, optionalLocalDeclarations, optionalStatementList, closeCurly)
    .SetReduceFunction (fun _ b c _ -> (b, c))

optionalLocalDeclarations.AddProduction(localDeclarations).SetReduceToFirst()
optionalLocalDeclarations.AddProduction()                 .SetReduceFunction (fun () -> [])

localDeclarations.AddProduction(localDeclarations, localDeclaration).SetReduceFunction (fun a b -> a @ [b])
localDeclarations.AddProduction(localDeclaration)                   .SetReduceFunction (fun a -> [a])

localDeclaration.AddProduction(typeDeclaration, name, semicolon)                         .SetReduceFunction (fun a b _ -> AST.ZeroDimensionalVariableDeclaration(a, b))
localDeclaration.AddProduction(typeDeclaration, name, openSquare, closeSquare, semicolon).SetReduceFunction (fun a b _ _ _ -> AST.MultiDimensionalVariableDeclaration(a, b))

//ifStatement.AddProduction(ifKeyword, openParen, expression, closeParen, statement, optionalElseStatement)
//    .SetReduceFunction (fun _ _ c _ e f -> (c, e, f))

let elseStatementProduction = optionalElseStatement.AddProduction(elseKeyword, statement)
elseStatementProduction.SetReduceFunction (fun _ b -> Some b)
//elseStatementProduction.SetPrecedence optionalElsePrecedenceGroup

let elseEpsilonProduction = optionalElseStatement.AddProduction()
elseEpsilonProduction.SetReduceFunction (fun () -> None)
//elseEpsilonProduction.SetPrecedence optionalElsePrecedenceGroup

returnStatement.AddProduction(returnKeyword, expression, semicolon).SetReduceFunction (fun _ b _ -> Some b)
returnStatement.AddProduction(returnKeyword, semicolon)            .SetReduceFunction (fun _ _ -> None)

breakStatement.AddProduction(breakKeyword, semicolon).SetReduceFunction (fun _ _ -> ())

expression.AddProduction(name, singleEquals, expression)
    .SetReduceFunction (fun a _ c -> AST.ZeroDimensionalAssignmentExpression({ Name = a }, c))
//expression.AddProduction(identifier, openSquare, expression, closeSquare, singleEquals, expression)
//    .SetReduceFunction (fun a _ c _ _ f -> AST.ArrayAssignmentExpression({ Identifier = a }, c, f))

expression.AddProduction(expression, doublePipes, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.ConditionalOr, c))
expression.AddProduction(expression, doubleEquals, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Equal, c))
expression.AddProduction(expression, bangEquals, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.NotEqual, c))
expression.AddProduction(expression, openAngleEquals, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.LessEqual, c))
expression.AddProduction(expression, openAngle, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Less, c))
expression.AddProduction(expression, closeAngleEquals, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.GreaterEqual, c))
expression.AddProduction(expression, closeAngle, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Greater, c))
expression.AddProduction(expression, doubleAmpersands, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.ConditionalAdd, c))
expression.AddProduction(expression, plus, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Add, c))
expression.AddProduction(expression, minus, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Subtract, c))
expression.AddProduction(expression, asterisk, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Multiply, c))
expression.AddProduction(expression, forwardSlash, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Divide, c))
expression.AddProduction(expression, percent, expression).SetReduceFunction (fun a _ c -> AST.BinaryExpression(a, AST.Modulus, c))

let unaryExpressionProduction = expression.AddProduction(unaryOperator, expression)
unaryExpressionProduction.SetReduceFunction (fun a b -> AST.UnaryExpression(a, b))
//unaryExpressionProduction.SetPrecedence unaryExpressionPrecedenceGroup

expression.AddProduction(openParen, expression, closeParen).SetReduceFunction (fun _ b _ -> b)

expression.AddProduction(name).SetReduceFunction (fun a -> AST.NameExpression({ Name = a }))
expression.AddProduction(name, openSquare, expression, closeSquare)
    //.SetReduceFunction (fun a _ c _ -> AST.ArrayIdentifierExpression({ Identifier = a }, c))
//expression.AddProduction(name, openParen, optionalArguments, closeParen)
//    .SetReduceFunction (fun a _ c _ -> AST.FunctionCallExpression(a, c))
//expression.AddProduction(name, period, sizeKeyword)
//    .SetReduceFunction (fun a _ _ -> AST.ArraySizeExpression { Identifier = a })

expression.AddProduction(trueLiteral) .SetReduceFunction (fun a -> AST.LiteralExpression a)
expression.AddProduction(falseLiteral).SetReduceFunction (fun a -> AST.LiteralExpression a)
expression.AddProduction(integerLiteral)  .SetReduceFunction (fun a -> AST.LiteralExpression a)
//expression.AddProduction(floatLi).SetReduceFunction (fun a -> AST.LiteralExpression a)

//expression.AddProduction(newKeyword, typeDeclaration, openSquare, expression, closeSquare)
//    .SetReduceFunction (fun _ b _ d _ -> AST.ArrayAllocationExpression(b, d))

//unaryOperator.AddProduction(exclamation).SetReduceFunction (fun a -> AST.)
unaryOperator.AddProduction(minus)      .SetReduceFunction (fun a -> AST.Negate)
//unaryOperator.AddProduction(plus)       .SetReduceFunction (fun a -> AST.Identity)

optionalArguments.AddProduction(arguments).SetReduceToFirst()
optionalArguments.AddProduction()         .SetReduceFunction (fun () -> [])

arguments.AddProduction(arguments, comma, expression).SetReduceFunction (fun a _ c -> a @ [c])
arguments.AddProduction(expression)                  .SetReduceFunction (fun a -> [a])

// Ignore whitespace and comments
configurator.LexerSettings.Ignore <- [| @"\s+"; @"/\*[^(\*/)]*\*/"; @"//[^\n]*\n" |]

let parser = configurator.CreateParser()

let parse (s: string) : Everything =
    try
        parser.Parse(s) :?> Everything
    with
        | ex -> raise ex