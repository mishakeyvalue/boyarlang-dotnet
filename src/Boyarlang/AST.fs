module Boyarlang.Compiler.AST

open System
open System.Linq.Expressions

type Everything = Declaration list

and Declaration = 
    | StaticVariableDeclaration of VariableDeclaration
    | FunctionDeclaration of FunctionDeclaration

and VariableDeclaration =
    | ZeroDimensionalVariableDeclaration of TypeDeclaration * Name
    | MultiDimensionalVariableDeclaration of TypeDeclaration * Name

and FunctionDeclaration = ReturnType * FunctionName * FunctionParameters

and ReturnType = TypeDeclaration
and FunctionName = Name
and FunctionParameters = VariableDeclaration list

and TypeDeclaration = 
    | Void
    | Bool
    | Integer
    | Float

and Statement =
    | ExpressionStatement of ExpressionStatement
    | CompoundStatement of CompoundStatement
    | IfStatement of IfStatement
    | WhileStatement of WhileStatement
    | ReturnStatement of ReturnStatement
    | BreakStatement

and ExpressionStatement = 
    | Expression of Expression
    | Nop

and CompoundStatement = LocalDeclarations * Statement list
and LocalDeclarations = VariableDeclaration list

and IfStatement = Expression * IfBody
and IfBody = Statement * Statement option

and WhileStatement = Expression * Statement

and ReturnStatement = Expression option

and Expression = 
    | ZeroDimensionalAssignmentExpression of NameReference * Expression
    | BinaryExpression of Expression * BinaryOperator * Expression
    | UnaryExpression of UnaryOperator * Expression
    | NameExpression of NameReference
    | FunctionCallExpression of Name * Arguments
    | LiteralExpression of Literal

and BinaryOperator =
    | ConditionalOr
    | Equal
    | NotEqual
    | LessEqual
    | Less
    | GreaterEqual
    | Greater
    | ConditionalAdd
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulus

and UnaryOperator = 
    | Negate

and Literal =
    | BoolLiteral of bool
    | IntegerLiteral of int
    | FloatLiteral of float

and Arguments = Expression list
and Name = string
and NameReference = { Name : string }

