grammar LogicPL;

@header{
import ast.node.*;
import ast.node.expression.*;
import ast.node.statement.*;
import ast.node.declaration.*;
import ast.node.expression.values.*;
import ast.node.expression.operators.*;
import ast.type.primitiveType.*;
import ast.type.*;
}

program returns[Program p]:
    {$p = new Program(); $p.setLine(0);}
    (f = functionDec {$p.addFunc($f.functionDeclaration);})*
    main = mainBlock {$p.setMain($main.main) ;}
    ;

functionDec returns[FuncDeclaration functionDeclaration]:
    {ArrayList<ArgDeclaration> args = new ArrayList<>();
     ArrayList<Statement> statements = new ArrayList<>();}
    FUNCTION name = identifier
    LPAR (arg1 = functionVarDec {args.add($arg1.argDeclaration);} (COMMA arg = functionVarDec {args.add($arg.argDeclaration);})*)? RPAR COLON returnType = type
    LBRACE ((stmt = statement {statements.add($stmt.statementRet);})+) RBRACE
    {$functionDeclaration = new FuncDeclaration($name.identifierRet, $returnType.typeRet, args, statements); $functionDeclaration.setLine($name.identifierRet.getLine());}
    ;

functionVarDec returns [ArgDeclaration argDeclaration]:
    t = type arg_iden = identifier {$argDeclaration = new ArgDeclaration($arg_iden.identifierRet, $t.typeRet); $argDeclaration.setLine($arg_iden.identifierRet.getLine());}
    ;

mainBlock returns [MainDeclaration main]:
    {ArrayList<Statement> mainStmts = new ArrayList<>();}
    m = MAIN LBRACE (s = statement {mainStmts.add($s.statementRet);})+ RBRACE
    {$main = new MainDeclaration(mainStmts); $main.setLine($m.getLine());}
    ;

statement:
    assignSmt | ( predicate SEMICOLON )
    | implication | returnSmt
    | printSmt | forLoop | localVarDeclaration
    ;

assignSmt:
    variable ASSIGN expression SEMICOLON
    ;

variable returns[Variable var]:
    i = identifier {$var = $i.identifierRet;}
    | i = identifier LBRACKET e = expression RBRACKET
    {$var = new ArrayAccess($i.identifierRet.getName(),$e.expr);}
    ;

localVarDeclaration:
     varDeclaration
    | arrayDeclaration
    ;

varDeclaration:
    type identifier (ASSIGN expression )? SEMICOLON
    ;

arrayDeclaration:
    type LBRACKET INT_NUMBER RBRACKET identifier
    (arrayInitialValue )? SEMICOLON
    ;

arrayInitialValue:
    ASSIGN arrayList
    ;

arrayList:
    LBRACKET ( value | identifier ) (COMMA ( value | identifier ))* RBRACKET
    ;

printSmt:
    PRINT LPAR printExpr RPAR SEMICOLON
    ;

printExpr:
    variable
    | query
    ;

query:
      queryType1
     | queryType2
    ;

queryType1 returns [QueryExpression qexp]:
    LBRACKET QUARYMARK id = predicateIdentifier LPAR var = variable RPAR RBRACKET
    {$qexp = new QueryExpression($id.identifierRet);
     $qexp.setVar($var.var);
    }
    ;

queryType2:
    LBRACKET predicateIdentifier LPAR QUARYMARK RPAR RBRACKET
    ;

returnSmt:
    RETURN (value  | identifier)? SEMICOLON
    ;

forLoop:
    FOR LPAR identifier COLON identifier RPAR
    LBRACE ((statement)*) RBRACE
    ;

predicate:
    predicateIdentifier LPAR variable RPAR
    ;

implication:
    LPAR expression RPAR ARROW LPAR ((statement)+) RPAR
    ;

expression returns [Expression expr]:
    l = andExpr r = expression2
    {$expr = new BinaryExpression($l.expr,$r.expr.getLeft(),$r.expr.getBinaryOperator());}
    ;

expression2  returns[BinaryExpression expr]:
    {BinaryOperator bop = new BinaryOperator();}
    OR {bop = BinaryOperator.or;}
    (left = andExpr) (right = expression2)
    {$expr = new BinaryExpression($left.expr,$right.expr,bop);}
    |{$expr = new BinaryExpression();}
    ;

andExpr returns[Expression expr]:
    l = eqExpr r = andExpr2
    {$expr = new BinaryExpression($l.expr,$r.expr.getLeft(),$r.expr.getBinaryOperator());}
    ;

andExpr2 returns[BinaryExpression expr]:
    {BinaryOperator bop = new BinaryOperator();}
    AND {bop = BinaryOperator.and;}
    (left = eqExpr) (right = andExpr2)
    {$expr = new BinaryExpression($left.expr,$right.expr,bop);}
    |{$expr = new BinaryExpression();}
    ;

eqExpr returns[Expression expr]:
    l = compExpr  r = eqExpr2
    {$expr = new BinaryExpression($l.expr,$r.expr.getLeft(),$r.expr.getBinaryOperator());}
    ;

eqExpr2 returns[BinaryExpression expr]:
    {BinaryOperator bop = new BinaryOperator();}
    ( EQ {bop = BinaryOperator.eq;}
    | NEQ {bop = BinaryOperator.neq;}
    )
    (left = compExpr) (right = eqExpr2)
    {$expr = new BinaryExpression($left.expr,$right.expr,bop);}
    |{$expr = new BinaryExpression();}
    ;

compExpr returns[Expression expr]:
   l = additive r = compExpr2
   {$expr = new BinaryExpression($l.expr,$r.expr.getLeft(),$r.expr.getBinaryOperator());}
    ;

compExpr2 returns[BinaryExpression expr]:
    {BinaryOperator bop = new BinaryOperator();}
    ( LT {bop = BinaryOperator.lt;}
    | LTE {bop = BinaryOperator.lte;}
    | GT {bop = BinaryOperator.gt;}
    | GTE {bop = BinaryOperator.gte;}
    )
    (left =additive) (right = compExpr2)
    {$expr = new BinaryExpression($left.expr,$right.expr,bop);}
    |{$expr = new BinaryExpression();}
    ;

additive returns[Expression expr]:
    l = multicative  r = additive2
    {$expr = new BinaryExpression($l.expr,$r.expr.getLeft(),$r.expr.getBinaryOperator());}
    ;

additive2 returns[BinaryExpression expr]:
    {BinaryOperator bop = new BinaryOperator();}
    ( PLUS {bop = BinaryOperator.add;}
    | MINUS {bop = BinaryOperator.sub;}
    )
    (left =multicative)  (right = additive2)
    {$expr = new BinaryExpression($left.expr,$right.expr,bop);}
    |{$expr = new BinaryExpression();}
    ;

multicative  returns[Expression expr]:
    l = unary r = multicative2
    {$expr = new BinaryExpression($l.expr,$r.expr.getLeft(),$r.expr.getBinaryOperator());}
    ;

multicative2 returns[BinaryExpression expr]:
     {BinaryOperator bop = new BinaryOperator();}
     ( MULT {bop = BinaryOperator.mult;}
     | MOD  {bop = BinaryOperator.mod;}
     | DIV  {bop = BinaryOperator.div;}
     )
     (left = unary) (right = multicative2)
     {$expr = new BinaryExpression($left.expr,$right.expr,bop);}

    | {$expr = new BinaryExpression();}
    ;

unary returns[Expression expr]:
    ex = other {$expr = $ex.expr;}
    |
     {UnaryOperator uop = new UnaryOperator();}
     ( PLUS {uop = UnaryOperator.plus;}
     | MINUS {uop = UnaryOperator.minus;}
     | NOT {uop = UnaryOperator.not;}
     )
    ex = other {$expr = new UnaryExpression(uop,$ex.expr);}
    ;

other returns[Expression expr]:
    LPAR e = expression RPAR {$expr = $e.expr;}
    | evar = variable {$expr = $evar.var;}
    | eval = value {$expr = $eval.val;}
    | equery = queryType1 {$expr = $equery.qexp;}
    | ef = functionCall {$expr = $ef.funcall;}
    ;

functionCall returns[FunctionCall funcall]:
    { ArrayList<Expression> my_args = new ArrayList<Expression>();}
    id = identifier LPAR (e1 = expression {my_args.add($e1.expr)}
    (COMMA e2 = expression {my_args.add($e2.expr)})*)? RPAR
    {$funcall = new FunctionCall(my_args,$id.identifierRet);}
    ;

value returns[Value val]:
    v = numericValue {$val = $v.val;}
    | TRUE {$val = new BooleanValue(true); $val.setType(new BooleanType());}
    | FALSE {$val = new BooleanValue(false); $val.setType(new BooleanType());}
    | MINUS v = numericValue {$val = $v.val; }
    ;

numericValue returns[Value val]:
    INT_NUMBER {$val = new IntValue(Integer.parseInt($INT_NUMBER.getText()));
                $val.setType(new IntType());}
    | FLOAT_NUMBER {$val = new FloatValue(Float.parseFloat($FLOAT_NUMBER.getText()));
                    $val.setType(new FloatType());}
    ;

identifier returns[Identifier identifierRet]:
    IDENTIFIER {$identifierRet = new Identifier($IDENTIFIER.getText());}
    ;

predicateIdentifier returns[Identifier identifierRet]:
    PREDICATE_IDENTIFIER {$identifierRet = new Identifier($PREDICATE_IDENTIFIER.getText());}
    ;

type returns [Type t]:
    BOOLEAN {$t = new BooleanType();}
    | INT {$t = new IntType();}
    | FLOAT {$t = new FloatType();}
    ;




FUNCTION : 'function';
BOOLEAN : 'boolean';
INT : 'int';
FLOAT: 'float';
MAIN: 'main';
PRINT: 'print';
RETURN: 'return';
FOR: 'for';
TRUE: 'true';
FALSE: 'false';

LPAR: '(';
RPAR: ')';
COLON: ':';
COMMA: ',';
LBRACE: '{';
RBRACE: '}';
SEMICOLON: ';';
ASSIGN: '=';
LBRACKET: '[';
RBRACKET: ']';
QUARYMARK: '?';
ARROW: '=>';
OR: '||';
AND: '&&';
EQ: '==';
GT: '>';
LT: '<';
GTE: '>=';
LTE: '<=';
PLUS: '+';
MINUS: '-';
MULT: '*';
DIV: '/';
MOD: '%';
NEQ: '!=';
NOT: '!';


WS : [ \t\r\n]+ -> skip ;
COMMENT : '#' ~[\r\n]* -> skip ;

IDENTIFIER : [a-z][a-zA-Z0-9_]* ;
PREDICATE_IDENTIFIER : [A-Z][a-zA-Z0-9]* ;
INT_NUMBER : [0-9]+;
FLOAT_NUMBER: ([0-9]*[.])?[0-9]+;