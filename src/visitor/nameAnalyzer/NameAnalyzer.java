package visitor.nameAnalyzer;

import ast.node.Program;
import ast.node.declaration.*;
import ast.node.statement.VarDecStmt;
import compileError.*;
import compileError.Name.*;
import symbolTable.SymbolTable;
import symbolTable.symbolTableItems.*;
import symbolTable.itemException.ItemAlreadyExistsException;
import symbolTable.symbolTableItems.VariableItem;
import visitor.Visitor;

import java.util.ArrayList;

public class NameAnalyzer extends Visitor<Void> {

    public ArrayList<CompileError> nameErrors = new ArrayList<>();

    @Override
    public Void visit(Program program) {
        SymbolTable.root = new SymbolTable();
        SymbolTable.push(SymbolTable.root);

        for (FuncDeclaration functionDeclaration : program.getFuncs()) {
            functionDeclaration.accept(this);
        }

        for (var stmt : program.getMain().getMainStatements()) {
            if (stmt instanceof VarDecStmt) {
                stmt.accept(this);
            }
        }

        return null;
    }

    @Override
    public Void visit(FuncDeclaration funcDeclaration) {
        var functionItem = new FunctionItem(funcDeclaration);
        var functionSymbolTable = new SymbolTable(SymbolTable.top, funcDeclaration.getName().getName());
        functionItem.setFunctionSymbolTable(functionSymbolTable);
        functionItem.setFunctionDeclaration(funcDeclaration);
        functionItem.setName(funcDeclaration.getName().getName());

        funcDeclaration.setArgs(funcDeclaration.getArgs());
        funcDeclaration.setStatements(funcDeclaration.getStatements());
        funcDeclaration.setIdentifier(funcDeclaration.getIdentifier());

        try {
            SymbolTable.root.put(functionItem);
        } catch (ItemAlreadyExistsException e) {
            FunctionRedefinition functionRedefinition = new FunctionRedefinition(funcDeclaration.getLine(),
                    funcDeclaration.getName().getName());

            this.nameErrors.add(functionRedefinition);
        }
        SymbolTable symbolTable = new SymbolTable();
        symbolTable.pre = SymbolTable.top;
        functionItem.setFunctionSymbolTable(symbolTable);
        SymbolTable.push(symbolTable);

        if (funcDeclaration.getName() != null)
            funcDeclaration.getName().accept(this);

        if (funcDeclaration.getIdentifier() != null)
            funcDeclaration.getIdentifier().accept(this);

        for (ArgDeclaration varDeclaration : funcDeclaration.getArgs()) {
            varDeclaration.accept(this);
        }

        for (var stmt : funcDeclaration.getStatements()) {
            if (stmt instanceof VarDecStmt) {
                stmt.accept(this);
            }
        }

        SymbolTable.pop();
        return null;
    }

    @Override
    public Void visit(VarDecStmt varDeclaration) {
        var variableItem = new VariableItem(varDeclaration.getIdentifier().getName(), varDeclaration.getType());
        variableItem.setName(varDeclaration.getIdentifier().getName());
        variableItem.setType(varDeclaration.getType());
        variableItem.setVarDeclaration(varDeclaration);

        try {
            SymbolTable.root.put(variableItem);
        } catch (ItemAlreadyExistsException e) {
            VariableRedefinition variableRedefinition = new VariableRedefinition(varDeclaration.getLine(),
                    varDeclaration.getIdentifier().getName());
            this.nameErrors.add(variableRedefinition);

        }
        if (varDeclaration.getIdentifier() != null)
            varDeclaration.getIdentifier().accept(this);

        if (varDeclaration.getInitialExpression() != null)
            varDeclaration.getInitialExpression().accept(this);

        return null;
    }
}
