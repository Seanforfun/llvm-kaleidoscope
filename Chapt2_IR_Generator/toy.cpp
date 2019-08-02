//
// Created by sean on 29/07/19.
//

#include <string>
#include <memory>
#include <vector>
#include <map>
#include <iostream>
#include <cstdio>
#include <cstdlib>


#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Verifier.h"

static llvm::LLVMContext llvmContext;
static llvm::IRBuilder<> builder(llvmContext);
static std::unique_ptr<llvm::Module> theModules;
static std::map<std::string, llvm::Value*> nameValueTbl;

class LexerParseException: public std::exception{
private:
    std::string msg;
public:
    explicit LexerParseException(const std::string& msg){
        this->msg = "Lexer Exception: " + msg;
    }

    const char* what() const noexcept override {
        return this->msg.c_str();
    }
};

enum Token{
    TOKEN_EOF = -1,

    // commands
            TOKEN_DEF = -2,
    TOKEN_EXT = -3,

    //PRIMARY
            TOKEN_IDENTIFIER = -4,
    TOKEN_NUMBER = -5
};

static std::string identifierStr;   // if current token is an identifier, this variable holds the name.
static double numVal; // if current token is a number, this variable holds the value.

/**
 * Get token from standard input.
 * @return if matches enum TOKEN return its value, otherwise return the ASCII of the character.
 */
static int getToken(){
    static int lastCharacter = ' ';

    while(isspace(lastCharacter))
        lastCharacter = getchar();

    if(isalpha(lastCharacter)){ // current token an identifier
        identifierStr = lastCharacter;
        while(isalpha((lastCharacter = getchar()))){
            identifierStr += lastCharacter;
        }
        if(identifierStr == "def") return TOKEN_DEF;
        else if(identifierStr == "extern") return TOKEN_EXT;
        return TOKEN_IDENTIFIER;
    }

    if(isdigit(lastCharacter) || lastCharacter == '.'){
        int dotCount = 0;
        if(lastCharacter == '.') ++dotCount;
        std::string numStr;
        numStr += lastCharacter;
        while(isdigit(lastCharacter = getchar()) || lastCharacter == '.'){
            if(lastCharacter == '.' && dotCount){
                throw LexerParseException("Incorrect format for numerical value, mutliple '.'");
            }
            numStr += lastCharacter;
        }
        numVal = strtod(numStr.c_str(), nullptr);
        return TOKEN_NUMBER;
    }

    if(lastCharacter == '#'){
        do{
            lastCharacter = getchar();
        }while (lastCharacter != EOF && lastCharacter != '\n' && lastCharacter != '\r');

        if(lastCharacter != EOF) return getToken();
    }

    if(lastCharacter == EOF) return TOKEN_EOF;

    int lastCharASCII = lastCharacter;
    lastCharacter = getchar();
    return lastCharASCII;
}

//===-----------------------------------------------
// Abstract syntax tree
//===-----------------------------------------------
namespace {
    class ExprAST{
    public:
        virtual ~ExprAST() = default;

        virtual llvm::Value* codeGen() = 0;
    };

    class NumberExprAST: public ExprAST{
        double val;

    public:
        NumberExprAST(double val): val(val) {}

        llvm::Value* codeGen() override;
    };

    // class for variable like 'a'
    class VariableExprAST: public ExprAST{
        std::string variable;

    public:
        VariableExprAST(std::string  variable): variable(std::move(variable)){}

        llvm::Value* codeGen() override;
    };

    class BinaryExprAST: public ExprAST{
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs,
                      std::unique_ptr<ExprAST> rhs) :
                Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}

        llvm::Value* codeGen() override;
    };

    // class for function call
    class CallExprAST: public ExprAST{
        std::string callee; // function name;
        std::vector<std::unique_ptr<ExprAST>> args;

    public:
        CallExprAST(std::string callee,
                    std::vector<std::unique_ptr<ExprAST>> args) : callee(std::move(callee)), args(std::move(args)) {}

        llvm::Value* codeGen()override;
    };

    class ProtoTypeAST{
    private:
        std::string name;
        std::vector<std::string> args;

    public:
        ProtoTypeAST(std::string name,
                     std::vector<std::string> args) : name(std::move(name)), args(std::move(args)) {}

        llvm::Function* codeGen();

        const std::string& getName() const {
            return name;
        }
    };

    class FunctionAST{
        std::unique_ptr<ProtoTypeAST> prototype;
        std::unique_ptr<ExprAST> body;
    public:
        FunctionAST(std::unique_ptr<ProtoTypeAST> prototype, std::unique_ptr<ExprAST> body) :
                prototype(std::move(prototype)), body(std::move(body)) {}

        llvm::Function* codeGen();
    };

}

//===---------------------------------------------------------------
// Log module
//===---------------------------------------------------------------
std::unique_ptr<ExprAST> logError(const std::string& msg){
    fprintf(stderr, "Error: %s\n", msg.c_str());
    return nullptr;
}

std::unique_ptr<ProtoTypeAST> logErrorP(const std::string& msg){
    logError(msg);
    return nullptr;
}

llvm::Value* logErrorV(const std::string& msg){
    logError(msg);
    return nullptr;
}

//===-----------------------------------------------------------
// Parser
//===-----------------------------------------------------------
static int curToken;
static int getNextToken(){
    curToken = getToken();
    return curToken;
}

std::map<char, int> binopPrecedence; // +, -, *, <

static int getTokenPrecedence(){
    if(!isascii(curToken)) return -1;
    int tokenPrecedence = binopPrecedence[curToken];
    if(tokenPrecedence <= 0) return -1;
    return tokenPrecedence;
}

static std::unique_ptr<ExprAST> parseExpression();

static std::unique_ptr<ExprAST> parseNumberExpr(){
    auto result =  llvm::make_unique<NumberExprAST>(numVal);
    getNextToken();
    return result;
}

static std::unique_ptr<ExprAST> parsePatheneseExpr(){
    getNextToken(); // eat (

    auto v = parseExpression();
    if(!v) return nullptr;
    if(curToken != ')') logError("Expect ')'");
    getNextToken(); // eat )
    return v;
}

static std::unique_ptr<ExprAST> parseIdentifierExpr(){
    std::string idName = identifierStr;

    getNextToken(); // move to next token

    // just an identifier, like a b
    if(curToken != '(') return llvm::make_unique<VariableExprAST>(idName);

    // enter the bracket parse
    getNextToken();
    std::vector<std::unique_ptr<ExprAST>> args;
    if(curToken != ')'){ //a = (b)
        while (1){
            if(auto arg = parseExpression()){
                args.push_back(std::move(arg));
            }else return nullptr;

            if(curToken == ')') break;
            if(curToken != ',') logError("Expect ',' or ')' in argument list");
            getNextToken();
        }
    }
    getNextToken();
    return llvm::make_unique<CallExprAST>(idName, std::move(args));
}

static std::unique_ptr<ExprAST> parsePrimary(){
    switch (curToken){
        case TOKEN_IDENTIFIER:
            return parseIdentifierExpr();
        case TOKEN_NUMBER:
            return parseNumberExpr();
        case '(':
            return parsePatheneseExpr();
        default:
            return logError("Unknown token");
    }
}

//===----------------------------------------------------
// binary expression parsing
//===----------------------------------------------------
static std::unique_ptr<ExprAST> parseBinaryOpExpressionRHS(int, std::unique_ptr<ExprAST>);

/**
 * Parse costumized expressions
 * @return
 */
static std::unique_ptr<ExprAST> parseExpression(){
    std::unique_ptr<ExprAST> LHS = parsePrimary();
    if(!LHS) return nullptr;

    return parseBinaryOpExpressionRHS(0, std::move(LHS));
}

/**
 *  Create a binary operation AST.
 * @param opPrec    precedence of previous operator.
 * @param LHS  LHS AST of current binary operation
 * @return  BinaryOpExprAST pointer.
 */
static std::unique_ptr<ExprAST> parseBinaryOpExpressionRHS(int opPrec, std::unique_ptr<ExprAST> LHS){
    while (true){
        int curPrec = getTokenPrecedence(); // precedence of current operator.

        if(curPrec < opPrec) return LHS;    // This is the end of the expression. Current token is not an operator.

        int curOp = curToken;
        getNextToken(); // skip current operator.

        std::unique_ptr<ExprAST> RHS = parsePrimary();
        if(!RHS) return nullptr;

        int nextPrec = getTokenPrecedence();
        if(curPrec < nextPrec){
            RHS = parseBinaryOpExpressionRHS(curPrec + 1, std::move(RHS));
            if(RHS) return nullptr;
        }

        LHS = llvm::make_unique<BinaryExprAST>(curOp, std::move(LHS), std::move(RHS));
    }
}

static std::unique_ptr<ProtoTypeAST> parsePrototype(){
    if(curToken != TOKEN_IDENTIFIER) return logErrorP("Expect a function name in prototype");
    std::string functionName = identifierStr;
    getNextToken();

    if(curToken != '(') return logErrorP("Expect '(' in prototype");
    std::vector<std::string> args;
    int cur  = getNextToken();
    while(cur == TOKEN_IDENTIFIER){
        args.push_back(identifierStr);
        cur = getNextToken();
    }
    if(curToken != ')') return logErrorP("Expect ')' matching '('");
    getNextToken();
    return llvm::make_unique<ProtoTypeAST>(functionName, std::move(args));
}

static std::unique_ptr<FunctionAST> parseDefinition(){
    getNextToken(); //skip token 'def'

    auto proto = parsePrototype();
    if(!proto) return nullptr;

    auto body = parseExpression();
    return llvm::make_unique<FunctionAST>(std::move(proto), std::move(body));
}

static std::unique_ptr<ProtoTypeAST> parseExtern(){
    getNextToken(); // skip "extern"
    return parsePrototype();
}

static std::unique_ptr<FunctionAST> parseTopLevel(){
    if(auto body = parseExpression()){
        std::vector<std::string> v;
        auto proto = llvm::make_unique<ProtoTypeAST>("", std::move(v));
        return llvm::make_unique<FunctionAST>(std::move(proto), std::move(body));
    }
    return nullptr;
}

static void definitionHandler(){
    if(auto pdef = parseDefinition()){
        // TODO
        if(auto* ir = pdef->codeGen()){
            fprintf(stderr, "Parsed a function definition.\n");
            ir->print(llvm::errs());
            fprintf(stderr, "\n");
        }
    }else if(curToken == ';') return;
    else getNextToken();
}

static void externHandler(){
    if(auto pExtern = parseExtern()){
        if(auto* ir = pExtern->codeGen()){
            fprintf(stderr, "Parsed a extern.\n");
            ir->print(llvm::errs());
            fprintf(stderr, "\n");
        }
    }else if(curToken == ';') return;
    else getNextToken();
}

static void topLevelHandler(){
    if(auto pTop = parseTopLevel()) {
        if (auto* ir = pTop->codeGen()) {
            fprintf(stderr, "Parsed a top-level expression.\n");
            ir->print(llvm::errs());
            fprintf(stderr, "\n");
        }
    }
    else getNextToken();
}

//===--------------------------------------------------------------------------
// Code Generator
//===--------------------------------------------------------------------------

llvm::Value* NumberExprAST::codeGen() {
    return llvm::ConstantFP::get(llvmContext, llvm::APFloat(val));
}

llvm::Value *VariableExprAST::codeGen() {
    llvm::Value* v = nameValueTbl[variable];
    if(!v) return logErrorV("Unknown variable name!");
    return v;
}

llvm::Value *BinaryExprAST::codeGen() {
    llvm::Value* l = LHS->codeGen();
    llvm::Value* r = RHS->codeGen();

    if(!l || !r) return nullptr;

    switch (Op){
        case '<':
            l = builder.CreateFCmpULT(l, r, "tmpCmp");
            return builder.CreateUIToFP(l, llvm::Type::getDoubleTy(llvmContext), "tmpBool");
        case '+':
            return builder.CreateFAdd(l, r, "tmpAdd");
        case '-':
            return builder.CreateFSub(l, r, "tmpSub");
        case '*':
            return builder.CreateFMul(l, r, "tmpMul");
        default:
            logErrorV("Invalid Operator!");
    }
    return nullptr;
}

llvm::Value* CallExprAST::codeGen(){
    //1. Check if function exist
    llvm::Function* function = theModules->getFunction(callee);
    if(!function) return logErrorV("No function" + callee + "!");

    //2. Check if number of args matches
    if(function->arg_size() != args.size()) return logErrorV("Argument number mismatch!");

    //3. Add values to SSR
    std::vector<llvm::Value*> argsVect;
    for(auto & arg : args){
        argsVect.push_back(arg->codeGen());
    }
    return builder.CreateCall(function, argsVect, "tmpCall");
}

llvm::Function *ProtoTypeAST::codeGen() {
    // 1. Create a variable type vector, all variables in prototype can only be double.
    std::vector<llvm::Type*> argsVec(args.size(), llvm::Type::getDoubleTy(llvmContext));

    // 2. Create define a function type
    // return type is double
    // pass variables' type
    // no vararg, no int... a kind variable
    llvm::FunctionType* functionType = llvm::FunctionType::get(llvm::Type::getDoubleTy(llvmContext), argsVec, false);

    // 3. Register function type(parameters' type, return type), allow external linkage, function name to module.
    llvm::Function* function = llvm::Function::Create(functionType, llvm::GlobalValue::ExternalLinkage, name, theModules.get());

    // 4. Set readable name for all variables in prototype.
    unsigned index = 0;
    for(auto& arg : function->args()){
        arg.setName(args[index++]);
    }
    return function;
}

llvm::Function *FunctionAST::codeGen() {
    // 1. Create Prototype
    llvm::Function* function = theModules->getFunction(prototype->getName());
    if(!function){
        function = prototype->codeGen();
    }
    if(!function) return nullptr;
    // Check if current function already has body.
    if(!function->empty()) return (llvm::Function*)logErrorV("Redefine function!");

    // 2. Create code body.
    llvm::BasicBlock* basicBlock = llvm::BasicBlock::Create(llvmContext, "entry", function);
    builder.SetInsertPoint(basicBlock);

    nameValueTbl.clear();
    for(auto& args: function->args()){
        nameValueTbl[args.getName()] = &args;
    }

    if(llvm::Value* ret = body->codeGen()){
        builder.CreateRet(ret);
        llvm::verifyFunction(*function);
        return function;
    }

    function->eraseFromParent();
    return nullptr;
}

static void MainLoop(){
    while(true){
        fprintf(stderr, "Ready> ");
        getNextToken();
        switch (curToken){
            case TOKEN_EOF: return;
            case TOKEN_DEF:
                definitionHandler();
                break;
            case TOKEN_EXT:
                externHandler();
                break;
            case ';':
                continue;
            default:
                topLevelHandler();
                break;
        }
    }
}

int main(){
    binopPrecedence['<'] = 10;
    binopPrecedence['+'] = 20;
    binopPrecedence['-'] = 20;
    binopPrecedence['*'] = 40;
    std::cout << "Start> " << std::endl;
    theModules = llvm::make_unique<llvm::Module>("Seanforfun", llvmContext);
    MainLoop();
    theModules->print(llvm::errs(), nullptr);
    return 0;
}