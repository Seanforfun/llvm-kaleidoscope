//
// Created by sean on 29/07/19.
//

#include <string>
#include <memory>
#include <vector>
#include <map>
#include <iostream>

#include "include/llvm/ADT/STLExtras.h"

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
        std::string numStr;
        numStr += lastCharacter;
        while(isdigit(lastCharacter = getchar()) || lastCharacter == '.'){
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
    };

    class NumberExprAST: public ExprAST{
        double val;

    public:
        NumberExprAST(double val): val(val) {}
    };

    // class for variable like 'a'
    class VariableExprAST: public ExprAST{
        std::string variable;

    public:
        VariableExprAST(const std::string & variable): variable(std::move(variable)){}
    };

    class BinaryExprAST: public ExprAST{
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;

    public:
        BinaryExprAST(char op, std::unique_ptr<ExprAST> &lhs,
                std::unique_ptr<ExprAST> &rhs) :
                Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
    };

    // class for function call
    class CallExprAST: public ExprAST{
        std::string callee; // function name;
        std::vector<std::unique_ptr<ExprAST>> args;

    public:
        CallExprAST(const std::string &callee,
                const std::vector<std::unique_ptr<ExprAST>> &args) : callee(callee), args(std::move(args)) {}
    };

    class ProtoTypeAST{
        std::string name;
        std::vector<std::unique_ptr<ExprAST>> args;

    public:
        ProtoTypeAST(const std::string &name,
                const std::vector<std::unique_ptr<ExprAST>> &args) : name(name), args(std::move(args)) {}
    };

    class FunctionAST{
        std::unique_ptr<ProtoTypeAST> prototype;
        std::unique_ptr<ExprAST> body;
    public:
        FunctionAST(std::unique_ptr<ProtoTypeAST> &prototype, std::unique_ptr<ExprAST> &body) :
        prototype(std::move(prototype)), body(std::move(body)) {}
    };
}

//===-----------------------------------------------------------
// Parser
//===-----------------------------------------------------------
static int curToken;
static int getNextToken(){
    return curToken = getToken();
}

std::map<char, int> binopPrecedence; // +, -, *, <

static int getTokenPrecedence(){
    if(!isascii(curToken)) return -1;
    int tokenPrecedence = binopPrecedence[curToken];
    if(tokenPrecedence <= 0) return -1;
    return tokenPrecedence;
}

std::unique_ptr<ExprAST> logError(const std::string msg){
    std::cerr << "Error: " << msg << std::endl;
    return nullptr;
}

std::unique_ptr<ProtoTypeAST> logErrorP(const std::string msg){
    logError(msg);
    return nullptr;
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

    if(curToken != '(') return llvm::make_unique<VariableExprAST>(idName);  // a = b

    getNextToken();
    std::vector<std::unique_ptr<ExprAST>> args;
    if(curToken != ')'){ //a = (b)
        while (1){
            if(auto arg = parseExpression()){
                args.push_back(arg);
            }else return nullptr;

            if(curToken == ')') break;
            if(curToken != ',') logError("Expect ',' or ')' in argument list");
            getNextToken();
        }
    }
    getNextToken();
    return llvm::make_unique<CallExprAST>(idName, args);
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
