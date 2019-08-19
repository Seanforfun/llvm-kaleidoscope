## User Defined Operator

This chapter is a demo to user defined operators. Here we following the guidance from [llvm](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl06.html) and try to implement binary and unary operators.

Please read this file with [code](https://github.com/Seanforfun/llvm-kaleidoscope/tree/master/Chapt5_User_Define_Op).

### Steps to add operators
1. Lexical: 
    * Add token and parse token
    ```objectivec
     // user defined operator
    TOKEN_UNARY = -11,
    TOKEN_BINARY = -12
    ```
    
    * parse token
    ```objectivec
    else if(identifierStr == "unary") {
        return TOKEN_UNARY;
    }else if(identifierStr == "binary") {
        return TOKEN_BINARY;
    }
    ```
    
2. Update the AST from Prototype AST
    * format:
    
        ```objectivec
        1. unary operator: define operator working on single variable, like !v(take bool reverse).
        def unary!(v) if v then 0 else 1;
        2. binary operator: define operator working on two varibales, like a | b(logic or).
        def binary| 10(LHS RHS) if LHS then 1 else if RHS then 1 else 0;
        ```
    
    * Regex:
        1. unary: ```def unary(.)([a-z]) (body);```
        2. binary: ```def binary(.) (precedence[0-1][0-9][0-9])*([a-z] [a-z]) (body);```
    
    * AST:
        1. Function name([binary| unary] + (Op))
        2. Args: for unary 1 variable and binary for 2 variables.
        ```objectivec
        class ProtoTypeAST{
        private:
            std::string name;
            std::vector<std::string> args;
            bool IsOperator;
            unsigned precedence;
        public:
            ProtoTypeAST(std::string name,
                         std::vector<std::string> args,
                         bool IsOperator = false,
                         unsigned precedence = 0) : name(std::move(name)),
                         args(std::move(args)),
                              IsOperator(IsOperator),
                              precedence(precedence) {}
            llvm::Function* codeGen();
            const std::string& getName() const {
                return name;
            }
            const bool isUnary() const {
                return IsOperator && args.size() == 1;
            }
            const bool isBinary() const {
                return IsOperator && args.size() == 2;
            }
            const char getOperator() const{
                return name[name.size() - 1];
            };
            unsigned int getPrecedence() const {
                return precedence;
            }
        };
        ```
    
    * Parse definition(Prototype and function body):
        1. Prototype
        ```objectivec
        case TOKEN_UNARY:
            getNextToken();
            if(!isascii(curToken)) return logErrorP("Operator incorrect!");
            functionName += (char)curToken;
            kind = 1;
            getNextToken();
            break;
        case TOKEN_BINARY:
            getNextToken();
            if(!isascii(curToken)) return logErrorP("Operator incorrect!");
            functionName += (char)curToken;
            kind = 2;
            getNextToken();
            if(curToken == TOKEN_NUMBER){
                if(numVal < 1 || numVal > 100) return logErrorP("Precedence must between 1 and 100!");
                precedence = (unsigned)numVal;
                getNextToken();
            }
            break;
        ```
        
        2. Body: Rewrite the body parse expression.
        ```objectivec
        static std::unique_ptr<ExprAST> parseExpression(){
            std::unique_ptr<ExprAST> LHS = parseUnary();
            if(!LHS) return nullptr;
        
            return parseBinaryOpExpressionRHS(0, std::move(LHS));
        }
        ```
        
    * For the rest, it will follow the previous code.