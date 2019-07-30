## Lexer and Parser

### What is Lexer(Character Level)
When compiler is translating a code, it requires to split code into tokens. The tool to extract the tokens is call lexer(or scanner).

The tokens returned by lexer may have two parts:
1. token code: a symbol representing what kind of token it is.
2. metadata: information contained by this token.

### Token types
In this demo, we defined 5 token types.
```objectivec
enum Token{
    TOKEN_EOF = -1, // end of file

    // commands: basic commands, def and extern
    TOKEN_DEF = -2,
    TOKEN_EXT = -3,

    //primary: the variables or numbers created by program.
    // e.g a = 3.0, where a is a identifier and 3.0 is a number.
    TOKEN_IDENTIFIER = -4, 
    TOKEN_NUMBER = -5
};
```
Once we call a function to get next token, it will return the type if the token: either values above or its original ASCII value like '+', '('.

## Get next token method
1. Use two global variables to hold the values of a token.
```objectivec
static std::string identifierStr;   //TOKEN_IDENTIFIER: if current token is an identifier, this variable holds the name.
static double numVal; //TOKEN_NUMBER: if current token is a number, this variable holds the value.
```

2. Get token.
    1. Skip beginning white space.
    2. If current token is identifier(starts with letter), read the whole word. If current word is command, return the correct token code.
    3. If current token is number, load it as a double value and return TOKEN_NUMBER.
    4. Comment all character after # unless we meet EOF , \r or \n. Cannot handle the case (# \r\n), \n will be read as a token and return.
    5. EOF case.
    6. Other cases, '(', ')', ',' etc, for these symbols just return them.
```objectivec
/**
 * Get token from standard input.
 * @return if matches enum TOKEN return its value, otherwise return the ASCII of the character.
 */
static int getToken(){
    static int lastCharacter = ' ';
    //1. Skip beginning white space.
    while(isspace(lastCharacter))
        lastCharacter = getchar();

    //2. If current tokens is identifier(starts with letter), read the whole word.
    // If current word is command, return the correct token code.
    if(isalpha(lastCharacter)){ // current token an identifier
        identifierStr = lastCharacter;
        while(isalpha((lastCharacter = getchar()))){
            identifierStr += lastCharacter;
        }
        if(identifierStr == "def") return TOKEN_DEF;
        else if(identifierStr == "extern") return TOKEN_EXT;
        return TOKEN_IDENTIFIER;
    }

    //3. If current token starts with digit, parse it as a number token.
    if(isdigit(lastCharacter) || lastCharacter == '.'){
        std::string numStr;
        numStr += lastCharacter;
        while(isdigit(lastCharacter = getchar()) || lastCharacter == '.'){
            numStr += lastCharacter;
        }
        numVal = strtod(numStr.c_str(), nullptr);
        return TOKEN_NUMBER;
    }
    
    //4. Comment all character after # unless we meet EOF , \r or \n
    // Cannot handle the case (# \r\n), \n will be read as a token and return.
    if(lastCharacter == '#'){
        do{
            lastCharacter = getchar();
        }while (lastCharacter != EOF && lastCharacter != '\n' && lastCharacter != '\r');

        if(lastCharacter != EOF) return getToken();
    }
    
    //5. EOF case.
    if(lastCharacter == EOF) return TOKEN_EOF;

    //6. Other cases, '(', ')', ',' etc, for these symbols just return them.
    int lastCharASCII = lastCharacter;
    lastCharacter = getchar();
    return lastCharASCII;
}
```

## Parser(Token Level)
Only tokens cannot represent the grammar or higher level meaning of the program. We need parser to organize the tokens and show the hierarchy.

### Abstract Syntax Tree(AST)
1. Example: position := initial + rate * 60; 
    ![Imgur](https://i.imgur.com/LsxJQ3p.png)
2. Recursive descent parser: This is a parse algorithm from top level to bottom level.
3. Operator-precedence Parsing: Method to define the priority of operators.

### AST Models
1. Base AST Model
    ```objectivec
    class ExprAST{
    public:
        virtual ~ExprAST() = default;
    };
    ```
2. Number AST: hold number token
    ```objectivec
    class NumberExprAST: public ExprAST{
            double val;
    
        public:
            NumberExprAST(double val): val(val) {}
        };
    ```

3. VariableAST: class for variable like 'a'

    ```objectivec
    class VariableExprAST: public ExprAST{
       std::string variable;
    
    public:
       VariableExprAST(const std::string & variable): variable(std::move(variable)){}
    };
    ```

4. BinaryOpAST: hold binary operations
    * a + b, op is '+', LHS, RHS are variableAST.
    
    ```objectivec
    class BinaryExprAST: public ExprAST{
        char Op;
        std::unique_ptr<ExprAST> LHS, RHS;
    
    public:
        BinaryExprAST(char op, std::unique_ptr<ExprAST> &lhs,
                std::unique_ptr<ExprAST> &rhs) :
                Op(op), LHS(std::move(lhs)), RHS(std::move(rhs)) {}
    };
    ```

5. FunctionCallAST: hold function call.
    * foo(a, b), callee is function name foo and args is a vector hold all parameter.
    
    ```objectivec
    class CallExprAST: public ExprAST{
        std::string callee; // function name;
        std::vector<std::unique_ptr<ExprAST>> args;
    
    public:
        CallExprAST(const std::string &callee,
                const std::vector<std::unique_ptr<ExprAST>> &args) : callee(callee), args(std::move(args)) {}
    };
    ```

6. PrototypeAST: hold prototype, record function name and arguments, its structure is similar to FunctionCallAST.
    ```objectivec
    class ProtoTypeAST{
        std::string name;
        std::vector<std::string> args;
    
    public:
        ProtoTypeAST(const std::string &name,
                const std::vector<std::string> &args) : name(name), args(std::move(args)) {}
    };
    ```

7. FunctionAST: hold the definition of a function, including function prototype and body of the function.
    ```objectivec
    class FunctionAST{
        std::unique_ptr<ProtoTypeAST> prototype;
        std::unique_ptr<ExprAST> body;
    public:
        FunctionAST(std::unique_ptr<ProtoTypeAST> &prototype, std::unique_ptr<ExprAST> &body) :
        prototype(std::move(prototype)), body(std::move(body)) {}
    };
    ```

### Objective of Parser
1. For expression a + b, we want to get:
    ```objectivec
    auto LHS = llvm::make_unique<VariableExprAST>("a");
    auto RHS = llvm::make_unique<VariableExprAST>("b");
    auto Result = llvm::make_unique<BinaryExprAST>("+", std::move(LHS), std::move(RHS));
    ```

2. Parser for numerical numbers, case TOKEN_NUMBER.
    ```objectivec
    static std::unique_ptr<ExprAST> parseNumberExpr(){
      auto result =  llvm::make_unique<NumberExprAST>(numVal);
      getNextToken();
      return result;
    }
    ```

3. Parser for parenthesis, if current read character is '('.
    ```objectivec
    static std::unique_ptr<ExprAST> parsePatheneseExpr(){
        getNextToken(); // eat ( , move to net character.
    
        auto v = parseExpression();    // recursively parse the expression between '( ' and ')'
        if(!v) return nullptr;
        if(curToken != ')') logError("Expect ')'");    // check the symmetry of parenthesis
        getNextToken(); // eat ) 
        return v;  // return the internal AST.
    }
    ``` 

4. Parser for identifier, e.g:
    * abcd, single variable.e.g a = b , a = (b)
    * abcd(a, b ,c ,d), function call.
    * abcd(1 + 2), function call.
    ```objectivec
    static std::unique_ptr<ExprAST> parseIdentifierExpr(){
        std::string idName = identifierStr;
    
        getNextToken(); // move to next token
       
       // current expression is a = b, where b is current identifier.
        if(curToken != '(') return llvm::make_unique<VariableExprAST>(idName);  // a = b
    
       // current expression is a function call;
        getNextToken();    // skip '('.
        std::vector<std::unique_ptr<ExprAST>> args;
        if(curToken != ')'){ //a = (b)
            while (1){
                if(auto arg = parseExpression()){  // recursively called internal AST parser
                    args.push_back(arg);
                }else return nullptr;
    
                if(curToken == ')') break; // current function call is end.
                if(curToken != ',') logError("Expect ',' or ')' in argument list");
                getNextToken();    // skip ','
            }
        }
        getNextToken();   // move to next token.
        return llvm::make_unique<CallExprAST>(idName, args);
    }
    ```

5. Primary Parser: handle TOKEN_IDENTIFIER, TOKEN_NUMBER or parenthesis.
    * deal with all primary TOKEN types.
    ```objectivec
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
    ```

6. Expression Parser: parse binary operations, e.g parse a+b+(c+d) * e * f + g:
    1. primary parse for LHS 'a'. [op RHS] are [+ b], [+ (c + d)], [* e], [* f] and [+ g].
    2. If current is the end case.
    3. Compare current operator with next operator, 3 cases, exist(same and higher) not exist(-1))
    ```objectivec
    /**
     *  Create a binary operation AST.
     * @param opPrec    precedence of previous operator.
     * @param LHS  LHS AST of current binary operation
     * @return  BinaryOpExprAST pointer.
     */
    static std::unique_ptr<ExprAST> parseBinaryOpExpressionRHS(int opPrec, std::unique_ptr<ExprAST> LHS){
        while (1){
            int curPrec = getTokenPrecedence(); // precedence of current operator.
    
            if(curPrec < opPrec) return LHS;    // This is the end of the expression. Current token is not an operator.
    
            int curOp = curToken;
            getNextToken(); // skip current operator.
    
            std::unique_ptr<ExprAST> RHS = parsePrimary(); // Get RHS variable.
            if(!RHS) return nullptr;
    
            int nextPrec = getTokenPrecedence();   
            if(curPrec < nextPrec){
                RHS = parseBinaryOpExpressionRHS(curPrec + 1, std::move(RHS));
                if(RHS) return nullptr;
            }
            
           // next operator has same or lower precedence to current one, go to the while loop again.
           // merge current LHS and RHS.
            LHS = llvm::make_unique<BinaryExprAST>(curOp, std::move(LHS), std::move(RHS));
        }
    }
    ```

7. Parse prototype: parse function prototype like: ```foo(a b c d)```
    1. save the function name.
    2. push all arguments names into a vector.
    3. Create a prototype AST holding the function name and args names.
    ```objectivec
    static std::unique_ptr<ProtoTypeAST> parsePrototype(){
        if(curToken != TOKEN_IDENTIFIER) return logErrorP("Expect a function name in prototype");
        //1. save the function name.
        std::string functionName = identifierStr;
        getNextToken();
    
        if(curToken != '(') return logErrorP("Expect '(' in prototype");
        //2. push all arguments names into a vector.
        std::vector<std::unique_ptr<ExprAST>> args;
        while((cur = getNextToken()) == TOKEN_IDENTIFIER){
            args.push_back(identifierStr);
            getNextToken();
        }
        if(curToken != ')') return logErrorP("Expect ')' matching '('");
        getNextToken();
        //3. Create a prototype AST holding the function name and args names.
        return llvm::make_unique<ProtoTypeAST>(functionName, std::move(args));
    }
    ```

8. Parse Definition: parse function definition, e.g: ```def foo(a b) a + b```
    1. definition can be divided to three parts: def token, prototype and body
    ```objectivec
    static std::unique_ptr<FunctionAST> parseDefinition(){
        getNextToken(); //skip token 'def'
    
       // prototype
        auto proto = parsePrototype();
        if(!proto) return nullptr;
    
       // body
        auto body = parseExpression();
        return llvm::make_unique<FunctionAST>(std::move(proto), std::move(body));
    }
    ```

9. Parse Extern: parse extern like ```extern foo(a b)```
    ```objectivec
    static std::unique_ptr<ProtoTypeAST> parseExtern(){
        getNextToken(); // skip "extern"
        return parsePrototype();
    }
    ```

10. Parse Anonymous functions, e.g. ```y```
    ```objectivec
    static std::unique_ptr<FunctionAST> parseTopLevel(){
        if(auto body = parseExpression();
        auto proto = llvm::make_unique<ProtoTypeAST>("", std::vector<std::string> v);
        return llvm::make_unique<FunctionAST>(std::move(proto), std::move(body));
    }
    ```

### Top Level Handlers
1. Definition Handler:
    ```objectivec
    static void definitionHandler(){
        if(parseDefinition()){
            fprintf(stderr, "Parsed a function definition.\n")
        }else getNextToken();
    }
    ```

2. Extern Handler:
    ```objectivec
    static void externHandler(){
        if(parseExtern()) fprintf(stderr, "Parsed a extern.\n")
        else getNextToken();
    }
    ```

3. Top Level Handler for anonymous functions:
    ```objectivec
    static void topLevelHandler(){
        if(parseTopLevel()) fprintf(stderr, "Parsed a top-level expression.\n")
    }
    ```