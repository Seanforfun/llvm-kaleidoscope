## IR Generator

Please read with [code](https://github.com/Seanforfun/llvm-kaleidoscope/blob/master/Chapt2_IR_Generator/toy.cpp).

### Introduction
IR is short for Intermediate representation. It is a form that LLVM used to represent the code. IR is very like Assembly code but more human readable.

LLVM created its own api to translate AST(Abstract Syntax Tree) to IR.  
![Imgur](https://i.imgur.com/tw7JV36.png)

* LLVMContext
    * Context container in thread scope, "includes the type and constant uniquing tables".
    * Not thread-safe.
    * Opaquely

* Module
    * Top level container of all other LLVM Intermediate Representation (IR) objects.
    * Each module directly contains a list of globals variables, a list of functions, a list of libraries (or other modules) this module depends on, a symbol table, and various data about the target's characteristics.

* Basic Block
    * A basic block is simply a container of instructions that execute sequentially.
    * We may use multiple basic block to construct a function.

* IRBuilder
    * API combinations to create and insert instructions. 
    * Builder could insert instruction to multiple levels.

### IR Code Generator
1. Numeric value IR generator
    * Return a Value pointer, recording its type and value. Value will also maintain what other variables referred this variable.  
    
    ```objectivec
    llvm::Value* NumberExprAST::codeGen() {
        return llvm::ConstantFP::get(llvmContext, llvm::APFloat(val));
    }
    ```

2. Variable value IR generator
    * In this tutorial, only variable type is double.
    * We will save all variable name and its value pointer into a map. <name, Value*>
    
    ```objectivec
    llvm::Value *VariableExprAST::codeGen() {
        llvm::Value* v = nameValueTbl[variable];
        if(!v) return logErrorV("Unknown variable name!");
        return v;
    }
    ``` 

3. Binary Operation Expression IR generator
     * Create basic binary operator expressions.
     * All expressions are llvm context level(can be used by all BB).
     
    ```objectivec
    llvm::Value *BinaryExprAST::codeGen() {
        llvm::Value* l = LHS->codeGen();
        llvm::Value* r = RHS->codeGen();
    
        if(!l || !r) return nullptr;
    
        switch (Op){
            case '<':
               // api only use 1 bit to represent the result.
               // return will be 0 or 1
                l = builder.CreateFCmpULT(l, r, "tmpCmp");
                // convert integer 0/1 to double.
                return builder.CreateUIToFP(l, llvm::Type::getDoubleTy(llvmContext), "tmpBool");
            case '+':
               // Add instruction
                return builder.CreateFAdd(l, r, "tmpAdd");
            case '-':
               // Subtract instruction
                return builder.CreateFSub(l, r, "tmpSub");
            case '*':
               // Multiplication instruction
                return builder.CreateFMul(l, r, "tmpMul");
            default:
                logErrorV("Invalid Operator!");
        }
        return nullptr;
    }
    ```
    
4. Call expression IR generator
    * e.g foo(1, 2)
    1. Check if function is defined.
    2. Check if user call the function correctly.
    3. Create function call IR.
    
    ```objectivec
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
    ```

5. Prototype IR generator， e.g foo(a b c)
    1. Create a variable type vector, all variables in this tutorial can only be double.
    2. Create Function: 
        * FunctionType: return type, variable type ...
        * Function: FunctionType, function name, linkage(ExternalLinkage means may be defined in other module), which module it belongs to.
    3. Set human readable variable name.
    
    ```objectivec
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
    ```

6. Function IR generator， e.g def foo(a b) a + b;
    1. Prototype generator.
    2. Body(insert code)
    ```objectivec
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
    ```

### Result
![Imgur](https://i.imgur.com/eDK0Ep0.png)

### Reference
1. [Kaleidoscope: Code generation to LLVM IR](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html)