## Flow Control

Please read with [code](https://github.com/Seanforfun/llvm-kaleidoscope/blob/master/Chapt4_Flow_Control/toy.cpp).

### IF Control
1. Syntax for if flow control

    ```objectivec
    def foo(x) if x > 0 then 1 else 0;
    ```
2. Abstract Syntax Tree for IF control.
    ![Imgur](https://i.imgur.com/JJ7xhOs.png)

3. Basic Block structure without optimization
    * Knowledge 1: SSA(static single assignment) form.
        ![Imgur](https://i.imgur.com/ZYwE5PI.png)
    
    * PHI Node: As the result of y3 is not determined in the last block, there is a PHI(Ф) node which select the y3 value according to condition.

4. IR Code Explanation:
    ```
    def baz(x) if x then foo() else bar();
    define double @baz(double %x) {
     # Comparison block, determine which block to jump to.
    entry:
    %ifcond = fcmp one double %x, 0.000000e+00
    br i1 %ifcond, label %then, label %else
    
    # Then block, calculate results and jump to PHI Node.
    then:       ; preds = %entry
    %calltmp = call double @foo()
    br label %ifcont
    
    # Else block, calculate results and jump to PHI Node.
    else:       ; preds = %entry
    %calltmp1 = call double @bar()
    br label %ifcont
    
    # Phi node merge the result.
    ifcont:     ; preds = %else, %then
    %iftmp = phi double [ %calltmp, %then ], [ %calltmp1, %else ]
    ret double %iftmp
    }
    ```

5. Code generator for if flow control block
    ```objectivec
    llvm::Value *IfExprAST::codeGen() {
        //%ifcond = fcmp one double %x, 0.000000e+00
        llvm::Value* CondV = COND->codeGen();
        if(!CondV) return nullptr;
    
        CondV = builder.CreateFCmpONE(CondV, llvm::ConstantFP::get(llvmContext, llvm::APFloat(0.0)), "ifcond");
    
        llvm::Function* function = builder.GetInsertBlock()->getParent();
        llvm::BasicBlock* thenBlock = llvm::BasicBlock::Create(llvmContext, "then");
        llvm::BasicBlock* elseBlock = llvm::BasicBlock::Create(llvmContext, "else");
        llvm::BasicBlock* mergeBlock = llvm::BasicBlock::Create(llvmContext, "ifcont");
    
        // 1. Condition check and branch
        // br i1 %ifcond, label %then, label %else
        builder.CreateCondBr(CondV, thenBlock, elseBlock);
    
        // emit then block
        function->getBasicBlockList().push_back(thenBlock);
        builder.SetInsertPoint(thenBlock);
        // %calltmp = call double @foo()
        llvm::Value* thenVal = THEN->codeGen();
        if(!thenVal) return nullptr;
        // br label %ifcont
        builder.CreateBr(mergeBlock);
        thenBlock = builder.GetInsertBlock();
    
        //2. emit then block
        function->getBasicBlockList().push_back(elseBlock);
        builder.SetInsertPoint(elseBlock);
        llvm::Value* elseValue = ELSE->codeGen();
        if(!elseValue) return nullptr;
        // br label %ifcont
        builder.CreateBr(mergeBlock);
    
        elseBlock = builder.GetInsertBlock();
    
        //3. emit the merge block
        function->getBasicBlockList().push_back(mergeBlock);
        builder.SetInsertPoint(mergeBlock);
    
        // Create Ф node in SSA graph(static single assignment graph)
        llvm::PHINode* phiNode = builder.CreatePHI(llvm::Type::getDoubleTy(llvmContext), 2, "tmpIf");
    
        phiNode->addIncoming(thenVal, thenBlock);
        phiNode->addIncoming(elseValue, elseBlock);
    
        return phiNode;
    }
    ```

6. Result

    ![Imgur](https://i.imgur.com/AVrvScR.png)

### For control
1. Syntax for for flow control
    ```
    def foo() for i=0, i<10 in sin(i); # default step size is 1.
    def foo() for i=0, i<10, 1.0 in sin(i);
    ```
    
2. Abstract Syntax Tree for for loop.
    ```objectivec
    class ForExprAST : public ExprAST{
        std::string value; // internal flow control variable
        std::unique_ptr<ExprAST> start, end, step, body;
    
    public:
        ForExprAST(std::string value, std::unique_ptr<ExprAST> start, std::unique_ptr<ExprAST> end,
                   std::unique_ptr<ExprAST> step, std::unique_ptr<ExprAST> body) :
                   value(std::move(value)),
                   start(std::move(start)),
                   end(std::move(end)),
                   step(std::move(step)),
                   body(std::move(body)) {}
    
        llvm::Value *codeGen() override;
    };
    ```

3. for-loop step
    * Step 1: initialize variable.
    * Step 2: check condition, if true, continue else break.
    * Step 3: service.
    * Step 4: add variable and step, go back to Step 2.

4. IR Code
    
    ![Imgur](https://i.imgur.com/zkyye6j.png)

5. IR Generator
    ```objectivec
    llvm::Value *ForExprAST::codeGen() {
        llvm::Function* function = builder.GetInsertBlock()->getParent();
    
        llvm::Value* startVal = start->codeGen();
        auto entryBB = builder.GetInsertBlock();
        auto checkBB = llvm::BasicBlock::Create(llvmContext, "check");
        auto bodyBB = llvm::BasicBlock::Create(llvmContext, "body");
        auto afterLoopBB = llvm::BasicBlock::Create(llvmContext, "after_loop");
        function->getBasicBlockList().push_back(checkBB);
        function->getBasicBlockList().push_back(bodyBB);
        function->getBasicBlockList().push_back(afterLoopBB);
    
        builder.SetInsertPoint(entryBB);
        builder.CreateBr(checkBB);
    
        builder.SetInsertPoint(checkBB);
        llvm::PHINode* Variable = builder.CreatePHI(llvm::Type::getDoubleTy(llvmContext), 2, value);
        Variable->addIncoming(startVal, entryBB);
    
        llvm::Value* oldVal = nameValueTbl[value];
        nameValueTbl[value] = Variable;
    
        // compare
        llvm::Value* stepVal;
        if(step){  // use step
            stepVal = step->codeGen();
            if(!stepVal) return nullptr;
        }else{
            stepVal = llvm::ConstantFP::get(llvm::Type::getDoubleTy(llvmContext), 0.0);
        }
        llvm::Value* nextVal = builder.CreateFAdd(Variable, stepVal);
        Variable->addIncoming(nextVal, bodyBB);
    
        auto EndVal = end->codeGen();
        EndVal = builder.CreateFCmpONE(EndVal, llvm::ConstantFP::get(llvmContext, llvm::APFloat(0.0)), "ifcond");
        builder.CreateCondBr(EndVal, afterLoopBB, bodyBB);
    
        // task
        builder.SetInsertPoint(bodyBB);
        if(!(body->codeGen())) return nullptr;
        builder.CreateBr(checkBB);
    
        // end
        builder.SetInsertPoint(afterLoopBB);
        if(oldVal)
            nameValueTbl[value] = oldVal;
        else nameValueTbl.erase(value);
        return llvm::ConstantFP::get(llvmContext, llvm::APFloat(1.0));
    }
    ```

### Reference
1. [静态单赋值形式](https://zh.wikipedia.org/wiki/%E9%9D%99%E6%80%81%E5%8D%95%E8%B5%8B%E5%80%BC%E5%BD%A2%E5%BC%8F)
2. [Kaleidoscope: Extending the Language: Control Flow](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl05.html#lexer-extensions-for-if-then-else)