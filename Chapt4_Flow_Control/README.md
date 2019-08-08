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

### Reference
1. [静态单赋值形式](https://zh.wikipedia.org/wiki/%E9%9D%99%E6%80%81%E5%8D%95%E8%B5%8B%E5%80%BC%E5%BD%A2%E5%BC%8F)
2. [Kaleidoscope: Extending the Language: Control Flow](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl05.html#lexer-extensions-for-if-then-else)