## Optimizer and JIT Support

Please run with [code](https://github.com/Seanforfun/llvm-kaleidoscope/blob/master/Chapt3_Optimizer_and_JIT/toy.cpp).

### Optimizer
In LLVM structure, optimizer is applied on IR codes and defined by passes.

#### Steps for optimization
1. Initialize pass manager and add optimization passes.
    * Create a Function pass manager which is the container of passes.
    * Add passes to the manager.
    * Initialize the pass manager.
    ```objectivec
    void initializeModuleAndFPM(){
        theModules = llvm::make_unique<llvm::Module>("Seanforfun", llvmContext);
    
        theFPM = llvm::make_unique<llvm::legacy::FunctionPassManager>(theModules.get());
    
        //InstructionCombining - Combine instructions to form fewer, simple instructions.
        theFPM->add(llvm::createInstructionCombiningPass());
    
        //Reassociate - This pass reassociates commutative expressions in an order that
        //is designed to promote better constant propagation
        //For example:  4 + (x + 5)  ->  x + (4 + 5)
        theFPM->add(llvm::createReassociatePass());
    
        //Create a legacy GVN pass. This also allows parameterizing whether or not loads are eliminated by the pass.
        theFPM->add(llvm::createGVNPass());
    
        //CFGSimplification - Merge basic blocks, eliminate unreachable blocks,
        // simplify terminator instructions, convert switches to lookup tables, etc.
        theFPM->add(llvm::createCFGSimplificationPass());
    
        theFPM->doInitialization();
    }
    ```

2. Call Pass manager to optimize the function.
    ```objectivec
    theFPM->run(*function);
    ```

### Create a simple pass
Follow the instruction from [Writing an LLVM Pass](https://llvm.org/docs/WritingAnLLVMPass.html).

1. Create our Pass class inherit from Pass class.
    ```objectivec
    namespace {
        struct Hello: public FunctionPass{
            static char ID;
            Hello() : FunctionPass(ID) {}
    
           // The service of current pass
            bool runOnFunction(Function &F) override {
                errs() << "Hello:";
                errs().write_escaped(F.getName()) << "\n";
                return false;
            }
        };
    }
    
    char Hello::ID = 0;
    
    /**
    * Register current pass to PassManager.
    */
    static RegisterPass<Hello> X("Hello", "Hello World", false, false);
    
    static RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
            [](const PassManagerBuilder &Builder,
               legacy::PassManagerBase &PM){
        PM.add(new Hello());
    });
    ```
    
2. Apply the pass with opt command
    * Create .bc file by compile
        ```bash
        clang -emit-llvm -o hello.bc -c hello.cpp
        ``` 

    * Run hello.bc file with pass
    
        ![Imgur](https://i.imgur.com/K1jPnyJ.png)
    
    * Get Run time analysis
    
        ![Imgur](https://i.imgur.com/WtHB4Rb.png) 
    
### JIT(Just in time) Compiler
The code for Kaleidoscope is represented in file [KaleidoscopeJIT.h](https://github.com/Seanforfun/llvm-kaleidoscope/blob/master/include/KaleidoscopeJIT.h).

Learning tutorials for JIT will be offered later. Here I will follow current tutorial and embed the JIT to our compiler.

* Prepare the environment to create code for the current native target and declare and initialize the JIT.
    ```objectivec
    //initialize the native target corresponding to the host.
    llvm::InitializeNativeTarget();
    //initialize the native target asm printer.
    llvm::InitializeNativeTargetAsmPrinter();
    //initialize the native target asm parser
    llvm::InitializeNativeTargetAsmParser();
    ```

* Set up the data layout for module, data layout is a class which represents the data format of the code.

    ```objectivec
    TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());
    ```
    
    ![Imgur](https://i.imgur.com/cSsVmRU.png)

* JIT
    1. Add module to JIT(save function handler)
    2. For a function call, it will open a new module for creating the call expression.
    3. Call Function pointer to call the function.
    
    ```objectivec
    static void topLevelHandler(){
        if(auto pTop = parseTopLevel()) {
            if (auto* ir = pTop->codeGen()) {
                fprintf(stderr, "Parsed a top level expression.\n");
                ir->print(llvm::errs());
                auto H = theJIT->addModule(std::move(theModules));
                initializeModuleAndFPM();
    
                auto exprSymbol = theJIT->findSymbol("__anon_expr");
                assert(exprSymbol && "Function not found!");
    
                double (*FP)() = (double (*)())(intptr_t)cantFail(exprSymbol.getAddress());
                fprintf(stderr, "Evaluated to %f\n", FP());
    
                theJIT->removeModule(H);
            }
        } else getNextToken();
    }
    ```
    

### Reference
1. [Writing an LLVM Pass](https://llvm.org/docs/WritingAnLLVMPass.html)
