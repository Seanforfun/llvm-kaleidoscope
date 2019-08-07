//
// Created by sean on 06/08/19.
//

#ifndef KALEISCOPE_HELLOPASS_H
#define KALEISCOPE_HELLOPASS_H


#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Function.h>

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

namespace {
    struct Hello: public FunctionPass{
        static char ID;
        Hello() : FunctionPass(ID) {}

        bool runOnFunction(Function &F) override {
            errs() << "Hello:";
            errs().write_escaped(F.getName()) << "\n";
            return false;
        }
    };
}

char Hello::ID = 0;

namespace {
    RegisterPass<Hello> X("Hello", "Hello World", false, false);

    RegisterStandardPasses Y(PassManagerBuilder::EP_EarlyAsPossible,
                             [](const PassManagerBuilder &Builder,
                                legacy::PassManagerBase &PM){
                                 PM.add(new Hello());
                             });

}


#endif //KALEISCOPE_HELLOPASS_H

