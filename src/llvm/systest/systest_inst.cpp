/*
 * Author: Markus Kusano
 *
 * Instrumentation pass for SysTest.
 *
 * This will instrument pthread_* functions with the specified dynamic analysis
 * functions (see src/runtime).
 *
 * Currently, atomic instructions are instrumented in the same way as stores
 * (systest_store_pre())
 */

#include <set>
#include "llvm/Pass.h"
#include <string>
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
  // name of memory instrumentation functions
  const char *LOAD_PRE_STR = "systest_load_pre";
  const char *STORE_PRE_STR = "systest_store_pre";

  struct SysTestInst : ModulePass {

    static char ID;


    // Set of functions to instrument. Initialized in constructor
    std::set<std::string> funcsToInst;

    SysTestInst() : ModulePass(ID) {
      // Initialize funcsToInst with pthread functions
      funcsToInst.insert("pthread_create");
      funcsToInst.insert("pthread_join");

      funcsToInst.insert("pthread_mutex_init");     
      funcsToInst.insert("pthread_mutex_destroy");    
      funcsToInst.insert("pthread_mutex_lock");     
      funcsToInst.insert("pthread_mutex_unlock");     
      funcsToInst.insert("pthread_mutex_trylock");  
      funcsToInst.insert("pthread_mutex_timedlock");

      funcsToInst.insert("pthread_cond_init");      
      funcsToInst.insert("pthread_cond_destroy");   
      funcsToInst.insert("pthread_cond_signal");    
      funcsToInst.insert("pthread_cond_broadcast"); 
      funcsToInst.insert("pthread_cond_wait");      
      funcsToInst.insert("pthread_cond_timedwait"); 

      funcsToInst.insert("pthread_rwlock_init");    
      funcsToInst.insert("pthread_rwlock_destroy"); 
      funcsToInst.insert("pthread_rwlock_rdlock");  
      funcsToInst.insert("pthread_rwlock_wrlock");  
      funcsToInst.insert("pthread_rwlock_tryrdlock");
      funcsToInst.insert("pthread_rwlock_trywrlock");
      funcsToInst.insert("pthread_rwlock_unlock");  

      funcsToInst.insert("pthread_exit");           
      funcsToInst.insert("__assert_fail");          
    }

    virtual bool runOnModule(Module &M) {
      // Insert the memory instrumentation functions
      declareMemFunctions(M);
      for (Module::iterator mi = M.begin(); mi != M.end(); ++mi) {
        Function &f = *mi;
        instFunc(f);
      }
      // IR was modified
      return true;
    }

    // Function instrumentation logic
    void instFunc(Function &f) {
      // Check if function declaration matches a function in funcsToInst. If
      // so, prefix it with "systest_" so it can be instrumented at runtime
      if (f.isDeclaration()) {
        std::string fname = f.getName();
        if (funcsToInst.count(fname) != 0) {
          f.setName("systest_" + fname);
        }
      }
      for (Function::iterator fi = f.begin(); fi != f.end(); ++fi) {
        BasicBlock &bb = *fi;
        instBB(bb);
      }
    }

    // Basicblock instrumentation logic
    void instBB(BasicBlock &B) {
      for (BasicBlock::iterator bi = B.begin(); bi != B.end(); ++bi) {
        Instruction &i = *bi;
        instInstruction(i);
      }
    }

    // Instruction instruemtnation logic
    void instInstruction(Instruction &i) {
      if (LoadInst *li = dyn_cast<LoadInst>(&i)) {
        // Create a load_pre function call with the Load instruction's
        // arguments
        createAndInsLoadPre(li);
      }
      else if (StoreInst *si = dyn_cast<StoreInst>(&i)) {
        // Create store_pre function call with the Store instruction's
        // arguments
        createAndInsStorePre(si);
      }
    }

    // Add function declarations for memory instrumentation
    //void declareMemFunctions(Instruction &i) {
      // call declareMemFunctions on Module
      //declareMemFunctions(i->getParent()->getParent()->getParent());
    //}

    // Create a load pre instrumentation function for the passed load
    // instruction. It will be inserted before the passed load instruction
    void createAndInsLoadPre(LoadInst *li) {
      Module *M = li->getParent()->getParent()->getParent();
      LLVMContext &C = li->getContext();
      Function *preFunc = M->getFunction(LOAD_PRE_STR);
      assert(preFunc && "load pre function declaration not found");

      // Get the pointer operand of the load and cast it to a int64
      Value *ptr = li->getPointerOperand();

      // 64 bit integer type
      Type *IntTy64 = Type::getInt64Ty(C);

      //  The result of castTo is either a CastInst or a Constant
      Value *ptrCast = castPtrTo(ptr, IntTy64, "ptrTo64", li);

      Value *id = getAndInsertInstId(li);

      // Setup vector of arguments
      std::vector<Value *> args;
      args.push_back(ptrCast);
      args.push_back(id);

      // Create the Call Instruction and insert it before the load instruction
      CallInst::Create(preFunc, args, "", li);
    }

    // Create a store pre instrumentation function for the passed load
    // instruction. It will be inserted before the passed load instruction
    void createAndInsStorePre(StoreInst *si) {
      Module *M = si->getParent()->getParent()->getParent();
      Function *preFunc = M->getFunction(STORE_PRE_STR);
      LLVMContext &C = si->getContext();
      assert(preFunc && "store pre function declaration not found");

      // Get the pointer operand of the load and cast it to a int64
      Value *ptr = si->getPointerOperand();

      // 64 bit integer type
      Type *IntTy64 = Type::getInt64Ty(C);

      //  The result of castTo is either a CastInst or a Constant
      Value *ptrCast = castPtrTo(ptr, IntTy64, "ptrTo64", si);

      Value *id = getAndInsertInstId(si);

      // Setup vector of arguments
      std::vector<Value *> args;
      args.push_back(ptrCast);
      args.push_back(id);

      // Create the Call Instruction and insert it before the store instruction
      CallInst::Create(preFunc, args, "", si);
    }

    // Insert and/or get the instruction ID for the passed instruction
    Value *getAndInsertInstId(Instruction *i) {
      LLVMContext &C = i->getContext();
      // TODO: Implement this. Currently it just returns an unsigned 64bit zero
      return ConstantInt::get(Type::getInt64Ty(C), 0U, false);
    }

    void declareMemFunctions(Module &m) {
      LLVMContext &C = m.getContext();
      // void type
      Type *voidTy = Type::getVoidTy(C);

      // 64 bit integer
      Type *IntTy64 = Type::getInt64Ty(C);

      // Setup type for load/store pre. Both take two 64 bit ints as parameters
      // and return void
      std::vector<Type*> params;
      params.push_back(IntTy64);
      params.push_back(IntTy64);
      bool isVarArg = false;
      FunctionType *funcType = FunctionType::get(voidTy, params, isVarArg);

      // insert functions to module
      m.getOrInsertFunction(LOAD_PRE_STR, funcType);
      m.getOrInsertFunction(STORE_PRE_STR, funcType);
    }

    // Cast the passed value to the passed type
    //
    // Modifed from SCUtils.h
    Value *castPtrTo(Value *V, Type *Ty, Twine Name, 
       Instruction *InsertPt) {
      // Don't bother creating a cast if it's already the correct type.
      assert (V && "castPtrTo: trying to cast a NULL Value!\n");
      if (V->getType() == Ty) {
        return V;
      }
                                                                                   
      // If it's a constant to an integer, just create a constant expression.
      if (Constant *C = dyn_cast<Constant>(V)) {
        if (C->getType()->isIntOrIntVectorTy()) {
          Constant *CE = ConstantExpr::getZExtOrBitCast(C, Ty);
          return CE;
        }
      }
      // Otherwise, insert a cast instruction.
      //return CastInst::CreateZExtOrBitCast(V, Ty, Name, InsertPt);
      return CastInst::CreatePointerCast(V, Ty, Name, InsertPt);
    }

  }; // struct SysTestInst
} // namespace


char SysTestInst::ID = 0;
static RegisterPass<SysTestInst> X("systest_inst",
                                    "SysTest Instrumentation Pass",
                                    true,  // modified CFG
                                    true); // non-analysis pass
