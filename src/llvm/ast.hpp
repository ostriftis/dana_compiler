#ifndef __AST_HPP__
#define __AST_HPP__



#include <iostream>
#include <vector>
#include <string>
#include <tuple>
#include <utility>
#include <map>
#include <set>
#include <llvm/IR/Module.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Utils.h>

#include "symbol.hpp"
#include "lexer.hpp"

using namespace llvm;

const std::string name[] = {"UNDEFINED", "TYPE_int", "TYPE_byte", "TYPE_bool", "TYPE_array", "TYPE_void", "TYPE_func"};

extern const std::string libfuncs[];

bool is_libfunc(std::string s);
void initialize_st();

/* This is to represent all variables in the Scope */
extern std::vector<std::map<std::string, AllocaInst*>> NamedValues;


struct func_details {
    std::string name;
    std::map<std::string, Type*> argBaseTypes;
    std::vector<std::string> captured_vars;  // Variables captured from parent scopes
    std::map<std::string, Type*> captured_types;  // Types of captured variables

    func_details(std::string n, std::map<std::string, Type *> b) {
        name = n;
        argBaseTypes = b;
    }
};

func_details * get_parent();
/*This stack is used to handle function name definitions and calls.
  We always push back the last function, so when some kid searches for its parent
  function, it will always be the back of the previous element in the vector*/
extern std::vector<std::vector<func_details *>> func_names;

// Map from function name to its captured variables (for nested functions)
extern std::map<std::string, std::vector<std::pair<std::string, Type*>>> captured_variables;

/*  This is used for break and continue stmts:
    - the first element is the loop body
    - the second element is the loop end      */
extern std::map<std::string, std::pair<BasicBlock *, BasicBlock *>> NamedLoops;
extern std::vector<std::pair<BasicBlock *, BasicBlock *>> Loops;

AllocaInst *lookup(std::string s);

class Def;
class SimpleStmt;

struct MixedContent {
    std::vector<Def*> definitions;
    std::vector<SimpleStmt*> statements;
    
    void addDef(Def* def) {
        definitions.push_back(def);
    }
    
    void addStmt(SimpleStmt* stmt) {
        statements.push_back(stmt);
    }
};


class AST {
    public:
        virtual void printAST(std::ostream &out) const = 0;
        virtual void sem_analyze() {}
        virtual Value* igen() const {return nullptr;}
        virtual Function* igen_main() const {return nullptr;}

        void compile(const std::string& input_file, bool flag_i, bool flag_f, 
                    bool flag_O, const std::string& output_file);

        void LLVM_IR_gen(bool optimize=true) {
                // Initialize
            TheModule = std::make_unique<Module>("dana program", TheContext);
            TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());
            
            if (optimize) {
              // 1. Mem2Reg: Promotes memory to registers (SSA form)
              TheFPM->add(createPromoteMemoryToRegisterPass());
              // 2. SROA: Scalar Replacement of Aggregates
              TheFPM->add(createSROAPass());
              // 3. Early CSE: Common Subexpression Elimination
              TheFPM->add(createEarlyCSEPass());
              // 4. Instruction Combining
              TheFPM->add(createInstructionCombiningPass());
              // 5. Reassociate: Expression reassociation
              TheFPM->add(createReassociatePass());
              // 6. CFG Simplification
              TheFPM->add(createCFGSimplificationPass());
              // 7. Loop Invariant Code Motion (LICM)
              TheFPM->add(createLICMPass());
              // 8. Loop Unrolling
              TheFPM->add(createLoopUnrollPass());
              // 9. GVN: Global Value Numbering
              TheFPM->add(createGVNPass());
              // 10. Dead Code Elimination
              TheFPM->add(createDeadCodeEliminationPass());
              // 11. Final CFG Cleanup
              TheFPM->add(createCFGSimplificationPass());
              // 12. Final Instruction Combining
              TheFPM->add(createInstructionCombiningPass());
            }
            TheFPM->doInitialization();

            // Initialize types
            i8  = IntegerType::get(TheContext, 8);
            i32 = IntegerType::get(TheContext, 32);
            i64 = IntegerType::get(TheContext, 64);

            void_type = Type::getVoidTy(TheContext);
            i8_ptr = PointerType::get(i8, 0);

            st.enterScope();
            initialize_st();
            sem_analyze(); 
            st.exitScope();

            // Initialize library functions
            FunctionType *writeInteger_type = FunctionType::get(Type::getVoidTy(TheContext), {i64}, false);
            TheWriteInteger = Function::Create(writeInteger_type, Function::ExternalLinkage,
                               "writeInteger", TheModule.get());
            
            
            FunctionType *writeString_type = FunctionType::get(Type::getVoidTy(TheContext),
                                {PointerType::get(i8, 0)}, false);
            TheWriteString = Function::Create(writeString_type, Function::ExternalLinkage,
                       "writeString", TheModule.get());
            
            
            FunctionType *writeChar_type = FunctionType::get(Type::getVoidTy(TheContext), {i8}, false);
            TheWriteChar = Function::Create(writeChar_type, Function::ExternalLinkage,
                       "writeChar", TheModule.get());

            FunctionType *writeByte_type = FunctionType::get(Type::getVoidTy(TheContext), {i8}, false);
            TheWriteByte = Function::Create(writeByte_type, Function::ExternalLinkage,
                       "writeByte", TheModule.get());
            
            FunctionType *readInteger_type = FunctionType::get(i64, {}, false);
            TheReadInteger = Function::Create(readInteger_type, Function::ExternalLinkage,
                       "readInteger", TheModule.get());
                
            FunctionType *readString_type = FunctionType::get(void_type, {i64, i8_ptr}, false);
            TheReadString = Function::Create(readString_type, Function::ExternalLinkage,
                       "readString", TheModule.get());

            FunctionType *readChar_type = FunctionType::get(i8, {}, false);
            TheReadChar = Function::Create(readChar_type, Function::ExternalLinkage,
                       "readChar", TheModule.get());

            FunctionType *readByte_type = FunctionType::get(i8, {}, false);
            TheReadByte = Function::Create(readByte_type, Function::ExternalLinkage,
                       "readByte", TheModule.get());


            FunctionType *extend_type = FunctionType::get(i64, {i8}, false);
            TheExtend = Function::Create(extend_type, Function::ExternalLinkage,
                       "extend", TheModule.get());

            FunctionType *shrink_type = FunctionType::get(i8, {i64}, false);
            TheShrink = Function::Create(shrink_type, Function::ExternalLinkage,
                       "shrink", TheModule.get());

            FunctionType *strlen_type = FunctionType::get(i64, {i8_ptr}, false);
            TheStrLen = Function::Create(strlen_type, Function::ExternalLinkage,
                       "strlen", TheModule.get());

            FunctionType *strcmp_type = FunctionType::get(i64, {i8_ptr, i8_ptr}, false);
            TheStrCmp = Function::Create(strcmp_type, Function::ExternalLinkage,
                       "strcmp", TheModule.get());

            FunctionType *strcpy_type = FunctionType::get(void_type, {i8_ptr, i8_ptr}, false);
            TheStrCpy = Function::Create(strcpy_type, Function::ExternalLinkage,
                       "strcpy", TheModule.get());

            FunctionType *strcat_type = FunctionType::get(void_type, {i8_ptr, i8_ptr}, false);
            TheStrCat = Function::Create(strcat_type, Function::ExternalLinkage,
                       "strcat", TheModule.get());

            // Define and start the main function.
            //FunctionType *main_type = FunctionType::get(i32, {}, false);
            //Function *main =    Function::Create(main_type, Function::ExternalLinkage,
            //                                                  "main", TheModule.get());
            
            ///BasicBlock *BB = BasicBlock::Create(TheContext, "entry", main);
            ///Builder.SetInsertPoint(BB);

            // Emit the program code.
            std::map<std::string, AllocaInst*> first;
            std::vector<func_details *> f;
            std::clog << "TheModule in LLVM_IR_gen = " << TheModule.get() << std::endl;
            NamedValues.push_back(first);
            func_names.push_back(f);
            Function *main = igen_main();
            NamedValues.pop_back();
            func_names.pop_back();


            //Builder.CreateRet(c32(0));

            // Verify the IR.
            bool bad = verifyModule(*TheModule, &errs());
            if (bad) {
            std::cerr << "The IR is bad!" << std::endl;
            TheModule->print(errs(), nullptr);
            std::exit(1);
            }

            // Optimize!
            TheFPM->run(*main);

            // Note: Output is now handled by compile() function
        }

        Type *get_llvm_type(gen_type *t, bool by_ref = false) const {
            DataType dt = t->dtype;

            Type *baseType = nullptr;
            if(dt != TYPE_array) {
                if (dt == TYPE_int) baseType = i64;
                else if (dt == TYPE_byte) baseType = i8;
                else if (dt == TYPE_bool) baseType = i8;
                else if (dt == TYPE_void) baseType = Type::getVoidTy(TheContext);

                if (by_ref && !baseType->isVoidTy()) {
                    baseType = PointerType::get(baseType, 0);
                }
            }
            else {
                DataType artype = t->ar->get_type();
                Type* arrayType = nullptr;
                if (artype == TYPE_int) arrayType = i64;
                else if(artype == TYPE_byte) arrayType = i8;
                
                if (t->ar->size[0] == -1) {
                    int length = t->ar->size.size();
                    for (int i = 1; i < length; i++) {
                        // Handle multidimensional
                        arrayType = ArrayType::get(arrayType, 0);
                    }
                    std::clog << "Element has type array with first dim []" << std::endl;
                    baseType = PointerType::get(arrayType, 0);
                }
                else {
                    for (int i: t->ar->size) {
                        arrayType = llvm::ArrayType::get(arrayType, i);
                    }
                    // For sized arrays, if by_ref is true (function parameters), use pointer
                    if (by_ref) {
                        baseType = PointerType::get(arrayType, 0);
                    } else {
                        baseType = arrayType;
                    }
                }
            }
            if (!baseType) return nullptr;

            
            return baseType;
        }

        Type *get_llvm_type(DataType t, std::vector<int> dims = {}) const {
            if (dims.empty()) {
                if (t == TYPE_int) return i64;
                else if (t == TYPE_byte) return i8;
                else if (t == TYPE_bool) return i8;
                else if (t == TYPE_void) return Type::getVoidTy(TheContext); 
            }
            else {
                Type* arrayType;
                if (t == TYPE_int) arrayType = i64;
                else if(t == TYPE_byte) arrayType = i8;
                
                // --- FIX ---
                // Iterate backwards to construct the type from the innermost dimension outwards.
                for (auto it = dims.rbegin(); it != dims.rend(); ++it) {
                    arrayType = llvm::ArrayType::get(arrayType, *it);
                }
                return arrayType;
            }
            return nullptr;
        }


        static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, StringRef VarName, Type *type) {
        /*Borrowed from LLVM - Kaleidoscope tutorial. First line gets the start of the BB 
          in order to insert the alloca instruction*/
          IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
          return TmpB.CreateAlloca(type, nullptr, VarName);
        }

 protected:
    static LLVMContext TheContext;
    static IRBuilder<> Builder;
    static std::unique_ptr<Module> TheModule;
    static std::unique_ptr<legacy::FunctionPassManager> TheFPM;

    static GlobalVariable *TheVars;
    static GlobalVariable *TheNL;
    static Function *TheWriteInteger;
    static Function *TheWriteString;
    static Function *TheWriteChar;
    static Function *TheWriteByte;

    static Function *TheReadInteger;
    static Function *TheReadString;
    static Function *TheReadChar;
    static Function *TheReadByte;

    static Function *TheExtend;
    static Function *TheShrink;
    
    static Function *TheStrCat;
    static Function *TheStrCmp;
    static Function *TheStrCpy;
    static Function *TheStrLen;
    
    static Type *i1;
    static Type *i8;
    static Type *i32;
    static Type *i64;

    static Type *i8_ptr;
    static Type *void_type;


    static ConstantInt* c8(char c) {
      return ConstantInt::get(TheContext, APInt(8, c, true));
    }
    static ConstantInt* c64(long long n) {
      return ConstantInt::get(TheContext, APInt(64, n, true));
    }
};

inline std::ostream &operator << ( std::ostream& strm, DataType tt ) {
   return strm << name[tt];
}


inline std::ostream &operator<<(std::ostream &out, const AST &ast) {
  ast.printAST(out);
  return out;
}

/* Here we define Expressions*/

class Expr : public AST {
    public:
        void check_type(DataType expected_type) {
            sem_analyze();
            if (type->dtype == expected_type) return;

            // Allow bidirectional conversion between int and byte
            if ((expected_type == TYPE_int && type->dtype == TYPE_byte) ||
                (expected_type == TYPE_byte && type->dtype == TYPE_int)) {
                return;
            }

            yyerror("Type mismatch: expected " + name[expected_type] + ", instead got: " + name[type->dtype]);
        }
        void check_type(gen_type *expected_type) {
            sem_analyze();

            // Handle scalar types, allowing for int <-> byte conversion
            if (type->dtype != TYPE_array && expected_type->dtype != TYPE_array) {
                if (type->dtype == expected_type->dtype) return;
                if ((expected_type->dtype == TYPE_int && type->dtype == TYPE_byte) ||
                    (expected_type->dtype == TYPE_byte && type->dtype == TYPE_int)) {
                    return;
                }
            }
            // Handle array types (must match exactly)
            else if (type->dtype == TYPE_array && expected_type->dtype == TYPE_array) {
                if (type->ar->get_type() == expected_type->ar->get_type()) {
                    if (expected_type->ar->size.front() == -1 || type->ar->get_size() == expected_type->ar->get_size()) {
                        return;
                    }
                }
            }

            // If we reach here, it's a mismatch
            std::string expected_str = name[expected_type->dtype];
            if (expected_type->dtype == TYPE_array) expected_str += " of " + name[expected_type->ar->get_type()];

            std::string actual_str = name[type->dtype];
            if (type->dtype == TYPE_array) actual_str += " of " + name[type->ar->get_type()];

            yyerror("Type mismatch: expected " + expected_str + ", instead got: " + actual_str);
        }

        void check_types(std::vector<gen_type *> types) {
            sem_analyze();
            for (gen_type * t : types) {
                DataType actual_type = this->type->dtype;
                DataType expected_type = t->dtype;

                // Exact match
                if (actual_type == expected_type) {
                    // For arrays, do a deep check
                    if (actual_type == TYPE_array) {
                        if (this->type->ar->get_type() == t->ar->get_type() && this->type->ar->get_size() == t->ar->get_size()) {
                            return; // Match found
                        }
                    } else {
                        return; // Scalar match found
                    }
                } 
                // Bidirectional conversion for scalars
                else if ((expected_type == TYPE_int && actual_type == TYPE_byte) ||
                        (expected_type == TYPE_byte && actual_type == TYPE_int)) {
                    if (actual_type != TYPE_array && expected_type != TYPE_array) {
                        return; // Match found
                    }
                }
            }

            // If loop finishes without returning, no match was found
            yyerror("Type mismatch");
        }
        bool isConst() const {return is_const;}
        gen_type * getType() {return type;}
        int getVal() const {return val;}

        bool isExpr() {return is_expr;}
        bool isLval() {return is_l_val;}

        /*This func is used to change the field is_expr if an l_val is used as an expression
          so we should get its value  or as a real l_val so we should get its memory address.
          It will only be implemented on L_val*/
        virtual void fromExpr(bool c) {}


    protected:
        gen_type *type;
        bool is_const;
        int val;
        // Used to determine wether l_val is used as lval or rval.
        bool is_expr;
        bool is_l_val = false;
};


class SimpleStmt : public AST {
    public:
        void set_type(DataType t) {calle_type = t;}
    protected:
        DataType calle_type;
};

class Id : public Expr {
    public:
        Id(std::string i): id(i) {std::clog << "Creating node for Id" << std::endl;}

        void printAST(std::ostream &out) const override {
          out << "Id(" << id << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Id with name: " << id << std::endl;
            STEntry *e = st.lookup(id);
            type = new gen_type(*e->type);
            std::clog << "Ending sem analysis for node: Id with name: " << id << std::endl;
        }

        virtual Value* igen() const override {
            std::clog << "Started igen for node: Id with name: " << id << std::endl;
            
            AllocaInst *A = lookup(id);

            std::string var_name = id + "_";

            std::clog << "Ended igen for node: Id with name: " << id << std::endl;


            return Builder.CreateLoad(A->getAllocatedType(), A, var_name);
        }
        

        std::string get_id() {return id;}
    private:
        std::string id;
};

class Def : public AST {
    /*This class exists only to work as a common type for:
      func_def, func_decl and var_def so as to define LocalDef*/
    public:
        Function *parent_func;
        
        // Collect all variable names referenced in this definition
        virtual void collectReferencedVars(std::set<std::string>& vars) {}
};

class StmtList : public AST {
    public:
        StmtList(): stmt_list() {std::clog << "Creating node for stmt_list" << std::endl;}

        void append(SimpleStmt *s) {stmt_list.push_back(s); }


        void printAST(std::ostream &out) const override {
          out << "StmtList(";
          bool first = true;
          for (const auto &s : stmt_list) {  
            if (!first) out << ", ";
            first = false;
            out << *s;
          }
          out << ")";
        }
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: StmtList " << std::endl;
            for (SimpleStmt *stmt : stmt_list) {
                stmt->set_type(calle_type);
                stmt->sem_analyze();
            }
            std::clog << "Ended sem analysis for node: StmtList " << std::endl;
        }
        void set_type(DataType t) {calle_type = t;}

        virtual Value *igen() const override {
            std::clog << "Started igen for node: StmtList " << std::endl;

            for(SimpleStmt *stmt: stmt_list) stmt->igen();


            std::clog << "Ended igen for node: StmtList " << std::endl;
            return nullptr;
        }
    private:
        std::vector<SimpleStmt *> stmt_list;
        DataType calle_type;
};

class Stmt : public AST {
    public:
        Stmt(StmtList *l): stmt_list(l) {std::clog << "Creating node for stmt" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Stmt("<< *stmt_list << ")";
        }
        
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Stmt" << std::endl;
            stmt_list->set_type(calle_type);
            stmt_list->sem_analyze();
            std::clog << "Ended sem analysis for node: Stmt" << std::endl;
        }
        void set_type(DataType t) {calle_type = t;}
        
        virtual Value *igen() const override {
            std::clog << "Started igen for node: Stmt" << std::endl;


            stmt_list->igen();

            std::clog << "Ended for node: Stmt" << std::endl;
            return nullptr;
        }

    private:
        StmtList *stmt_list;
        DataType calle_type;

};


class Block : public AST {
    public:
        Block(Stmt *s): stmt(s) {std::clog << "Creating node for block" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Block(" << *stmt << ")";
        }
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Block" << std::endl;
            stmt->set_type(calle_type);
            stmt->sem_analyze();
            std::clog << "Ended sem analysis for node: Block" << std::endl;
        }

        void set_type(DataType t) {calle_type = t;}
        
        virtual Value *igen() const override {
            std::clog << "Started igen for node: Block" << std::endl;
            stmt->igen();
            std::clog << "Ended igen for node: Block" << std::endl;

            return nullptr;
        }



    private:
        Stmt *stmt;
        //It is needed for type checking on stmts exit and return
        DataType calle_type;
        Function *parent_func;
};


/*Subclasses of Expr*/

class Int : public Expr {
    public:
        Int(long long n): num(n) {
            is_const = true; val = n; is_expr = true;
            std::clog << "Creating node for Int" << std::endl;
        }

        void printAST(std::ostream &out) const override {
            out << "Int(" << num << ")";
        }
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Int with val: "<< num << std::endl;
            type = new gen_type(TYPE_int);
            std::clog << "Ended sem analysis for node: Int with val: "<< num << std::endl;
        }

        virtual Value* igen() const override {
            std::clog << "Igen for node: Int with val: "<< num << std::endl;

            return c64(num);
        }


        long long value() {return num;}

    private:
        long long num;
};

class Char : public Expr {
    public:
        Char(char c): character(c) {
            is_const = true; is_expr = true;
            std::clog << "Creating node for char" << std::endl;
        }

        void printAST(std::ostream &out) const override {
            out << "Char(" << character << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Char with val: "<< character << std::endl;
            type = new gen_type(TYPE_byte);
            std::clog << "Ended sem analysis for node: Char with val: "<< character << std::endl;
        }

        virtual Value* igen() const override {
            std::clog << "Igen for node: Char with val: "<< character << std::endl;
            return c8(character);
        }

        int value() {return character;}
        
    private:
        char character;
};

class Bool : public Expr {
    public:
        Bool(bool b): boolconst(b) {
            if(b) equiv = '\x01';
            else equiv = '\0';
            is_const = true;
            is_expr = true;
            std::clog << "Creating node for Bool" << std::endl;
        }

        void printAST(std::ostream &out) const override {
            out << "Bool(" << boolconst << ")";
        }
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Bool with val: "<< boolconst << std::endl;
            type = new gen_type(TYPE_byte);
            std::clog << "Ended sem analysis for node: Bool with val: "<< boolconst << std::endl;
        }
        virtual Value* igen() const override {
            std::clog << "Igen for node: Bool with val: "<< boolconst << std::endl;
            return c8(equiv);
        }

    private:
        bool boolconst;
        char equiv;
};


class BinOp : public Expr {
    public:
        BinOp(Expr *e1, std::string o, Expr *e2): expr1(e1), op(o), expr2(e2) {
            is_const = false; is_expr = true;
            std::clog << "Creating node for binop" << std::endl;
        }

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr1 << ", " << *expr2 << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: BinOp with l: "<< *expr1 
                      << " r: " << *expr2 << "and op: " << op << std::endl;
            if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") {
                expr1->check_type(TYPE_int);
                expr2->check_type(TYPE_int);
                type = new gen_type(TYPE_int);
            }
            else if (op == "|" || op == "&") {
                expr1->check_type(TYPE_byte);
                expr2->check_type(TYPE_byte);
                type = new gen_type(TYPE_byte);
            }
            std::clog << "Ended sem analysis for node: BinOp with l: "<< *expr1 
                      << " r: " << *expr2 << "and op: " << op << std::endl;
        }
        virtual Value* igen() const override {
            std::clog << "Igen for node: BinOp with l: "<< *expr1 
                      << " r: " << *expr2 << "and op: " << op << std::endl;
            Value* l = expr1->igen();
            Value* r = expr2->igen();
            
            if (op == "+") return Builder.CreateAdd(l, r, "addtmp");
            else if (op == "-") return Builder.CreateSub(l, r, "subtmp");
            else if (op == "*") return Builder.CreateMul(l, r, "multmp");
            else if (op == "/") return Builder.CreateSDiv(l, r, "divtmp");
            else if (op == "%") return Builder.CreateSRem(l, r, "modtmp");
            else if (op == "|") return Builder.CreateOr(l, r, "ortmp");
            else if (op == "&") return Builder.CreateAnd(l, r, "andtmp");
            
            return nullptr;
        }
    private:
        Expr *expr1;
        std::string op;
        Expr *expr2;
};

class UnOp : public Expr {
    public:
        UnOp(std::string o, Expr *e): op(o), expr(e) {
            is_const = false; is_expr = true;
            std::clog << "Creating node for unop" << std::endl;
        }

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: UnOp with expr: " << *expr 
                      << "and op: " << op << std::endl;


            if (op == "+" || op == "-") {
                expr->check_type(TYPE_int);
                type = new gen_type(TYPE_int);
            }
            else if (op == "!") {
                expr->check_type(TYPE_byte);
                type = new gen_type(TYPE_byte);
            }


            std::clog << "Ended sem analysis for node: UnOp with expr: " << *expr 
                      << "and op: " << op << std::endl;
        }
        virtual Value* igen() const override {
            std::clog << "Igen for node: UnOp with expr: " << *expr 
                      << "and op: " << op << std::endl;


            Value *v = expr->igen();
            if (op == "+") return v;
            else if (op == "-") return Builder.CreateSub(c64(0), v, "unsubtmp");
            else if (op == "!") return Builder.CreateXor(v, c8(1), "unnottmp");

            return nullptr;
        }

    private:
        std::string op;
        Expr *expr; 
};

class L_val : public Expr {
    public:
        L_val(Id * i, std::string v = "",  std::vector<Expr *> e = {}) : id(i), str(v), expr_list(e) {
            if (i == nullptr) {
                is_string = true;
                is_const = true;
            }
            else {
                is_string = false;
                is_const = false;
            }
            is_expr = false;
            is_l_val = true;
            std::clog << "Creating node for l_val" << std::endl;
        }

        void append(Expr *e) {expr_list.push_back(e);}

        void printAST(std::ostream &out) const override {
            out << "L_val(" ;
            if (is_string) out << str;
            else {
                out << *id;
                for (const auto &s : expr_list) {
                    out << "[" << *s << "]";
                }
            }
            out << ")";
        }

        std::string get_id() {return id->get_id();}

        bool isString() {return is_string;}

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: L_val";
            if(!is_string) std::clog << "with Id: " << *id << std::endl;
            else std::clog << "with string: " << str << std::endl;
            if(!is_string) {
                id->sem_analyze();
                type = new gen_type(*id->getType());

                if (!expr_list.empty()) {
                    if (type->dtype != TYPE_array) yyerror("Variable is not an array");
                    else {
                        int counter = 0;
                        int dims = type->ar->size.size();
                        for (Expr *s : expr_list) {
                            s->check_type(TYPE_int);
                            if (s->isConst()) {
                                int index = s->getVal();
                                if(index < 0 || (type->ar->size[counter] != -1 && index >= type->ar->size[counter])) yyerror("Array index out of bounds");
                            }
                            counter++;
                            if (counter > dims){ 
                                yyerror("Array has: " + std::to_string(dims) + " dimensions but " 
                                                    +  std::to_string(counter) + " were given");
                                break;
                            }
                        }

                        // Get the original dimensions and the number of indices used.
                        int original_dims = type->ar->size.size();
                        int accessed_dims = counter;

                        if (accessed_dims < original_dims) {
                            // This is a PARTIAL access (e.g., arr[i] from a 2D arr). The result is still an array.
                            std::vector<int> new_dims;
                            // Copy the remaining (unaccessed) dimensions.
                            for (int i = accessed_dims; i < original_dims; ++i) {
                                new_dims.push_back(type->ar->size[i]);
                            }
                            // Create a new array type with the remaining dimensions.
                            type = new gen_type(type->ar->get_type(), new_dims);
                        } else {
                            // This is a FULL access (e.g., arr[i][j] from a 2D arr). The result is a scalar.
                            type = new gen_type(type->ar->get_type());
                        }
                    }
                }
            }
            else {
                std::vector<int> dim = {(int) str.length()};
                type = new gen_type(TYPE_byte, dim);
                
                // Handle string indexing (e.g., "haha"[2])
                if (!expr_list.empty()) {
                    int counter = 0;
                    for (Expr *s : expr_list) {
                        s->check_type(TYPE_int);
                        if (s->isConst()) {
                            int index = s->getVal();
                            if(index < 0 || index >= (int)str.length()) 
                                yyerror("String index out of bounds");
                        }
                        counter++;
                    }
                    
                    // Strings are 1D arrays, so any indexing results in a scalar byte
                    if (counter == 1) {
                        type = new gen_type(TYPE_byte);
                    } else if (counter > 1) {
                        yyerror("String literals are 1-dimensional, but " + std::to_string(counter) + " indices were given");
                    }
                }
            }

            std::clog << "Ended sem analysis for node: L_val";
            if(!is_string) std::clog << "with Id: " << *id << std::endl;
            else std::clog << "with string: " << str << std::endl;
        }

        virtual Value *igen() const override {
            std::clog << "Igen for node: L_val";
            if(!is_string) std::clog << "with Id: " << *id << std::endl;
            else std::clog << "with string: " << str << std::endl;

            if(is_string) {
                std::string with_null = str + '\0';
                Value *strPtr = Builder.CreateGlobalString(with_null, "lval_str");
                
                // Handle string indexing (e.g., "haha"[2])
                if (!expr_list.empty()) {
                    std::vector<Value *> indices;
                    indices.push_back(c64(0)); // First index to get from global to array
                    
                    for (Expr *expr : expr_list) {
                        Value *index = expr->igen();
                        indices.push_back(index);
                    }
                    
                    // Get the element pointer
                    Value *elemPtr = Builder.CreateInBoundsGEP(
                        ArrayType::get(Type::getInt8Ty(TheContext), str.length() + 1),
                        strPtr, 
                        indices
                    );
                    
                    // If used as an expression, load the value
                    if (is_expr) {
                        return Builder.CreateLoad(Type::getInt8Ty(TheContext), elemPtr, "lval_expr");
                    }
                    return elemPtr;
                }
                
                return strPtr;
            }
            else {
                /*get the pointer*/
                
                AllocaInst *ptr = lookup(id->get_id());
                // Check if it is an array element
                std::vector<Value *> indices;
                
                // We need the 0 index for:
                // 1. Non-pointer types (stack arrays)
                // 2. Captured variables (which are pointers to arrays, not array elements)
                // We DON'T need it for reference parameters (which point to the first element)
                Type *allocType = ptr->getAllocatedType();
                bool isCapturedArrayVar = false;
                
                if (allocType->isPointerTy()) {
                    // Check if this is a captured variable by looking at the current function's details
                    func_details *det = get_parent();
                    if (det && det->captured_types.count(id->get_id()) > 0) {
                        isCapturedArrayVar = true;
                    }
                }
                
                if (!allocType->isPointerTy() || isCapturedArrayVar) {
                    indices.push_back(c64(0));
                }
                
                if(!expr_list.empty()) {
                    std::clog << "ENTERED !expr_list.empty() if for l_val: " << id->get_id()
                              << std::endl;
                    Type *elementType = ptr->getAllocatedType(); 
                    std::clog << "elementType->isPointerTy() = " << elementType->isPointerTy() << std::endl;
                    Value *elemPtr;
                    Type *baseType;
                    for(Expr *expr: expr_list) {
                        
                        Value *index = expr->igen();
                        indices.push_back(index);
                    }
                    if (elementType->isPointerTy()) {
                        Value *base = Builder.CreateLoad(elementType, ptr, "ptrbase");
                        
                        std::clog << "Created Gep instr" << std::endl;
                        func_details *det = get_parent();
                        std::clog << "Found details for func: " << det->name << std::endl;
                        std::clog << "Checking for " << id->get_id() << " in argBaseTypes, count = " << det->argBaseTypes.count(id->get_id()) << std::endl;
                        
                        // Check if this is a captured variable or a regular parameter
                        if (det->argBaseTypes.count(id->get_id()) > 0) {
                            // Regular parameter - use argBaseTypes
                            baseType = det->argBaseTypes[id->get_id()];
                            std::clog << "Found in argBaseTypes!" << std::endl;
                        } else if (det->captured_types.count(id->get_id()) > 0) {
                            // Captured variable - use captured_types
                            std::clog << "Using captured_types for variable: " << id->get_id() << std::endl;
                            baseType = det->captured_types[id->get_id()];
                        } else {
                            std::cerr << "ERROR: Cannot find base type for: " << id->get_id() << std::endl;
                            yyerror("Cannot determine base type for array access");
                            return nullptr;
                        }
                        
                        // For loaded pointers (ref params or captured vars), we don't need the leading 0 index
                        // The loaded pointer already points to the element type
                        std::vector<Value *> gepIndices;
                        if (!indices.empty() && indices[0] == c64(0)) {
                            // Skip the first index if it's 0 (was added for array-to-pointer decay)
                            gepIndices.assign(indices.begin() + 1, indices.end());
                        } else {
                            gepIndices = indices;
                        }
                        
                        elemPtr = Builder.CreateGEP(baseType, base, gepIndices);
                        
                        // Fix: Make sure we're loading the correct element type
                        if (is_expr) {
                            Type *loadType = get_llvm_type(type);
                            return Builder.CreateLoad(loadType, elemPtr, "lval_expr");
                        }
                        return elemPtr;
                    }
                    else {
                        elemPtr = Builder.CreateInBoundsGEP(elementType, ptr, indices);
                        baseType = elementType->getArrayElementType();}
                    if (is_expr) {
                        return Builder.CreateLoad(baseType, elemPtr, "lval_expr");
                    }
                    return elemPtr;
                }
            if(is_expr) {

                std::clog << "ENTERED is_expr if for l_val: " << id->get_id() << std::endl;
                Type *t = ptr->getAllocatedType();

                // Determine if this is a reference parameter OR a captured variable (pointer)
                // by checking the function's parameters or if the alloca is a pointer type
                Function *currentFunc = Builder.GetInsertBlock()->getParent();
                bool isRefParam = false;
                
                // Check if this identifier matches any function parameter that is a pointer type
                for (auto &arg : currentFunc->args()) {
                    if (arg.getName() == id->get_id() && arg.getType()->isPointerTy()) {
                        isRefParam = true;
                        break;
                    }
                }
                
                // Also check if this is a captured variable (local alloca with pointer type)
                // Captured variables are stored as pointers to parent scope variables
                bool isCapturedVar = t->isPointerTy() && !isRefParam;
                
                
                if (isRefParam || isCapturedVar) {
                    if (isCapturedVar) {
                        std::clog << "l_val: " << id->get_id() << " is a captured variable (pointer)" << std::endl;
                    }
                    // For reference parameters or captured variables: load the pointer, then load the value
                    Value *ref_ptr = Builder.CreateLoad(t, ptr, "ref_ptr");
                    
                    // For array reference parameters used in expressions, return the pointer directly
                    if (type->dtype == TYPE_array) {
                        return ref_ptr;
                    }
                    
                    // For scalar reference parameters or captured variables, load the value
                    Type *base_type = get_llvm_type(type);
                    return Builder.CreateLoad(base_type, ref_ptr, "lval_expr");
                } else {
                    std::clog << "l_val: " << id->get_id() << " is not a ref parameter" << std::endl;
                    // For regular variables: load the value directly
                    return Builder.CreateLoad(t, ptr, "lval_expr");
                }
            } else {
                // For l-value access (assignments)
                std::clog << "ENTERED !is_expr if for l_val: " << id->get_id() << std::endl;
                Type *t = ptr->getAllocatedType();
                Function *currentFunc = Builder.GetInsertBlock()->getParent();
                bool isRefParam = false;
                
                // Check if this identifier matches any function parameter that is a pointer type
                for (auto &arg : currentFunc->args()) {
                    if (arg.getName() == id->get_id() && arg.getType()->isPointerTy()) {
                        isRefParam = true;
                        break;
                    }
                }
                
                // Also check if this is a captured variable (local alloca with pointer type)
                bool isCapturedVar = t->isPointerTy() && !isRefParam;
                
                if (isRefParam || isCapturedVar) {
                    if (isCapturedVar) {
                        std::clog << "l_val: " << id->get_id() << " is a captured variable (pointer) for assignment" << std::endl;
                    }
                    // For reference parameters or captured variables: load and return the pointer
                    return Builder.CreateLoad(t, ptr, "ref_ptr");
                } else {
                    // For regular variables: return the alloca
                    return ptr;
                }
            }
                return ptr;
            }
            return nullptr;
        }


        virtual void fromExpr(bool c) override {is_expr = c;}

    private:
        Id *id;
        std::string str;
        bool is_string; 
        std::vector<Expr *> expr_list;


};

class ExprAux : public Expr {
    public:
        ExprAux(): expr_list() {std::clog << "Creating node for expraux" << std::endl;}
        
        void append(Expr *e) {expr_list.push_back(e); }

        void insert_front(Expr *e) {expr_list.insert(expr_list.begin(), e);}

        void printAST(std::ostream &out) const override {
          out << "ExprAux(";
          bool first = true;
          for (const auto &s : expr_list) {  
            if (!first) out << ", ";
            first = false;
            out << *s;
          }
          out << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Expraux" << std::endl;
            for (Expr *e: expr_list) e->sem_analyze();
            std::clog << "Ended sem analysis for node: Expraux" << std::endl;
        }
        

        std::vector<Expr *> expr_list;
};

class FuncCall : public Expr {
    public:
        FuncCall(Id *v, ExprAux * e = nullptr): var(v), expr_list(e) {
            is_const = false;
            std::clog << "Creating node for funccall" << std::endl;
        }

        void printAST(std::ostream &out) const override {
            out << "FuncCall(" << *var;
            if(expr_list != nullptr) out << *expr_list;
            out << ")";
        }
        
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: FuncCall with name: " << var->get_id() << std::endl;
            
            STEntry *e = st.lookup(var->get_id());
            type = new gen_type(e->f->func_type());   //get the return type from the st entry
            if (type->dtype == TYPE_void) yyerror("Procedure has been called as a function");
            
            if (expr_list != nullptr){

                expr_list->sem_analyze();

                /*Check if number of parameters is the same*/
                int param_num = expr_list->expr_list.size();
                int correct_num = e->f->par_num();
                if (param_num != correct_num) yyerror("Wrong number of parameters");
                
                /*Check parameter types*/
                for (int i = 0; i < correct_num; i++) {
                    gen_type *correct = std::get<1>(e->f->params[i]);
                    expr_list->expr_list[i]->check_type(correct);

                    /*Check if it is by reference or value*/
                    bool is_ref = std::get<2>(e->f->params[i]);

                    bool cond = is_ref && (expr_list->expr_list[i]->isLval() == false);
                    if (cond) {
                        /*if is_ref and is expr are both true, that means that the paramter passed is
                          an expression while it should have been an l_val */
                        //std::cout << "From func def: " << is_ref << " from par: " << expr_list->expr_list[i]->isRef() << std::endl;
                        yyerror("In func: " + var->get_id() + " parameter: " + std::to_string(i+1) + " was expecting pass by reference, instead got an expression");
                    }
                    else if (is_ref && (expr_list->expr_list[i]->isLval() == true)) {
                        /*this has as a side-effect (we want to have that side-effect) of making an l_val field is_expr false
                          because by default, since the parameters are expressions, it will always be true. However if it is a by
                          reference parameter we want to provide the memory address so we have to set it false. Check L_val::igen() for more*/
                        expr_list->expr_list[i]->fromExpr(false);
                    }
                }
            }

            std::clog << "Ended sem analysis for node: FuncCall with name: " << var->get_id() << std::endl;
        }

        virtual Value *igen() const override {
            std::clog << "Started igen for node: FuncCall with name: " << var->get_id() << std::endl;

            int s = func_names.size();
            std::string prefix;
            if (!is_libfunc(var->get_id())) prefix = s <= 1? "": func_names[s-2].back()->name + "_";
            else prefix = "";
            if (prefix == var->get_id() + "_") {
                s--;
                if (!is_libfunc(var->get_id())) prefix = s <= 1? "": func_names[s-2].back()->name + "_";
                else prefix = "";
            } 
            
            std::string name = prefix + var->get_id();

            std::clog << "Func called is named: " << name << std::endl;

            Function *F;
            int i = s - 3; //go to the second to last possible parent (because s-1)
            while(!(F = TheModule->getFunction(name))) {
                if (i == -1) name = "_" + var->get_id();
                else if (i == -2) yyerror("Function definition not found");
                else name = func_names[i].back()->name + "_" +var->get_id();
                i--;
            }
            std::clog << "Function found in module" << std::endl;
            
            // Prepare arguments vector
            std::vector<Value *> ArgsV;
            
            // First, add explicit parameters from the call
            if (expr_list != nullptr) {
                std::vector<Expr *> params = expr_list->expr_list;
                
                for (unsigned i = 0, e = params.size(); i != e; ++i) {
                    ArgsV.push_back(params[i]->igen());
                    if (!ArgsV.back())
                        return nullptr;
                }
            }
            
            // Now, if this is a nested function call, we need to pass captured variables
            // Check the captured_variables map for this function
            std::string func_name = std::string(F->getName());
            std::clog << "Looking for captured variables for function: " << func_name << std::endl;
            std::clog << "captured_variables map has " << captured_variables.size() << " entries:" << std::endl;
            for (const auto& entry : captured_variables) {
                std::clog << "  - '" << entry.first << "' has " << entry.second.size() << " captures" << std::endl;
            }
            
            if (captured_variables.count(func_name) > 0 && !captured_variables[func_name].empty()) {
                const auto& caps = captured_variables[func_name];
                
                // Pass each captured variable
                for (const auto& cap_pair : caps) {
                    const std::string& cap_var = cap_pair.first;
                    
                    // Look up the variable in the current scope
                    AllocaInst* var_alloca = lookup(cap_var);
                    if (!var_alloca) {
                        std::cerr << "ERROR: Could not find captured variable: " << cap_var << std::endl;
                        yyerror("Captured variable not found: " + cap_var);
                        return nullptr;
                    }
                    
                    // Check if this variable is a captured pointer (its alloca holds a pointer)
                    Type* alloca_type = var_alloca->getAllocatedType();
                    if (alloca_type->isPointerTy()) {
                        // This is a captured variable - load its value (which is itself a pointer) and pass that
                        Value* ptr_val = Builder.CreateLoad(alloca_type, var_alloca, "captured_ptr");
                        ArgsV.push_back(ptr_val);
                    } else {
                        // This is a local variable - pass the alloca pointer itself
                        ArgsV.push_back(static_cast<Value*>(var_alloca));
                    }
                }
            }

            Value *call = Builder.CreateCall(F, ArgsV, "calltmp");
            return call;
        }
    private:
        Id *var;
        ExprAux * expr_list;
};


class Cond : public Expr {
    public:
    private:
};

class ExprCond : public Cond {
    public:
        ExprCond(Expr *e): expr(e) {std::clog << "Creating node for ExprCond" << std::endl;}
        void printAST(std::ostream &out) const override {
            out << "ExprCond(" << *expr <<  ")";
        }

        virtual void sem_analyze() override {

            std::clog << "Started sem analysis for node: ExprCond with expr: " << *expr << std::endl;
            std::vector<gen_type *> type_list = {new gen_type(TYPE_int), new gen_type(TYPE_byte)};
            expr->check_types(type_list);
            type = new gen_type(TYPE_bool);

            std::clog << "Ended sem analysis for node: ExprCond with expr: " << *expr << std::endl;
        }

        virtual Value * igen() const override {
            std::clog << "Started igen for node: ExprCond with expr: " << *expr << std::endl;
            

            Value * v = expr->igen();
            Type * t = v->getType();
            if (t == i64) {
                /*This is the case where the expression is arithmetic*/
                Value * c = Builder.CreateICmpNE(v, c64(0), "exprtocond");
                //convert to type byte
                std::clog << "Ended igen for node: ExprCond with expr: " << *expr << std::endl;
                return Builder.CreateZExt(c, Builder.getInt8Ty(), "expr_int_to_cond_bt");
            }
            /*This is the case where expr is true or false and of type byte*/
            else if (t == i8) {
                v = Builder.CreateICmpNE(v, ConstantInt::get(TheContext, APInt(8, 0, true)), "byte_to_bool");
            }
            std::clog << "Ended igen for node: ExprCond with expr: " << *expr << std::endl;
            return v;
        }

    private:
        Expr *expr;
};


class BinOpCond : public Cond {
    public:
        BinOpCond(Expr *e1, char* o, Expr *e2): expr1(e1), op(o), expr2(e2) {std::clog << "Creating node for binopcond" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr1 << ", " << *expr2 << ")";
        }

        virtual void sem_analyze() override {

            std::clog << "Started sem analysis for node: BinOpCond with l: "<< *expr1 
                      << " r: " << *expr2 << "and op: " << op << std::endl;
            if (op == "=" || op == "<" || op == ">" || op == "<>" || op == "<=" || op == ">=") {
                // Allow comparison between int types or between byte types
                std::vector<gen_type *> allowed_types = {new gen_type(TYPE_int), new gen_type(TYPE_byte)};
                expr1->check_types(allowed_types);
                expr2->check_types(allowed_types);
                
                // Ensure both operands have the same type
                expr1->sem_analyze();
                expr2->sem_analyze();
                if (expr1->getType()->dtype != expr2->getType()->dtype) {
                    yyerror("Type mismatch: comparison operands must have the same type");
                }
                
                type = new gen_type(TYPE_bool);
            }
            else if (op == "and" || op == "or") {
                expr1->check_type(TYPE_bool);
                expr2->check_type(TYPE_bool);
                type = new gen_type(TYPE_bool);
            }

            std::clog << "Ended sem analysis for node: BinOpCond with l: "<< *expr1 
                      << " r: " << *expr2 << "and op: " << op << std::endl;
        }

        virtual Value * igen() const override {
            std::clog << "Started igen for node: BinOpCond with l: "<< *expr1 
                    << " r: " << *expr2 << "and op: " << op << std::endl;

            Value *l = expr1->igen();
            Value *r = expr2->igen();
            
            // Ensure both operands have the same integer type by extending the smaller one.
            if (l->getType() != r->getType()) {
                if (l->getType()->isIntegerTy() && r->getType()->isIntegerTy()) {
                    unsigned lBits = l->getType()->getIntegerBitWidth();
                    unsigned rBits = r->getType()->getIntegerBitWidth();
                    
                    if (lBits > rBits) {
                        r = Builder.CreateZExt(r, l->getType(), "zexttmp");
                    } else {  
                        l = Builder.CreateZExt(l, r->getType(), "zexttmp");
                    }
                }
            }
            
            if (op == "=") return Builder.CreateICmpEQ(l, r, "eqtmp");
            else if (op == "<") return Builder.CreateICmpSLT(l, r, "lttmp");
            else if (op == ">") return Builder.CreateICmpSGT(l, r, "gttmp");
            else if (op == "<>") return Builder.CreateICmpNE(l, r, "neqtmp");
            else if (op == "<=") return Builder.CreateICmpSLE(l, r, "leqtmp");
            else if (op == ">=") return Builder.CreateICmpSGE(l, r, "geqtmp");
            else if (op == "and") return Builder.CreateAnd(l, r, "condandtmp");
            else if (op == "or") return Builder.CreateOr(l, r, "condortmp");

            std::clog << "Ended igen for node: BinOpCond with l: "<< *expr1 
                    << " r: " << *expr2 << "and op: " << op << std::endl;

            return nullptr;
        }

    private:
        Expr *expr1;
        std::string op;
        Expr *expr2;
};

class UnOpCond : public Cond {
    public:
        UnOpCond(std::string o, Expr *e): op(o), expr(e) {std::clog << "Creating node for unopcond" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr << ")";
        }

        virtual void sem_analyze() override {

            std::clog << "Started sem analysis for node: UnOpCond with expr: " << *expr 
                      << "and op: " << op << std::endl;
            expr->check_type(TYPE_bool);
            type = new gen_type(TYPE_bool);
            

            std::clog << "Ended sem analysis for node: UnOpCond with expr: " << *expr 
                      << "and op: " << op << std::endl;
        }
        
        virtual Value * igen() const {
            std::clog << "Started igen for node: UnOpCond with expr: " << *expr 
                    << "and op: " << op << std::endl;

            Value *v = expr->igen();

            std::clog << "Ended igen for node: UnOpCond with expr: " << *expr 
                    << "and op: " << op << std::endl;

            return Builder.CreateXor(v, ConstantInt::get(TheContext, APInt(1, 1, false)), "condnottmp");
        }

    private:
        std::string op;
        Expr *expr;
};

/*Subclasses of Stmt*/

class Skip : public SimpleStmt {
    public:
        Skip() {std::clog << "Creating node for skip" << std::endl;}
        void printAST(std::ostream &out) const override {
            out << "Skip()";
        }
        
        virtual Value *igen() const override {
            /*Skip command does nothing*/
            return nullptr;
        }

    private:
};

class Exit : public SimpleStmt {
    public:
        Exit() {std::clog << "Creating node for exit" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Exit()";
        }
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Exit" << std::endl;
            if (calle_type != TYPE_void) yyerror("Can't exit from a function, expected a return statement.");

            std::clog << "Ended sem analysis for node: Exit" << std::endl;
        }

        virtual Value *igen() const override {
            std::clog << "igen for node: Exit" << std::endl;
            // Check if we're in the main function
            Function *TheFunction = Builder.GetInsertBlock()->getParent();
            if (TheFunction->getName() == "main" && !TheFunction->getReturnType()->isVoidTy()) {
                // Main function returns int (i32), so return 0
                return Builder.CreateRet(ConstantInt::get(TheContext, APInt(32, 0, true)));
            }
            return Builder.CreateRetVoid();
        }        
    private:
};

class Return : public SimpleStmt {
    public:
        Return(Expr *e): expr(e) {std::clog << "Creating node for return" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Return(" << *expr << ")";
        }
        
        virtual void sem_analyze() override {

            std::clog << "Started sem analysis for node: Return with expr: " << *expr << std::endl;
            expr->check_type(calle_type);

            std::clog << "Ended sem analysis for node: Return with expr: " << *expr << std::endl;
        }
        
        virtual Value *igen() const override {
            std::clog << "Started igen for node: Return with expr: " << *expr << std::endl;
            Value *v = expr->igen();
            std::clog << "Ended igen for node: Return with expr: " << *expr << std::endl;
            return Builder.CreateRet(v);
        }

    private:
        Expr *expr;
};

class Loop : public SimpleStmt {
    public:
        Loop(Block *b, Id *v = nullptr): name(v), block(b) {std::clog << "Creating node for loop" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Loop(" ;
            if (name != nullptr) out << *name << ", ";
            out << *block << ")";
        }
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Loop";
            if(name != nullptr) std::clog << " with name: " << name->get_id();
            std::clog << std::endl;
            lp.insert((name != nullptr)? name->get_id(): "");
            block->set_type(calle_type);
            block->sem_analyze();
            lp.pop((name != nullptr)? name->get_id(): "");
            std::clog << "Ended sem analysis for node: Loop";
            if(name != nullptr) std::clog << " with name: " << name->get_id();
            std::clog << std::endl;
        }
        
        virtual Value *igen() const override {
            std::clog << "Started igen for node: Loop";
            if(name != nullptr) std::clog << " with name: " << name->get_id();
            std::clog << std::endl;
            BasicBlock *PrevBB = Builder.GetInsertBlock();
            Function *TheFunction = PrevBB->getParent();

            std::string loop_name = "loop_" + ((name!=nullptr) ? name->get_id() : "");
            BasicBlock *LoopBB = BasicBlock::Create(TheContext, loop_name, TheFunction);
            BasicBlock *EndLoopBB = BasicBlock::Create(TheContext, "end_" + loop_name);
            
            /*We push every loop to the stack*/
            Loops.push_back(std::make_pair(LoopBB, EndLoopBB));
            if (name != nullptr) {
                /*Loop has a name, so we push it in the named stack */
                NamedLoops[name->get_id()] = std::make_pair(LoopBB, EndLoopBB);
            }
            Builder.CreateBr(LoopBB);
            Builder.SetInsertPoint(LoopBB);
            block->igen();
            Builder.CreateBr(LoopBB);

            TheFunction->insert(TheFunction->end(), EndLoopBB);
            Builder.SetInsertPoint(EndLoopBB);
            /*Now, pop the loops from the stacks*/
            Loops.pop_back();
            if (name != nullptr) NamedLoops.erase(name->get_id());

            std::clog << "Ended igen for node: Loop";
            if(name != nullptr) std::clog << " with name: " << name->get_id();
            std::clog << std::endl;
            return nullptr;
        }

    private:
        Id *name;
        Block *block;
};

class Break : public SimpleStmt {
    public:
        Break(Id *n = nullptr): name(n) {std::clog << "Creating node for break" << std::endl;}
        void printAST(std::ostream &out) const override {
            
            out << "Break(";
            if(name != nullptr) out << *name;
            out << ")";
        }
        virtual void sem_analyze() {
            std::clog << "Started sem analysis for node: Break";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl; 
            lp.break_loop((name != nullptr)? name->get_id(): "");
            std::clog << "Ended sem analysis for node: Break";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl;
        }

        virtual Value *igen() const override {
            std::clog << "Started igen for node: Break";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl; 
            BasicBlock * BreakDest;
            if (name != nullptr) {
                BreakDest = NamedLoops.at(name->get_id()).second;
            }
            else {
                BreakDest = Loops.back().second;
            }
            Builder.CreateBr(BreakDest);

            std::clog << "Ended igen for node: Break";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl; 
            return nullptr;
        }

    private:
        Id *name;
};

class Continue : public SimpleStmt {
    public:
        Continue(Id *n = nullptr): name(n) {std::clog << "Creating node for continue" << std::endl;}
        void printAST(std::ostream &out) const override {
            out << "Continue(";
            if(name != nullptr) out << *name;
            out << ")";
        }

        virtual void sem_analyze() {
            std::clog << "Started sem analysis for node: Continue";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl;
            lp.cont_loop((name != nullptr)? name->get_id(): "");
            std::clog << "Ended sem analysis for node: Continue";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl;
        }

        virtual Value *igen() const override {
            std::clog << "Started igen for node: Continue";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl;
            BasicBlock * ContDest;
            if (name != nullptr) {
                ContDest = NamedLoops.at(name->get_id()).first;
            }
            else {
                ContDest = Loops.back().first;
            }
            Builder.CreateBr(ContDest);
            std::clog << "Ended igen for node: Continue";
            if(name != nullptr) std::clog << " with name " << name->get_id();
            std::clog << std::endl;
            return nullptr;
        }

    private:
        Id *name;
};


class ProcCall : public SimpleStmt {
    public:
        ProcCall(Id *v, ExprAux * e = nullptr): var(v), expr_list(e) {std::clog << "Creating node for proccall" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "ProcCall(" << *var;
            if(expr_list != nullptr) out << ", " << *expr_list;
            out << ")";
        }
        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: ProcCall with name: " << var->get_id() << std::endl;
            
            STEntry *e = st.lookup(var->get_id());
            if (e->f->func_type() != TYPE_void) yyerror("Function has been called as a procedure");
            
            
            if (expr_list != nullptr) {

                expr_list->sem_analyze();

                /*Check if number of parameters is the same*/
                int param_num = expr_list->expr_list.size();
                int correct_num = e->f->par_num();
                if (param_num != correct_num) yyerror("Wrong number of parameters");
                
                /*Check parameter types*/
                for (int i = 0; i < correct_num; i++) {
                    gen_type *correct = std::get<1>(e->f->params[i]);

                    expr_list->expr_list[i]->check_type(correct);

                    /*Check if it is by reference or value*/
                    bool is_ref = std::get<2>(e->f->params[i]);
                    bool cond = is_ref && (expr_list->expr_list[i]->isLval() == false);
                    if (cond) {
                        /*if is_ref and is expr are both true, that means that the paramter passed is
                          an expression while it should have been an l_val */
                        //std::cout << "From func def: " << is_ref << " from par: " << expr_list->expr_list[i]->isRef() << std::endl;
                        yyerror("In proc: " + var->get_id() + ", parameter: " + std::to_string(i+1) + " was expecting pass by reference, instead got an expression");
                    }
                    else if (is_ref && (expr_list->expr_list[i]->isLval() == true)) {
                        /*this has as a side-effect (we want to have that side-effect) of making an l_val field is_expr false
                          because by default, since the parameters are expressions, it will always be true. However if it is a by
                          reference parameter we want to provide the memory address so we have to set it false. Check L_val::igen() for more*/
                        expr_list->expr_list[i]->fromExpr(false);
                    }
                }
            }
           std::clog << "Ended sem analysis for node: ProcCall with name: " << var->get_id() << std::endl;
        }

        virtual Value *igen() const override {
            std::clog << "Started igen for node: ProcCall with name: " << var->get_id() << std::endl;

            int s = func_names.size();
            std::string prefix;
            if (!is_libfunc(var->get_id())) prefix = s <= 1? "": func_names[s-2].back()->name + "_";
            else prefix = "";
            if (prefix == var->get_id() + "_") {
                s--;
                if (!is_libfunc(var->get_id())) prefix = s <= 1? "": func_names[s-2].back()->name + "_";
                else prefix = "";
            } 

            std::string name = prefix + var->get_id();

            std::clog << "Func called has name: " << name << std::endl;
            Function *F;
            int i = s - 3; //go to the second to last possible parent (because s-1)
            while(!(F = TheModule->getFunction(name))) {
                if (i == -1) name = "_" + var->get_id();
                else if (i == -2) yyerror("Function definition not found");
                else name = func_names[i].back()->name + "_" +var->get_id();
                i--;
            }

            if (expr_list != nullptr) {
                std::vector<Expr *> params = expr_list->expr_list;
                std::vector<Value *> ArgsV;
                
                for (unsigned i = 0, e = params.size(); i != e; ++i) {
                    ArgsV.push_back(params[i]->igen());
                    if (!ArgsV.back())
                      return nullptr;
                }
                
                // Now, if this is a nested function call, we need to pass captured variables
                // Check the captured_variables map for this function
                std::string func_name = std::string(F->getName());
                
                if (captured_variables.count(func_name) > 0 && !captured_variables[func_name].empty()) {
                    const auto& caps = captured_variables[func_name];
                    
                    // Pass each captured variable
                    for (const auto& cap_pair : caps) {
                        const std::string& cap_var = cap_pair.first;
                        
                        // Look up the variable in the current scope
                        AllocaInst* var_alloca = lookup(cap_var);
                        if (!var_alloca) {
                            std::cerr << "ERROR: Could not find captured variable: " << cap_var << std::endl;
                            yyerror("Captured variable not found: " + cap_var);
                            return nullptr;
                        }
                        
                        // Check if this variable is a captured pointer (its alloca holds a pointer)
                        Type* alloca_type = var_alloca->getAllocatedType();
                        if (alloca_type->isPointerTy()) {
                            // This is a captured variable - load its value (which is itself a pointer) and pass that
                            Value* ptr_val = Builder.CreateLoad(alloca_type, var_alloca, "captured_ptr");
                            ArgsV.push_back(ptr_val);
                        } else {
                            // This is a local variable - pass the alloca pointer itself
                            ArgsV.push_back(static_cast<Value*>(var_alloca));
                        }
                    }
                }
                return Builder.CreateCall(F, ArgsV);

            }

            // Handle case with no explicit parameters - but may still need captured variables
            std::vector<Value *> ArgsV;
            
            // Check for captured variables even when there are no explicit parameters
            std::string func_name = std::string(F->getName());
            
            if (captured_variables.count(func_name) > 0 && !captured_variables[func_name].empty()) {
                const auto& caps = captured_variables[func_name];
                
                // Pass each captured variable
                for (const auto& cap_pair : caps) {
                    const std::string& cap_var = cap_pair.first;
                    
                    // Look up the variable in the current scope
                    AllocaInst* var_alloca = lookup(cap_var);
                    if (!var_alloca) {
                        std::cerr << "ERROR: Could not find captured variable: " << cap_var << std::endl;
                        yyerror("Captured variable not found: " + cap_var);
                        return nullptr;
                    }
                    
                    // Check if this variable is a captured pointer (its alloca holds a pointer)
                    Type* alloca_type = var_alloca->getAllocatedType();
                    if (alloca_type->isPointerTy()) {
                        // This is a captured variable - load its value (which is itself a pointer) and pass that
                        Value* ptr_val = Builder.CreateLoad(alloca_type, var_alloca, "captured_ptr");
                        ArgsV.push_back(ptr_val);
                    } else {
                        // This is a local variable - pass the alloca pointer itself
                        ArgsV.push_back(static_cast<Value*>(var_alloca));
                    }
                }
            }
            
            return Builder.CreateCall(F, ArgsV);
        }

    private:
        Id *var;
        ExprAux * expr_list;

};

class Assign : public SimpleStmt {
    public:
        Assign(L_val *l, Expr *e) : l_val(l), expr(e) {std::clog << "Creating node for assign" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Assign(" << *l_val << ", " << *expr << ")"; 
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Assign with lhs: " << *l_val
                      << " and rhs: " << *expr << std::endl;
            
            l_val->sem_analyze();
            
            // Remove the overly restrictive check that prevented assignment to value parameters
            // Value parameters are local copies and should be modifiable
            // Only reference parameters and local variables should be assignable
            
            expr->check_type(l_val->getType());
            std::clog << "Ended sem analysis for node: Assign with lhs: " << *l_val
                      << " and rhs: " << *expr << std::endl; 
        }

        virtual Value *igen() const override {
            std::clog << "Started igen for node: Assign with lhs: " << *l_val
                      << " and rhs: " << *expr << std::endl;
            Value *lhs = l_val->igen();
            Value *rhs = expr->igen();
            Builder.CreateStore(rhs, lhs);

            std::clog << "Ended igen for node: Assign with lhs: " << *l_val
                      << " and rhs: " << *expr << std::endl;
            return nullptr;
        }

    private:
        L_val *l_val;
        Expr *expr; 
};

class Elif : public AST {
    public:
        Elif(Cond * c, Block *b) : cond(c), block(b) {std::clog << "Creating node for elif" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Elif(" << *cond << ", " << *block << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: Elif with cond: " << *cond
                      << " and block:" << *block << std::endl;
            cond->check_type(TYPE_bool);
            block->set_type(callee_type);
            block->sem_analyze();

            std::clog << "Started sem analysis for node: Elif with cond: " << *cond
                      << " and block:" << *block << std::endl;
        }

        void set_type(DataType t) {callee_type = t;}


        std::pair<Cond *, Block *> get_elif() {
            return std::make_pair(cond, block);
        }

    private:
        Cond *cond;
        Block *block;
        DataType callee_type;
};

class IfAux : public AST {
    public:
        IfAux() : elif_list() {std::clog << "Creating node for ifaux" << std::endl;}
        
        void append(Elif *e) {elif_list.push_back(e); }


        void printAST(std::ostream &out) const override {
          out << "IfAux(";
          bool first = true;
          for (const auto &s : elif_list) {  
            if (!first) out << ", ";
            first = false;
            out << *s;
          }
          out << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: IfAux" << std::endl;
            for (const auto &elif: elif_list) {
                elif->set_type(calle_type);
                elif->sem_analyze();
            }
            std::clog << "Ended sem analysis for node: IfAux" << std::endl;
        }
        void set_type(DataType t) {calle_type = t;}

        std::vector<std::pair<Cond *, Block *>> get_elifs() {
            std::vector<std::pair<Cond *, Block *>> res = {};
            for(Elif * e: elif_list) res.push_back(e->get_elif());
            return res;
        }
        
    private:
        std::vector<Elif *> elif_list;
        DataType calle_type;
};

class If : public SimpleStmt {
    public:
        /*We assume that the statement of else is given as a 2nd param,
          and if there are any elifs they are the last parameters.*/
        If(Cond *c1, Block *b1, Block *b2 = nullptr, IfAux *el = nullptr):
         cond(c1), block1(b1), block2(b2), elif_list(el) {std::clog << "Creating node for If" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "If(" << *cond << ", " << *block1 ;
            if(elif_list != nullptr) out << ", " << *elif_list;
            if(block2 != nullptr) out << ", " << *block2;
            out << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analysis for node: If with cond: " << *cond
                      << " and block: " << *block1 << std::endl;
            cond->check_type(TYPE_bool);

            block1->set_type(calle_type);
            block1->sem_analyze();
            if (block2 != nullptr) {
                block2->set_type(calle_type);
                block2->sem_analyze();
            }
            if(elif_list != nullptr) {
                elif_list->set_type(calle_type);
                elif_list->sem_analyze();
            }
            std::clog << "Ended sem analysis for node: If with cond: " << *cond
                      << " and block: " << *block1 << std::endl;
        }

        virtual Value *igen() const override {
            std::clog << "Started igen for node: If with cond: " << *cond
                      << " and block: " << *block1 << std::endl;
            Value *c = cond->igen();
            std::clog << "Completed condition igen" << std::endl;


            Value *if_cond = Builder.CreateICmpNE(c, ConstantInt::get(TheContext, APInt(1, 0, true)), "if_cond");
            Function *TheFunction = Builder.GetInsertBlock()->getParent();
            BasicBlock *ThenBB =
              BasicBlock::Create(TheContext, "then", TheFunction);
            BasicBlock *ElseBB =
              BasicBlock::Create(TheContext, "else", TheFunction);
            BasicBlock *EndIfBB =
              BasicBlock::Create(TheContext, "endif", TheFunction);

            // If statements are for control flow and do not produce values.
            // PHI node merging is not needed.
            //bool needsValueMerge = false;

            if (elif_list != nullptr) {
                std::vector<std::pair<Cond *, Block *>> elifs = elif_list->get_elifs();
                if (!elifs.empty()) {
                    std::vector<BasicBlock *> ElifBBs;
                    std::vector<BasicBlock *> ThenElifBBs;
                    for (unsigned int i = 0; i < elifs.size(); i++) {
                        ElifBBs.push_back(BasicBlock::Create(TheContext, "elif", TheFunction));
                        ThenElifBBs.push_back(BasicBlock::Create(TheContext, "then", TheFunction));
                    }

                    Builder.CreateCondBr(if_cond, ThenBB, ElifBBs[0]);

                    Builder.SetInsertPoint(ThenBB);
                    block1->igen(); // Generate code, ignore null return value
                    if (!Builder.GetInsertBlock()->getTerminator()) {
                        Builder.CreateBr(EndIfBB);
                    }

                    for (unsigned int i = 0; i < elifs.size(); i++) {
                        Builder.SetInsertPoint(ElifBBs[i]);
                        Value *elif_c = elifs[i].first->igen();
                        Value *elif_cond = Builder.CreateICmpNE(elif_c, ConstantInt::get(TheContext, APInt(1, 0, true)), "elif_cond");
                        if (i < elifs.size() - 1) {
                            Builder.CreateCondBr(elif_cond, ThenElifBBs[i], ElifBBs[i+1]);
                        }
                        else {
                            Builder.CreateCondBr(elif_cond, ThenElifBBs[i], ElseBB);
                        }

                        Builder.SetInsertPoint(ThenElifBBs[i]);
                        elifs[i].second->igen(); // Generate code, ignore null return value
                        if (!Builder.GetInsertBlock()->getTerminator()) {
                            Builder.CreateBr(EndIfBB);
                        }
                    }
                } else {
                    // This handles the case of an if-else with no elifs.
                    Builder.CreateCondBr(if_cond, ThenBB, ElseBB);
                    Builder.SetInsertPoint(ThenBB);
                    block1->igen(); // Generate code, ignore null return value
                    if (!Builder.GetInsertBlock()->getTerminator()) {
                        Builder.CreateBr(EndIfBB);
                    }
                }
            }
            else {
                Builder.CreateCondBr(if_cond, ThenBB, ElseBB);

                Builder.SetInsertPoint(ThenBB);
                block1->igen(); // Generate code, ignore null return value
                if (!Builder.GetInsertBlock()->getTerminator()) {
                    Builder.CreateBr(EndIfBB);
                }
            }

            /*Else branch code*/
            Builder.SetInsertPoint(ElseBB);
            if (block2 != nullptr) {
                block2->igen(); // Generate code, ignore null return value
            }
            if (!Builder.GetInsertBlock()->getTerminator()) {
                Builder.CreateBr(EndIfBB);
            }

            // Only set InsertPoint at EndIfBB if some blocks actually branch into it.
            if (!EndIfBB->hasNPredecessors(0)) {
                Builder.SetInsertPoint(EndIfBB);
                // No PHI node needed as if-statements don't produce values.
            } else {
                // If EndIfBB has no predecessors, it means all paths returned.
                // The block is dead, so we should add an unreachable instruction
                // to make the IR valid.
                if (EndIfBB->getParent())
                    Builder.SetInsertPoint(EndIfBB);
                else
                    TheFunction->insert(TheFunction->end(), EndIfBB);
                Builder.CreateUnreachable();
            }
            return nullptr;
        }

    private:
        Cond *cond;
        Block *block1;
        Block *block2;
        IfAux *elif_list;
};



/* We implement the remaining grammar rules*/


class Type_ : public AST {
    public:
        Type_(DataType dt, std::vector<Int *> d = {}): dtype(dt), dim(d) {std::clog << "Creating node for type" << std::endl;}

        void append(Int * d) {dim.push_back(d);}

        void printAST(std::ostream &out) const override {
            out << "Type(" << dtype;
            bool first = true;
            for (const auto &s : dim) {  
                if (first) {out << ", "; first = false;}
                out << "[" << *s << "]";
            }
            out << ")";
        }

        DataType get_type() {return dtype;}
        std::vector<int> get_dims() {
            std::vector<int> res = {};
            for (Int *i: dim) {
                res.push_back(i->value());
            }
            return res;
        }

    private:
        DataType dtype;
        std::vector<Int *> dim;
        
};

class IdAux : public AST {
    public:
        IdAux(Id *v) {var = {}; var.push_back(v); std::clog << "Creating node for Idaux" << std::endl;}

        void append(Id *v) {var.push_back(v);}

        void printAST(std::ostream &out) const override {
            out << "IdAux(";
            bool first = true;
            for (const auto &s : var) {  
                if (first) {out << *s; first = false;}
                else {out << ", " << *s;}
            }
            out << ")";
        }

        int get_size() {return var.size();}

        Id * get(int i) {return var[i];}

    
    private:
        std::vector<Id *> var; 
};

class VarDef : public Def {
    public:
        VarDef(IdAux *v, Type_ *t): var_list(v), type(t) {std::clog << "Creating node for vardef" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "VarDef(" << *var_list << ", " << *type << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analyze for node: VarDef, with vars: " << *var_list 
                      << " and type: " << *type <<std::endl;
            int size = var_list->get_size();
            for (int i = 0; i < size; i++) {
                st.insert(var_list->get(i)->get_id(), type->get_type(), type->get_dims());
            }
            std::clog << "Ended sem analyze for node: VarDef, with vars: " << *var_list 
                      << " and type: " << *type <<std::endl;
        }
        
        virtual Value *igen() const override {
            std::clog << "Started igen for node: VarDef with vars: " << *var_list
                    << " and type: " << *type << std::endl;
            
            Type *llvm_t = get_llvm_type(type->get_type(), type->get_dims());
            int par_no = var_list->get_size();

            // Get the current function's mangled name to use as a prefix.
            func_details* current_func = get_parent();
            std::string prefix = current_func->name + "_";
            
            /*For each variable, create an AllocaInst with its mangled name*/
            for (int i = 0; i < par_no; i++) {
                std::string var_name = var_list->get(i)->get_id();
                std::string mangled_name = prefix + var_name;

                AllocaInst *a = CreateEntryBlockAlloca(parent_func, mangled_name, llvm_t);

                /*Now insert it into the current variable scope using the mangled name*/
                NamedValues.back()[mangled_name] = a;
            }
            
            std::clog << "Ended igen for node: VarDef with vars: " << *var_list
                    << " and type: " << *type << std::endl;
            return nullptr;
        }

    private:
        IdAux *var_list;
        Type_ *type;
};

class ArrayType_ : public AST {
    public:
        ArrayType_(DataType dt, std::vector<Int *> d = {new Int(-1)}): dtype(dt), dim(d) {std::clog << "Creating node for arraytype" << std::endl;}

        void append(Int * d) {dim.push_back(d);}

        void printAST(std::ostream &out) const override {
            out << "ArrayType(" << dtype;
            if (dim.empty()) out << "[]";
            else {
                for (const auto &s : dim) {  
                   out << "[" << *s << "]";
                }
            }
            out << ")";
        }

        DataType get_type() {return dtype;}
        std::vector<int> get_dims() {
            std::vector<int> res = {};
            for (Int *i: dim) {
                res.push_back(i->value());
            }
            return res;
        }

    private:
        DataType dtype;
        std::vector<Int *> dim;
};

class FparType : public AST {
    public:
        FparType(Type_ *t = nullptr, DataType dt = UNDEFINED, ArrayType_ *at = nullptr):
          tp(t), ref_dt(dt) ,atype(at) {std::clog << "Creating node for fpartype" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "FparType(";
            if (tp != nullptr) out << *tp;
            else if (ref_dt != UNDEFINED) out << ref_dt;
            else if (atype != nullptr) out << *atype;
            else {out << "There is a bug in fpar_type";}
            out << ")";
        }
        
        virtual void sem_analyze() override {
            /*This sem_analyze fills the general_type*/
            std::clog << "Started sem analyze for node: FparType" << std::endl;
            if (tp != nullptr) {
                general_type = new gen_type(tp->get_type(), tp->get_dims()); 
                if (general_type->dtype == TYPE_array) ref = true;
                else ref = false;
            }
            else if (ref_dt != UNDEFINED) {general_type = new gen_type(ref_dt); ref = true;}
            else if (atype != nullptr) {general_type = new gen_type(atype->get_type(), atype->get_dims()); ref = true;}
            std::clog << "Ended sem analyze for node: FparType" << std::endl;
        }
        
        gen_type *get_type() {return general_type;}

        bool byRef() {return ref;}


    private:
        /*type is from symbol.hpp and Type is an AST Node*/
        gen_type *general_type;
        Type_ *tp;
        DataType ref_dt;
        ArrayType_ *atype;
        bool ref;
};


class FparDef : public AST {
    public:
        FparDef(IdAux *vl, FparType * fp): var_list(vl), fpar_type(fp) {std::clog << "Creating node for fpardef" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "FparDef("<< *var_list << ", " << *fpar_type << ")";
        }
    
        virtual void sem_analyze() override {
            fpar_type->sem_analyze(); // now general_type is initialized
        }

        gen_type *get_type() {return fpar_type->get_type();}
        bool byRef() {return fpar_type->byRef();}
    
        IdAux *var_list;
        FparType * fpar_type;
};



class HeaderAux : public AST {
    public:
        HeaderAux(): par_list() {std::clog << "Creating node for headeraux" << std::endl;}
        
        void append(FparDef *fp) {par_list.push_back(fp);}

        void insert_front(FparDef *fp) {par_list.insert(par_list.begin(), fp);}

        void printAST(std::ostream &out) const override {
            out << "HeaderAux(";
            bool first = true;
            for (const auto &s : par_list) {  
                if (first) {out << *s; first = false;}
                else {out << ", " << *s;}
            }
            out << ")";
        }
        virtual void sem_analyze() override {
            for (FparDef * d: par_list) d->sem_analyze();
        }

        std::vector<std::tuple<std::string, gen_type *, bool>> get_params() {
            /*This function creates a list of each par definition with a certain type.
            It will be used by Header to create the function st entry*/
            
            std::vector<std::tuple<std::string, gen_type *, bool>> res = {};
            for (FparDef * d: par_list) {
                int length = d->var_list->get_size();
                for (int i = 0; i < length; i++) {
                    res.push_back(std::make_tuple(d->var_list->get(i)->get_id(), d->fpar_type->get_type(), d->fpar_type->byRef()));
                    std::clog << "byRef: " << d->fpar_type->byRef() << std::endl;
                }
            }
            return res;
        }


    private:
        std::vector<FparDef *> par_list;
};



class Header : public AST {
    public:
        Header(Id *v, DataType dt, HeaderAux *h): 
        var(v), dtype(dt), par_list(h) {params = {};std::clog << "Creating node for header" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "Header(" << *var;
            if (dtype != TYPE_void) out << ", " << dtype;
            if (par_list != nullptr) out << ", " << *par_list;
            out << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analyze for node: Header, for func: " << var->get_id() << std::endl;
            if (par_list != nullptr) {
                par_list->sem_analyze();
                params = par_list->get_params();
            }

            std::string func_name = var->get_id();

            st.insert(func_name, dtype, params);
            /*The parameters will be declared as variables in the new scope in FuncDef*/
            std::clog << "Ended sem analyze for node: Header, for func: " << var->get_id() << std::endl;
        }

        std::vector<std::tuple<std::string, gen_type *, bool>> get_params() {return params;}

        DataType get_type() {return dtype;}
        std::string get_name() {return var->get_id();}

    private:
        Id *var;
        DataType dtype;
        HeaderAux *par_list;
        /*This field is filled when calling sem_analyze.
          It provides func args appropriately formatted for local decl*/
        std::vector<std::tuple<std::string, gen_type *, bool>> params;
};


class FuncDecl : public Def {
    public:
        FuncDecl(Header *h): header(h) {std::clog << "Creating node for funcdecl" << std::endl;}
        void printAST(std::ostream &out) const override {
            out << "FuncDecl("<< *header << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analyze for node: FuncDecl" << std::endl;
            header->sem_analyze();
            std::clog << "Ended sem analyze for node: FuncDecl" << std::endl;
        }
        
        virtual Value *igen() const override {
            std::clog << "Started igen for node: FuncDecl with func name: "<< header->get_name() << std::endl;

            std::string name = header->get_name();
            Type *type = get_llvm_type(header->get_type());

            int s = func_names.size();
            std::string prefix = s <= 1? "": func_names[s-2].back()->name + "_";
            name = prefix + name;

            /*Here, the llvm types of the params will be saved*/
            std::vector<Type *> arg_types;

            /*Get the parameters from the header: */
            std::vector<std::tuple<std::string, gen_type *, bool>> params = header->get_params();

            for (auto par: params) {
                arg_types.push_back(get_llvm_type(std::get<1>(par), std::get<2>(par)));
            }

            /*In order to avoid name colissions for functions, every definition of a function
              will have a name depending on the parent function that defines it:
              E.g. if f defines g then g will be saved as f.g */
            

            FunctionType *FT = FunctionType::get(type, arg_types, false);
            Function *F = Function::Create(FT, Function::InternalLinkage, name, TheModule.get());

            int Idx = 0;
            /*Record arg names in the func*/
            std::map<std::string, Type *> bt;
            for (auto &Arg : F->args()) {
                std::clog << "Defining name for arg: " << Idx + 1 << " " 
                      <<  std::get<0>(params[Idx]) << std::endl;

                Arg.setName(std::get<0>(params[Idx]));
                if(Arg.getType()->isPointerTy()) {
                    gen_type *t = std::get<1>(params[Idx]);
                    Type *baseType;
                    std::vector<int> s = t->ar->get_size();
                    s.erase(s.begin());
                    baseType = get_llvm_type(t->ar->get_type(), s);
                
                    bt[std::get<0>(params[Idx])] = baseType;
                
                }
                Idx++;
            }
            
            // SAFETY CHECK for func_names
            if (!func_names.empty()) {
                func_names.back().push_back(new func_details(name, bt));
            } else {
                std::clog << "ERROR: func_names is empty in FuncDecl::igen!" << std::endl;
            }

            std::clog << "Ended igen for node: FuncDecl with func name: "<< header->get_name() << std::endl;
            return nullptr;
        }


    private:
        Header *header;
};


class LocalDef : public AST {
    public:
        LocalDef() { def_list = {}; std::clog << "Creating node for localdef" << std::endl;}

        void append(Def * d) {def_list.push_back(d);}

        void printAST(std::ostream &out) const override {
            out << "LocalDef(";
            bool first = true;
            for (const auto &s : def_list) {  
                if (first) {out << *s; first = false;}
                else {out << ", " << *s;}
            }
            out << ")";
        }

        virtual void sem_analyze() override {
            /*A local def is a funcdef, funcdecl or a vardef*/
            for (auto &def: def_list) def->sem_analyze();
        }

        virtual Value *igen() const override {
            std::clog << "Started igen for node: LocalDef" << std::endl;
            
            
            for (auto &def: def_list) {
                def->parent_func = parent;
                def->igen();
            }
            
            
            std::clog << "Ended igen for node: LocalDef" << std::endl;
            return nullptr;
        }

        Function *parent;
    private:
        std::vector<Def *> def_list;
        
};

class FuncDef : public Def {
    public:
        FuncDef(Header *h, LocalDef *ld, Block *b): 
        header(h), local_def(ld), block(b)  {std::clog << "Creating node for funcdef" << std::endl;}

        void printAST(std::ostream &out) const override {
            out << "FuncDef(" << *header << ", " << *local_def;
            out << ", "<< *block << ")";
        }

        virtual void sem_analyze() override {
            std::clog << "Started sem analyze for node: FuncDef" << std::endl;
            header->sem_analyze();
            st.enterScope();
            std::vector<std::tuple<std::string, gen_type *, bool>> params = header->get_params();
            DataType func_type = header->get_type();
            for (const auto &p : params) {
                std::string n = std::get<0>(p);
                gen_type * t = std::get<1>(p);
                bool by_ref = std::get<2>(p);

                if (t->dtype == TYPE_array) st.insert(n, t->ar->get_type(), t->ar->get_size());
                else st.insert(n, t->dtype, by_ref);
            }
            if(local_def != nullptr) local_def->sem_analyze();
            block->set_type(func_type);
            block->sem_analyze();
            st.exitScope();

            std::clog << "Ended sem analyze for node: FuncDef" << std::endl;
        }

        virtual Value *igen() const override {
            /*Check if there already is a declaration*/
            std::clog << "Started igen for node: FuncDef with name: " << header->get_name() << std::endl;
            
            std::string name = header->get_name();

            int s = func_names.size();
            std::clog << "func_names has size: " << s << std::endl;
            
            // SAFETY CHECK: Only access func_names[s-2] if it exists and is non-empty
            std::string prefix = "";
            if (s > 1 && s >= 2 && !func_names[s-2].empty()) {
                prefix = func_names[s-2].back()->name + "_";
                std::clog << "Setting prefix from parent function: " << prefix << std::endl;
            } else if (s <= 1) {
                std::clog << "Top-level function, no prefix needed" << std::endl;
            } else {
                std::clog << "WARNING: s=" << s << " but func_names[s-2] might be empty or invalid!" << std::endl;
            }
            name = prefix + name;
 
            std::clog << "function name in the llvm assembly will be: " << name << std::endl;
            std::clog << "TheModule in FuncDef::igen = " << TheModule.get() << std::endl;
            
            // Clear any previous captured variable tracking for this function
            captured_variables[name].clear();
            
            // Track implicit parameters for nested functions
            std::vector<std::pair<std::string, Type*>> implicit_params;
            
            Function *F = TheModule->getFunction(StringRef(name)); 

            std::clog << "getFunction returned succesfully" << std::endl;
            
            if (F == nullptr) {
                std::clog << "Didn't find function: " << name << ", creating it"<< std::endl;
                Type *type = get_llvm_type(header->get_type());

                /*Here, the llvm types of the params will be saved*/
                std::vector<Type *> arg_types = {};
                std::vector<std::string> arg_names = {};

                /*Get the parameters from the header: */
                std::vector<std::tuple<std::string, gen_type *, bool>> params = header->get_params();

                for (auto par: params) {
                    arg_types.push_back(get_llvm_type(std::get<1>(par), std::get<2>(par)));
                    arg_names.push_back(std::get<0>(par));
                }
                
                // For nested functions (s > 1), add implicit parameters for parent scope variables
                // This allows nested functions to access parent variables
                if (s > 1 && NamedValues.size() > 0 && !NamedValues.back().empty()) {
                    std::clog << "This is a nested function, adding parent scope variables as implicit parameters" << std::endl;
                    
                    // Get the parent function's scope
                    const auto& parent_scope = NamedValues.back();
                    
                    // Build a set of this function's parameter names to check for shadowing
                    std::set<std::string> local_param_names;
                    for (const auto& par : params) {
                        local_param_names.insert(std::get<0>(par));
                    }
                    
                    // Extract the parent function name to demangle variable names
                    if (s >= 2 && func_names.size() >= 2 && !func_names[s-2].empty()) {
                        std::string parent_func_name = func_names[s-2].back()->name;
                        std::string parent_prefix = parent_func_name + "_";
                        
                        std::clog << "Parent function: " << parent_func_name << std::endl;
                        std::clog << "Parent scope has " << parent_scope.size() << " variables" << std::endl;
                        
                        for (const auto& var_entry : parent_scope) {
                            std::string mangled_name = var_entry.first;
                            AllocaInst* alloca = var_entry.second;
                            
                            // Extract the actual variable name
                            if (mangled_name.find(parent_prefix) == 0) {
                                std::string var_name = mangled_name.substr(parent_prefix.length());
                                
                                // Skip variables that are incoming capture parameters (start with __parent_)
                                // These are just the mechanism for passing captured variables, not real variables to capture
                                if (var_name.find("__parent_") == 0) {
                                    std::clog << "  Skipping " << var_name << " (incoming capture parameter)" << std::endl;
                                    continue;
                                }
                                
                                // Skip if this variable is shadowed by a local parameter
                                if (local_param_names.count(var_name) > 0) {
                                    std::clog << "  Skipping " << var_name << " (shadowed by local parameter)" << std::endl;
                                    continue;
                                }
                                
                                Type* var_type = alloca->getAllocatedType();
                                
                                // Check if this is already a captured variable (pointer type)
                                // If so, we don't need to add another layer of indirection
                                bool is_already_captured = var_type->isPointerTy();
                                
                                if (is_already_captured) {
                                    std::clog << "  Adding implicit parameter (already captured): " << var_name << std::endl;
                                    // Pass the pointer type as-is
                                    arg_types.push_back(var_type);
                                    arg_names.push_back("__parent_" + var_name);
                                    implicit_params.push_back({var_name, var_type});
                                } else {
                                    std::clog << "  Adding implicit parameter: " << var_name << std::endl;
                                    // Add as pointer parameter
                                    arg_types.push_back(PointerType::get(var_type, 0));
                                    arg_names.push_back("__parent_" + var_name);
                                    implicit_params.push_back({var_name, var_type});
                                }
                            }
                        }
                    }
                }

                FunctionType *FT = FunctionType::get(type, arg_types, false);
                F = Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());

                int Idx = 0;
                /*Record arg names in the func*/
                std::map<std::string, Type *> bt;

                for (auto &Arg : F->args()) {
                    if (Idx < (int)arg_names.size()) {
                        std::clog << "Defining name for arg: " << Idx + 1 << " " 
                                  <<  arg_names[Idx] << std::endl;
                        Arg.setName(arg_names[Idx]);
                        
                        if (Idx < (int)params.size() && Arg.getType()->isPointerTy()) {
                            std::clog << "Arg " << arg_names[Idx] << " is a pointer, adding to bt" << std::endl;
                            gen_type *t = std::get<1>(params[Idx]);
                            Type *baseType;
                            if(t->dtype == TYPE_array) {
                                std::vector<int> s = t->ar->get_size();
                                s.erase(s.begin());
                                baseType = get_llvm_type(t->ar->get_type(), s);
                            }
                            else baseType = get_llvm_type(t->dtype);
                            bt[arg_names[Idx]] = baseType;
                            std::clog << "Added " << arg_names[Idx] << " to bt" << std::endl;
                        }
                    }
                    Idx++;
                }
                
                // Store implicit parameters info in func_details
                func_details* fd = new func_details(name, bt);
                for (const auto& imp : implicit_params) {
                    fd->captured_vars.push_back(imp.first);
                    Type* captured_type = imp.second;
                    
                    // If the captured variable is a pointer to an array element (like ptr from byte[]),
                    // we need to determine its base element type
                    // Check if this is from an array parameter in the parent scope
                    func_details* parent_det = get_parent();
                    if (parent_det && parent_det->argBaseTypes.count(imp.first) > 0) {
                        // Use the parent's base type (already computed correctly)
                        fd->captured_types[imp.first] = parent_det->argBaseTypes[imp.first];
                        std::clog << "  Captured " << imp.first << " base type from parent argBaseTypes" << std::endl;
                    } else if (parent_det && parent_det->captured_types.count(imp.first) > 0) {
                        // This is a multi-level capture - use the parent's captured type
                        fd->captured_types[imp.first] = parent_det->captured_types[imp.first];
                        std::clog << "  Captured " << imp.first << " type from parent captured_types" << std::endl;
                    } else {
                        // This is a local variable from parent scope
                        // If it's an array type, we need to get the element type
                        if (captured_type->isArrayTy()) {
                            fd->captured_types[imp.first] = captured_type->getArrayElementType();
                            std::clog << "  Captured " << imp.first << " element type from array" << std::endl;
                        } else {
                            // For non-array types, use as-is
                            fd->captured_types[imp.first] = captured_type;
                            std::clog << "  Captured " << imp.first << " using direct type" << std::endl;
                        }
                    }
                }
                std::clog << "About to store func_details, func_names.size() = " << func_names.size() << std::endl;
                if (!func_names.empty()) {
                    std::clog << "func_names.back().size() = " << func_names.back().size() << std::endl;
                    func_names.back().push_back(fd);
                    std::clog << "Stored func_details successfully" << std::endl;
                } else {
                    std::clog << "WARNING: func_names is empty, cannot store func_details" << std::endl;
                }
                
                std::clog << "Function created with " << implicit_params.size() 
                          << " implicit parameters" << std::endl;
            }
            
            // Also populate the captured_variables map for use at call sites
            if (!implicit_params.empty()) {
                captured_variables[name].clear();
                for (const auto& imp : implicit_params) {
                    captured_variables[name].push_back({imp.first, imp.second});
                }
                std::clog << "Stored " << implicit_params.size() 
                          << " captured variables for function " << name << std::endl;
            }

            std::map<std::string, AllocaInst*> args;

            std::clog << "Creating function BB" << std::endl;

            BasicBlock *BB = BasicBlock::Create(TheContext, "entry", F);
            Builder.SetInsertPoint(BB);


            std::clog << "Function BB created" << std::endl;

            /*Create entries in var scope for parameters with mangled names*/
            for (auto &Arg : F->args()) {
                    // The function's name, F->getName(), is already mangled. We use it as a prefix.
                    std::string mangled_arg_name = std::string(F->getName()) + "_" + std::string(Arg.getName());

                    // Create an alloca for this variable with the mangled name.
                    std::clog << "Creating Alloca entry for arg: " << std::string(Arg.getName()) 
                            << " with mangled name: " << mangled_arg_name << std::endl;

                std::clog << "Passing arg in current scope with name: " << std::string(Arg.getName());
                AllocaInst *Alloca = CreateEntryBlockAlloca(F, mangled_arg_name, Arg.getType());                    std::clog << "CreateEntryBlock finished succesfully" << std::endl;

                    // Store the initial value into the alloca.
                    Builder.CreateStore(&Arg, Alloca);

                    // Add arguments to variable symbol table using the mangled name.
                    args[mangled_arg_name] = Alloca;
            }

            std::clog << "Created Allocas for args" << std::endl;

            // For nested functions with implicit parent variable parameters,
            // create "reference" variables that point to the parent's variables
            if (!implicit_params.empty()) {
                std::clog << "Creating reference variables for " << implicit_params.size() 
                          << " parent scope variables" << std::endl;
                
                // The implicit parameters are at the end of the argument list
                int explicit_param_count = header->get_params().size();
                int arg_idx = explicit_param_count;
                
                for (const auto& imp : implicit_params) {
                    std::string var_name = imp.first;
                    Type* var_type = imp.second;
                    
                    // Create a "reference" variable with the nested function's mangled name
                    std::string ref_var_name = name + "_" + var_name;
                    
                    std::clog << "  Creating reference variable: " << ref_var_name 
                              << " pointing to parent's " << var_name << std::endl;
                    
                    // The parameter is a pointer to the parent's variable
                    // We store this pointer in our local scope so lookups find it
                    Argument* parent_var_arg = F->getArg(arg_idx);
                    AllocaInst* ref_alloca = CreateEntryBlockAlloca(F, ref_var_name, 
                                                                     PointerType::get(var_type, 0));
                    Builder.CreateStore(parent_var_arg, ref_alloca);
                    
                    // Add to the variable symbol table
                    // This allows the nested function to access the parent's variable
                    args[ref_var_name] = ref_alloca;
                    
                    arg_idx++;
                }
            }

            NamedValues.push_back(args);
            for (auto i = args.begin(); i != args.end(); i++) {
                std::clog << i->first << std::endl;
               }

            std::clog << "Entered args in new scope"<< std::endl;
            std::vector<func_details *> f;
            func_names.push_back(f);
            std::clog << "Entered new scope for funcs" <<std::endl;
            if (local_def != nullptr) {
                /* We do that because if a func def is before the var def, if we try to get the parent
                func from the module, we will get the local func so the definition will happen there.*/
                local_def->parent = F;
                local_def->igen();
            }


            
            /*Now generate code for the body*/
            std::clog << "Generating code for the body" << std::endl;
            /* We have to do that because if not the block will generate inside 
            a local def function body. */
            Builder.SetInsertPoint(BB);
            block->igen();
            std::clog << "Code generated for the body" << std::endl;
            
            // After generating the body, check if this nested function captured any variables
            if (s > 1 && !captured_variables[name].empty()) {
                std::clog << "Function " << name << " captured " << captured_variables[name].size() 
                          << " variables, needs to be regenerated with additional parameters" << std::endl;
                
                // Store captured variable info in func_details
                // SAFETY CHECK: ensure func_names.back() exists and is non-empty
                if (!func_names.empty() && !func_names.back().empty()) {
                    func_names.back().back()->captured_vars.clear();
                    func_names.back().back()->captured_types.clear();
                    
                    for (const auto& cap : captured_variables[name]) {
                        std::clog << "  - Captured: " << cap.first << std::endl;
                        func_names.back().back()->captured_vars.push_back(cap.first);
                        func_names.back().back()->captured_types[cap.first] = cap.second;
                    }
                    
                    // IMPORTANT: We need to regenerate this function with captured variables as parameters
                    // For now, we'll print a warning. The actual regeneration is complex and needs
                    // to be done before the body is generated, not after.
                    std::clog << "WARNING: Function uses closures but they are not fully implemented yet" << std::endl;
                } else {
                    std::clog << "ERROR: func_names is empty or func_names.back() is empty!" << std::endl;
                }
            }
            
            NamedValues.pop_back();
            func_names.pop_back();
            
            // If this is a nested function, remove its entry from the parent scope's func_names
            // to prevent it from being used as the parent for subsequent variable definitions
            if (!func_names.empty() && !func_names.back().empty()) {
                func_names.back().pop_back();
                std::clog << "Removed nested function from parent's func_names" << std::endl;
            }

            if (!Builder.GetInsertBlock()->getTerminator()) {
                Function *TheFunction = Builder.GetInsertBlock()->getParent();
                Type *retTy = TheFunction->getReturnType();

                if (retTy->isVoidTy()) {
                    Builder.CreateRetVoid();
                } else {
                    Value *zero = Constant::getNullValue(retTy);
                    Builder.CreateRet(zero);
                }
            }

            std::clog << "Ended igen for node: FuncDef with name: " << header->get_name() << std::endl;
            return nullptr;
        }

        virtual Function *igen_main() const {
            std::clog << "Started igen_main for node: FuncDef with name: " << header->get_name() << std::endl;
            
            // For the main function, we always use "main" as the name regardless of the actual function name
            std::string name = "main";
            
            std::clog << "function name in the llvm assembly will be: " << name << std::endl;
            
            Function *F; 
                        
            // Main function should return int (i32) to match C standard
            Type *type = i32;

            /*Here, the llvm types of the params will be saved*/
            std::vector<Type *> arg_types = {};

            /*Get the parameters from the header: */
            std::vector<std::tuple<std::string, gen_type *, bool>> params = header->get_params();

            for (auto par: params) {
                arg_types.push_back(get_llvm_type(std::get<1>(par), std::get<2>(par)));
            }

            FunctionType *FT = FunctionType::get(type, arg_types, false);
            F = Function::Create(FT, Function::ExternalLinkage, name, TheModule.get());

            int Idx = 0;
            /*Record arg names in the func*/
            std::map<std::string, Type *> bt;

            for (auto &Arg : F->args()) {
                std::clog << "Defining name for arg: " << Idx + 1 << " " 
                        <<  std::get<0>(params[Idx]) << std::endl;
                Arg.setName(std::get<0>(params[Idx]));
                if(Arg.getType()->isPointerTy()) {
                    std::clog << "Param above is a pointer" << std::endl;
                    gen_type *t = std::get<1>(params[Idx]);
                    Type *baseType;

                    std::vector<int> s = t->ar->get_size();
                    s.erase(s.begin());
                    baseType = get_llvm_type(t->ar->get_type(), s);
                    
                    bt[std::get<0>(params[Idx])] = baseType;
                    
                }
                Idx++;
            }

            // Use the original function name for func_details, not "main"
            // SAFETY CHECK for func_names
            if (!func_names.empty()) {
                func_names.back().push_back(new func_details(header->get_name(), bt));
                std::clog << "Inserted func_details on the back" << std::endl;
            } else {
                std::clog << "ERROR: func_names is empty when creating main function!" << std::endl;
            }

            std::map<std::string, AllocaInst*> args;

            std::clog << "Creating function BB" << std::endl;

            BasicBlock *BB = BasicBlock::Create(TheContext, "entry", F);
            Builder.SetInsertPoint(BB);

            std::clog << "Function BB created" << std::endl;

            for (auto &Arg : F->args()) {
                    // The main function's name is just "main".
                    std::string mangled_arg_name = std::string(F->getName()) + "_" + std::string(Arg.getName());

                    // Create an alloca for this variable.
                    std::clog << "Creating Alloca entry for arg: " << std::string(Arg.getName()) 
                            << " with mangled name: " << mangled_arg_name << std::endl;

                    AllocaInst *Alloca = CreateEntryBlockAlloca(F, mangled_arg_name, Arg.getType());

                    std::clog << "CreateEntryBlock finished succesfully" << std::endl;

                    // Store the initial value into the alloca.
                    Builder.CreateStore(&Arg, Alloca);

                    // Add arguments to variable symbol table.
                    args[mangled_arg_name] = Alloca;
            }
            
            std::clog << "Created Allocas for args" << std::endl;

            NamedValues.push_back(args);
            for (auto i = args.begin(); i != args.end(); i++) {
                std::clog << i->first << std::endl;
            }
            std::clog << "Entered args in new scope"<< std::endl;

            std::vector<func_details *> f;
            func_names.push_back(f);
            std::clog << "Entered new scope for funcs" <<std::endl;

            
            if (local_def != nullptr) {
                /* We do that because if a func def is before the var def, if we try to get the parent
                func from the module, we will get the local func so the definition will happen there.*/
                local_def->parent = F;
                local_def->igen();
            }

            /*Now generate code for the body*/
            std::clog << "Generating code for the body" << std::endl;
            
            /* We have to do that because if not the block will generate inside 
            a local def function body. */
            Builder.SetInsertPoint(BB);
            block->igen();
            std::clog << "Code generated for the body" << std::endl;
            NamedValues.pop_back();
            func_names.pop_back();
            
            if (!Builder.GetInsertBlock()->getTerminator()) {
                Function *TheFunction = Builder.GetInsertBlock()->getParent();
                Type *retTy = TheFunction->getReturnType();

                if (retTy->isVoidTy()) {
                    Builder.CreateRetVoid();
                } else {
                    Value *zero = Constant::getNullValue(retTy);
                    Builder.CreateRet(zero);
                }
            }
            std::clog << "Ended igen_main for node: FuncDef with name: " << header->get_name() << " (generated as main)" << std::endl;
            return F;
        }

    private:
        Header *header;
        LocalDef *local_def;
        Block *block;
};


#endif