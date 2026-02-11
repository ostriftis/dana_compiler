#ifndef __AST_HPP__
#define __AST_HPP__

#include <iostream>
#include <vector>
#include <string>
#include <tuple>

#include "symbol.hpp"
#include "lexer.hpp"

const std::string name[] = {"UNDEFINED", "TYPE_int", "TYPE_byte", "TYPE_bool", "TYPE_array", "TYPE_void", "TYPE_func"};

class AST {
    public:
        virtual void printAST(std::ostream &out) const = 0;
        virtual void sem_analyze() {}
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
            if (type->dtype != expected_type) 
                yyerror("Type mismatch: expected " + name[expected_type] + ", instead got: " + name[type->dtype]);
        }
        void check_type(gen_type *expected_type) {
            sem_analyze();
            if (type->dtype != expected_type->dtype) 
                yyerror("Type mismatch: expected " + name[expected_type->dtype] + ", instead got: " + name[type->dtype]);
            
            if (type->dtype == TYPE_array) {
                // Here we check the base type of the array
                DataType expected = expected_type->ar->get_type();
                if (type->ar->get_type() != expected) {
                    std::string msg = "Type mismatch: expected array of type: " + name[expected];
                    msg = msg + ", instead got: " + name[type->ar->get_type()];
                    yyerror(msg);
                }
                if (expected_type->ar->size.front() != -1) {
                    // We exclude the case where the first dim is ommited.
                    if(type->ar->get_size() != expected_type->ar->get_size()) yyerror("Array dimensions are wrong");
                }
            }

        }

        void check_types(std::vector<gen_type *> types) {
            sem_analyze();
            bool type_match = false;
            for (gen_type * t: types) {
                if (type->dtype == t->dtype) {
                    if(type->dtype == TYPE_array) {
                        if (type->ar->get_type() == t->ar->get_type() && type->ar->get_size() == t->ar->get_size()){
                            type_match = true;
                            break;
                        }
                    }
                    else {
                        type_match = true;
                        break;
                    }
                }
            }
            if(!type_match) yyerror("Type mismatch");
        }

        gen_type * getType() {return type;}
    protected:
        gen_type *type;
};


class SimpleStmt : public AST {
    public:
        void set_type(DataType t) {calle_type = t;}
    protected:
        DataType calle_type;
};

class Id : public Expr {
    public:
        Id(std::string i): id(i) {}

        void printAST(std::ostream &out) const override {
          out << "Id(" << id << ")";
        }

        virtual void sem_analyze() override {
            STEntry *e = st.lookup(id);
            type = new gen_type(*e->type);
        }

        std::string get_id() {return id;}
    private:
        std::string id;
};

class Def : public AST {
    /*This class exists only to work as a common type for:
      func_def, func_decl and var_def so as to define LocalDef*/
};

class StmtList : public AST {
    public:
        StmtList(): stmt_list() {}

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
            for (SimpleStmt *stmt : stmt_list) {
                stmt->set_type(calle_type);
                stmt->sem_analyze();
            }
        }
        void set_type(DataType t) {calle_type = t;}
    private:
        std::vector<SimpleStmt *> stmt_list;
        DataType calle_type;
};

class Stmt : public AST {
    public:
        Stmt(StmtList *l): stmt_list(l) {}

        void printAST(std::ostream &out) const override {
            out << "Stmt("<< *stmt_list << ")";
        }
        
        virtual void sem_analyze() override {
            stmt_list->set_type(calle_type);
            stmt_list->sem_analyze();
        }
        void set_type(DataType t) {calle_type = t;}

    private:
        StmtList *stmt_list;
        DataType calle_type;

};


class Block : public AST {
    public:
        Block(Stmt *s): stmt(s) {}

        void printAST(std::ostream &out) const override {
            out << "Block(" << *stmt << ")";
        }
        virtual void sem_analyze() override {
            stmt->set_type(calle_type);
            stmt->sem_analyze();
        }

        void set_type(DataType t) {calle_type = t;}
    private:
        Stmt *stmt;
        //It is needed for type checking on stmts exit and return
        DataType calle_type;
};


/*Subclasses of Expr*/

class Int : public Expr {
    public:
        Int(int n): num(n) {}

        void printAST(std::ostream &out) const override {
            out << "Int(" << num << ")";
        }
        virtual void sem_analyze() override {
            type = new gen_type(TYPE_int);
        }

        int value() {return num;}

    private:
        int num;
};

class Char : public Expr {
    public:
        Char(char c): character(c) {}

        void printAST(std::ostream &out) const override {
            out << "Char(" << character << ")";
        }

        virtual void sem_analyze() override {
            type = new gen_type(TYPE_byte);
        }
        int value() {return character;}
        
    private:
        char character;
};

class Bool : public Expr {
    public:
        Bool(bool b): boolconst(b) {}

        void printAST(std::ostream &out) const override {
            out << "Bool(" << boolconst << ")";
        }
        virtual void sem_analyze() override {
            type = new gen_type(TYPE_byte);
        }

    private:
        bool boolconst;
};


class BinOp : public Expr {
    public:
        BinOp(Expr *e1, std::string o, Expr *e2): expr1(e1), op(o), expr2(e2) {}

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr1 << ", " << *expr2 << ")";
        }

        virtual void sem_analyze() override {
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
        }

    private:
        Expr *expr1;
        std::string op;
        Expr *expr2;
};

class UnOp : public Expr {
    public:
        UnOp(std::string o, Expr *e): op(o), expr(e) {}

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr << ")";
        }

        virtual void sem_analyze() override {
            if (op == "+" || op == "-") {
                expr->check_type(TYPE_int);
                type = new gen_type(TYPE_int);
            }
            else if (op == "!") {
                expr->check_type(TYPE_byte);
                type = new gen_type(TYPE_byte);
            }
        }

    private:
        Expr *expr;
        std::string op; 
};

class L_val : public Expr {
    public:
        L_val(Id * i, std::string v = "",  std::vector<Expr *> e = {}) : id(i), str(v), expr_list(e) {
            if (i == nullptr) is_string = true;
            else is_string = false;
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
            if(!is_string) {
                id->sem_analyze();
                type = new gen_type(*id->getType());
                // dont need to check type between id and stentry because sen_analyze gets it directly from st

                if (!expr_list.empty()) {
                    /*That means that the identifier is supposed to be an array,
                      so let's check*/
                    if (type->dtype != TYPE_array) yyerror("Variable is not an array");
                    else {
                        for (Expr *s : expr_list) {
                            // Check that every expr inside [] is of type int 
                            s->check_type(TYPE_int);
                        }
                    }
                    /*that means that the l-value is an array element so it will have the base type*/
                    type = new gen_type(type->ar->get_type());
                }
            }
            else {
                //this will create a bool type array with one dimension, that of the string length
                std::vector<int> dim = {(int) str.length()};
                type = new gen_type(TYPE_byte, dim);
            }
        }

    private:
        Id *id;
        std::string str;
        bool is_string; 
        std::vector<Expr *> expr_list;
};

class ExprAux : public Expr {
    public:
        ExprAux(): expr_list() {}
        
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
            for (Expr *e: expr_list) e->sem_analyze();
        }
        

        std::vector<Expr *> expr_list;
};

class FuncCall : public Expr {
    public:
        FuncCall(Id *v, ExprAux * e = nullptr): var(v), expr_list(e) {}

        void printAST(std::ostream &out) const override {
            out << "FuncCall(" << *var;
            if(expr_list != nullptr) out << *expr_list;
            out << ")";
        }
        
        virtual void sem_analyze() override {

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
                }
            }
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
        ExprCond(Expr *e): expr(e) {}
        void printAST(std::ostream &out) const override {
            out << "ExprCond(" << *expr <<  ")";
        }

        virtual void sem_analyze() override {
            std::vector<gen_type *> type_list = {new gen_type(TYPE_int), new gen_type(TYPE_byte)};
            expr->check_types(type_list);
            type = new gen_type(TYPE_bool);
        }

    private:
        Expr *expr;
};


class BinOpCond : public Cond {
    public:
        BinOpCond(Expr *e1, char* o, Expr *e2): expr1(e1), op(o), expr2(e2) {}

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr1 << ", " << *expr2 << ")";
        }

        virtual void sem_analyze() override {
            if (op == "=" || op == "<" || op == ">" || op == "<>" || op == "<=" || op == ">=") {
                expr1->check_type(TYPE_int);
                expr2->check_type(TYPE_int);
                type = new gen_type(TYPE_bool);
            }
            else if (op == "and" || op == "or") {
                expr1->check_type(TYPE_bool);
                expr2->check_type(TYPE_bool);
                type = new gen_type(TYPE_bool);
            }
        }

    private:
        Expr *expr1;
        std::string op;
        Expr *expr2;
};

class UnOpCond : public Cond {
    public:
        UnOpCond(std::string o, Expr *e): op(o), expr(e) {}

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr << ")";
        }

        virtual void sem_analyze() override {
            expr->check_type(TYPE_bool);
            type = new gen_type(TYPE_bool);
        }
        

    private:
        Expr *expr;
        std::string op; 
};

/*Subclasses of Stmt*/

class Skip : public SimpleStmt {
    public:
        void printAST(std::ostream &out) const override {
            out << "Skip()";
        }

    private:
};

class Exit : public SimpleStmt {
    public:
        void printAST(std::ostream &out) const override {
            out << "Exit()";
        }
        virtual void sem_analyze() override {
            if (calle_type != TYPE_void) yyerror("Can't exit from a function, expected a return statement.");
        }
    private:
};

class Return : public SimpleStmt {
    public:
        Return(Expr *e): expr(e) {}

        void printAST(std::ostream &out) const override {
            out << "Return(" << *expr << ")";
        }
        
        virtual void sem_analyze() override {
            expr->check_type(calle_type);
        }

    private:
        Expr *expr;
};

class Loop : public SimpleStmt {
    public:
        Loop(Block *b, Id *v = nullptr): name(v), block(b) {}

        void printAST(std::ostream &out) const override {
            out << "Loop(" ;
            if (name != nullptr) out << *name << ", ";
            out << *block << ")";
        }
        virtual void sem_analyze() override {
            lp.insert((name != nullptr)? name->get_id(): "");
            block->set_type(calle_type);
            block->sem_analyze();
            lp.pop((name != nullptr)? name->get_id(): "");
            
        }

    private:
        Id *name;
        Block *block;
};

class Break : public SimpleStmt {
    public:
        Break(Id *n = nullptr): name(n) {}
        void printAST(std::ostream &out) const override {
            out << "Break(" << *name << ")";
        }
        virtual void sem_analyze() {
            if (name != nullptr) name->check_type(TYPE_loop);
            lp.break_loop((name != nullptr)? name->get_id(): "");
        }
    private:
        Id *name;
};

class Continue : public SimpleStmt {
    public:
        Continue(Id *n = nullptr): name(n) {}
        void printAST(std::ostream &out) const override {
            out << "Continue(" << *name << ")";
        }
        virtual void sem_analyze() {
            if (name != nullptr) name->check_type(TYPE_loop);
            lp.cont_loop((name != nullptr)? name->get_id(): "");
        }


    private:
        Id *name;
};


class ProcCall : public SimpleStmt {
    public:
        ProcCall(Id *v, ExprAux * e = nullptr): var(v), expr_list(e) {}

        void printAST(std::ostream &out) const override {
            out << "ProcCall(" << *var;
            if(expr_list != nullptr) out << ", " << *expr_list;
            out << ")";
        }
        virtual void sem_analyze() override {
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
                }
            }
        }
    private:
        Id *var;
        ExprAux * expr_list;

};

class Assign : public SimpleStmt {
    public:
        Assign(L_val *l, Expr *e) : l_val(l), expr(e) {}

        void printAST(std::ostream &out) const override {
            out << "Assign(" << *l_val << ", " << *expr << ")"; 
        }

        virtual void sem_analyze() override {
            l_val->sem_analyze();
            expr->check_type(l_val->getType());
        }

    private:
        L_val *l_val;
        Expr *expr; 
};

class Elif : public AST {
    public:
        Elif(Cond * c, Block *b) : cond(c), block(b) {}

        void printAST(std::ostream &out) const override {
            out << "Elif(" << *cond << ", " << *block << ")";
        }

        virtual void sem_analyze() override {
            cond->check_type(TYPE_bool);
            block->set_type(callee_type);
            block->sem_analyze();
        }

        void set_type(DataType t) {callee_type = t;}

    private:
        Cond *cond;
        Block *block;
        DataType callee_type;
};

class IfAux : public AST {
    public:
        IfAux() : elif_list() {}
        
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
            for (const auto &elif: elif_list) {
                elif->set_type(calle_type);
                elif->sem_analyze();
            }
        }
        void set_type(DataType t) {calle_type = t;}
        
    private:
        std::vector<Elif *> elif_list;
        DataType calle_type;
};

class If : public SimpleStmt {
    public:
        /*We assume that the statement of else is given as a 2nd param,
          and if there are any elifs they are the last parameters.*/
        If(Cond *c1, Block *b1, Block *b2 = nullptr, IfAux *el = nullptr):
         cond(c1), block1(b1), block2(b2), elif_list(el) {}

        void printAST(std::ostream &out) const override {
            out << "If(" << *cond << ", " << *block1 ;
            if(elif_list != nullptr) out << ", " << *elif_list;
            if(block2 != nullptr) out << ", " << *block2;
            out << ")";
        }

        virtual void sem_analyze() override {
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
        }

    private:
        Cond *cond;
        Block *block1;
        Block *block2;
        IfAux *elif_list;
};






/* We implement the remaining grammar rules*/


class Type : public AST {
    public:
        Type(DataType dt, std::vector<Int *> d = {}): dtype(dt), dim(d) {}

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
        IdAux(Id *v) {var = {}; var.push_back(v);}

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
        VarDef(IdAux *v, Type *t): var_list(v), type(t) {}

        void printAST(std::ostream &out) const override {
            out << "VarDef(" << *var_list << ", " << *type << ")";
        }

        virtual void sem_analyze() override {
            int size = var_list->get_size();
            for (int i = 0; i < size; i++) {
                st.insert(var_list->get(i)->get_id(), type->get_type(), type->get_dims());
            }
        } 

    private:
        IdAux *var_list;
        Type *type;
};

class ArrayType : public AST {
    public:
        ArrayType(DataType dt, std::vector<Int *> d = {new Int(-1)}): dtype(dt), dim(d) {}

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
        FparType(Type *t = nullptr, DataType dt = UNDEFINED, ArrayType *at = nullptr):
          tp(t), ref_dt(dt) ,atype(at) {}

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
            if (tp != nullptr) {general_type = new gen_type(tp->get_type(), tp->get_dims());}
            else if (ref_dt != UNDEFINED) {general_type = new gen_type(ref_dt);}
            else if (atype != nullptr) {general_type = new gen_type(atype->get_type(), atype->get_dims());}
        }
        
        gen_type *get_type() {return general_type;}
    private:
        /*type is from symbol.hpp and Type is an AST Node*/
        gen_type *general_type;
        Type *tp;
        DataType ref_dt;
        ArrayType *atype;
};


class FparDef : public AST {
    public:
        FparDef(IdAux *vl, FparType * fp): var_list(vl), fpar_type(fp) {}

        void printAST(std::ostream &out) const override {
            out << "FparDef("<< *var_list << ", " << *fpar_type << ")";
        }
    
        virtual void sem_analyze() override {
            fpar_type->sem_analyze(); // now general_type is initialized
        }
        gen_type *get_type() {return fpar_type->get_type();}

    
        IdAux *var_list;
        FparType * fpar_type;
};



class HeaderAux : public AST {
    public:
        HeaderAux(): par_list() {}
        
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

        std::vector<std::tuple<std::string, gen_type *>> get_params() {
            /*This function creates a list of each par definition with a certain type.
            It will be used by Header to create the function st entry*/
            std::vector<std::tuple<std::string, gen_type *>> res = {};
            for (FparDef * d: par_list) {
                int length = d->var_list->get_size();
                for (int i = 0; i < length; i++) {res.push_back(std::make_tuple(d->var_list->get(i)->get_id(), d->fpar_type->get_type()));}
            }
            return res;
        }

    private:
        std::vector<FparDef *> par_list;
};



class Header : public AST {
    public:
        Header(Id *v, DataType dt, HeaderAux *h): 
        var(v), dtype(dt), par_list(h) {params = {};}

        void printAST(std::ostream &out) const override {
            out << "Header(" << *var;
            if (dtype != TYPE_void) out << ", " << dtype;
            if (par_list != nullptr) out << ", " << *par_list;
            out << ")";
        }

        virtual void sem_analyze() override {
            if (par_list != nullptr) {
                par_list->sem_analyze();
                params = par_list->get_params();
            }

            std::string func_name = var->get_id();

            st.insert(func_name, dtype, params);
            /*The parameters will be declared as variables in the new scope in FuncDef*/
        }

        std::vector<std::tuple<std::string, gen_type *>> get_params() {return params;}

        DataType get_type() {return dtype;}
    private:
        Id *var;
        DataType dtype;
        HeaderAux *par_list;
        /*This field is filled when calling sem_analyze.
          It provides func args appropriately formatted for local decl*/
        std::vector<std::tuple<std::string, gen_type *>> params;
};


class FuncDecl : public Def {
    public:
        FuncDecl(Header *h): header(h) {}
        void printAST(std::ostream &out) const override {
            out << "FuncDecl("<< *header << ")";
        }

        virtual void sem_analyze() override {
            header->sem_analyze();
        }

    private:
        Header *header;
};


class LocalDef : public AST {
    public:
        LocalDef() { def_list = {}; }

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
    
    private:
        std::vector<Def *> def_list;
};

class FuncDef : public Def {
    public:
        FuncDef(Header *h, LocalDef *ld, Block *b): 
        header(h), local_def(ld), block(b)  {}

        void printAST(std::ostream &out) const override {
            out << "FuncDef(" << *header << ", " << *local_def;
            out << ", "<< *block << ")";
        }

        virtual void sem_analyze() override {

            header->sem_analyze();
            st.enterScope();
            std::vector<std::tuple<std::string, gen_type *>> params = header->get_params();
            DataType func_type = header->get_type();
            for (const auto &p : params) {
                std::string n = std::get<0>(p);
                gen_type * t = std::get<1>(p);
                if (t->dtype == TYPE_array) st.insert(n, t->ar->get_type(), t->ar->get_size());
                else st.insert(n, t->dtype);
            }
            if(local_def != nullptr) local_def->sem_analyze();
            block->set_type(func_type);
            block->sem_analyze();
            st.exitScope();
        }

    private:
        Header *header;
        LocalDef *local_def;
        Block *block;
};


#endif