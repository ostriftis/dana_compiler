#include <iostream>
#include <vector>
#include <string>


class AST {
    public:
        virtual void printAST(std::ostream &out) const = 0;
};

inline std::ostream &operator<<(std::ostream &out, const AST &ast) {
  ast.printAST(out);
  return out;
}

/* Here we define Expressions*/

class Expr : public AST {
    public:
    private:
};


class SimpleStmt : public AST {
    public:
    private:
};



class Def : public AST {
    /*This class exists only to work as a common type for:
      func_def, func_decl and var_def so as to define LocalDef*/
    public:
    private:
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
    private:
        std::vector<SimpleStmt *> stmt_list;
};

class Stmt : public AST {
    public:
        Stmt(StmtList *l): stmt_list(l) {}

        void printAST(std::ostream &out) const override {
            out << "Stmt("<< *stmt_list << ")";
        }
    private:
        StmtList *stmt_list;
};
class Block : public AST {
    public:
        Block(Stmt *s): stmt(s) {}

        void printAST(std::ostream &out) const override {
            out << "Block(" << *stmt << ")";
        }
    private:
        Stmt *stmt;
};


/*Subclasses of Expr*/

class Int : public Expr {
    public:
        Int(int n): num(n) {}

        void printAST(std::ostream &out) const override {
            out << "Int(" << num << ")";
        }

    private:
        int num;
};

class Char : public Expr {
    public:
        Char(char c): character(c) {}

        void printAST(std::ostream &out) const override {
            out << "Char(" << character << ")";
        }

    private:
        char character;
};

class Bool : public Expr {
    public:
        Bool(std::string b): boolconst(b) {}

        void printAST(std::ostream &out) const override {
            out << "Bool(" << boolconst << ")";
        }

    private:
        std::string boolconst;
};


class BinOp : public Expr {
    public:
        BinOp(Expr *e1, char* o, Expr *e2): expr1(e1), op(o), expr2(e2) {}

        void printAST(std::ostream &out) const override {
            out << op << "(" << *expr1 << ", " << *expr2 << ")";
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

    private:
        Expr *expr;
        std::string op; 
};

class L_val : public Expr {
    public:
        L_val(std::string v, bool b,  std::vector<Expr *> e = {}) : var(v), is_string(b), expr_list(e) {}

        void append(Expr *e) {expr_list.push_back(e);}

        void printAST(std::ostream &out) const override {
            out << "L_val(" << var;
            for (const auto &s : expr_list) {
                out << "[" << *s << "]";
            }
            out << ")";
        }

    private:
        std::string var;
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
    private:
        std::vector<Expr *> expr_list;
};

class FuncCall : public Expr {
    public:
        FuncCall(std::string v, ExprAux * e = nullptr): var(v), expr_list(e) {}

        void printAST(std::ostream &out) const override {
            out << "FuncCall(" << var;
            if(expr_list != nullptr) out << *expr_list;
            out << ")";
        }
    private:
        std::string var;
        ExprAux * expr_list;
};


class Cond : public Expr {
    public:
        Cond(Expr *e) : cond(e) {}

        void printAST(std::ostream &out) const override {
            out << "Cond(" << *cond << ")";
        }
    private:
        Expr *cond;
};


/* Here we define Statements*/



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

    private:
};

class Return : public SimpleStmt {
    public:
        Return(Expr *e): expr(e) {}

        void printAST(std::ostream &out) const override {
            out << "Return(" << *expr << ")";
        }

    private:
        Expr *expr;
};

class Loop : public SimpleStmt {
    public:
        Loop(Block *b, std::string v = ""): name(v), block(b) {}

        void printAST(std::ostream &out) const override {
            out << "Loop(" << name << *block << ")";
        }

    private:
        std::string name;
        Block *block;
};

class Break : public SimpleStmt {
    public:
        Break(std::string n = ""): name(n) {}
        void printAST(std::ostream &out) const override {
            out << "Break(" << name << ")";
        }

    private:
        std::string name;
};

class Continue : public SimpleStmt {
    public:
        Continue(std::string n = ""): name(n) {}
        void printAST(std::ostream &out) const override {
            out << "Continue(" << name << ")";
        }

    private:
        std::string name;
};


class ProcCall : public SimpleStmt {
    public:
        ProcCall(std::string v, ExprAux * e = nullptr): var(v), expr_list(e) {}

        void printAST(std::ostream &out) const override {
            out << "ProcCall(" << var;
            if(expr_list != nullptr) out << ", " << *expr_list;
            out << ")";
        }
    private:
        std::string var;
        ExprAux * expr_list;

};

class Assign : public SimpleStmt {
    public:
        Assign(L_val *l, Expr *e) : l_val(l), expr(e) {}

        void printAST(std::ostream &out) const override {
            out << "Assign(" << *l_val << ", " << *expr << ")"; 
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
    private:
        Cond *cond;
        Block *block;
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

    private:
        std::vector<Elif *> elif_list;
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

    private:
        Cond *cond;
        Block *block1;
        Block *block2;
        IfAux *elif_list;
};






/* We implement the remaining grammar rules*/


class DataType : public AST {
    public:
        DataType(std::string t): dtype(t) {}
        void printAST(std::ostream &out) const override {
            out << "DataType(" << dtype << ")";
        }
    private:
        std::string dtype;
};

class Type : public AST {
    public:
        Type(DataType *dt, std::vector<Int *> d = {}): dtype(dt), dim(d) {}

        void append(Int * d) {dim.push_back(d);}

        void printAST(std::ostream &out) const override {
            out << "Type(" << *dtype;

            for (const auto &s : dim) {  
                out << "[" << *s << "]";
            }
            out << ")";
        }
    private:
        DataType *dtype;
        std::vector<Int *> dim;
        
};

class IdAux : public AST {
    public:
        IdAux(std::string v) {var = {}; var.push_back(v);}

        void append(std::string v) {var.push_back(v);}

        void printAST(std::ostream &out) const override {
            out << "IdAux(";
            bool first = true;
            for (const auto &s : var) {  
                if (first) {out << s; first = false;}
                else {out << ", " << s;}
            }
            out << ")";
        }
    
    private:
        std::vector<std::string> var; 
};

class VarDef : public Def {
    public:
        VarDef(IdAux *v, Type *t): var_list(v), type(t) {}
        void printAST(std::ostream &out) const override {
            out << "VarDef(" << *var_list << ", " << *type << ")";
        }

    private:
        IdAux *var_list;
        Type *type;
};

class ArrayType : public AST {
    public:
        ArrayType(DataType *dt, std::vector<Int *> d = {}): dtype(dt), dim(d) {}

        void append(Int * d) {dim.push_back(d);}

        void printAST(std::ostream &out) const override {
            out << "ArrayType(" << *dtype;
            if (dim.empty()) out << "[]";
            else {
                for (const auto &s : dim) {  
                   out << "[" << *s << "]";
                }
            }
            out << ")";
        }
    private:
        DataType *dtype;
        std::vector<Int *> dim;
};

class FparType : public AST {
    public:
        FparType(Type *t = nullptr, DataType *dt = nullptr, ArrayType *at = nullptr):
          type(t), ref_dt(dt) ,atype(at) {}

        void printAST(std::ostream &out) const override {
            out << "FparType(";
            if (type != nullptr) out << *type;
            else if (ref_dt != nullptr) out << *ref_dt;
            else if (atype != nullptr) out << *atype;
            else {out << "There is a bug in fpar_type";}
            out << ")";
        }

    private:
        Type *type;
        DataType *ref_dt;
        ArrayType *atype;
};


class FparDef : public AST {
    public:
        FparDef(IdAux *vl, FparType * fp): var_list(vl), fpar_type(fp) {}

        void printAST(std::ostream &out) const override {
            out << "FparDef("<< *var_list << ", " << *fpar_type << ")";
        }

    private:
        IdAux *var_list;
        FparType * fpar_type;
};



class HeaderAux : public AST {
    public:
        HeaderAux(): par_list() {}
        
        void append(FparDef * fp) {par_list.push_back(fp);}

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

    private:
        std::vector<FparDef *> par_list;
};



class Header : public AST {
    public:
        Header(std::string v, DataType *dt, HeaderAux *h): 
        var(v), dtype(dt), par_list(h) {}

        void printAST(std::ostream &out) const override {
            out << "Header(" << var;
            if (dtype != nullptr) out << ", " << *dtype;
            if (par_list !=nullptr) out << ", " << *par_list;
            out << ")";
        }

    private:
        std::string var;
        DataType *dtype;
        HeaderAux *par_list;
};


class FuncDecl : public Def {
    public:
        FuncDecl(Header *h): header(h) {}
        void printAST(std::ostream &out) const override {
            out << "FuncDecl("<< *header << ")";
        }
    private:
        Header *header;
};


class LocalDef : public AST {
    public:
        LocalDef(): def_list() {}

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
    private:
        Header *header;
        LocalDef *local_def;
        Block *block;
};

