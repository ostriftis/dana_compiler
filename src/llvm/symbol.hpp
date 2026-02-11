#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include <cstdint>
#include <vector>
#include <map>
#include <tuple>
#include <iostream>
#include <iterator>

#include "lexer.hpp"

using namespace llvm;


enum DataType {
    UNDEFINED,
    TYPE_int,
    TYPE_byte,
    TYPE_bool,  //hidden
    TYPE_array,
    TYPE_void,  //for functions
    TYPE_func,  //hidden
    TYPE_loop   //hidden
};

/*This class is used to reprsent an array type*/

class array {
  public:
    array(const array &other) {
        size = other.size;
        type = other.type;
    }
    array(std::vector<int> s, DataType t) {
        size = s;
        type = t;
    }
    
    ~array() {}
    std::vector<int> get_size() {return size;}

    DataType get_type() {return type;}
    std::vector<int> size;
    DataType type;
        
};

/*Used as a generalized type for a datatype and an array type*/
struct gen_type {
  DataType dtype;
  array *ar;

  gen_type(const gen_type &other) {
    dtype = other.dtype;
    if (other.ar) {
        ar = new array(*other.ar); // copy array object
    } else {
        ar = nullptr;
    }
}

  gen_type(DataType dt, std::vector<int> d = {}) {
    if (d.empty()) {
      dtype = dt;
      ar = nullptr;
    }
    else {
      ar = new array(d, dt);
      dtype = TYPE_array;
    }
  }
  ~gen_type() {delete ar;}
};

/*This class is used to represent a function symbol table entry*/

class func {
  public:
    DataType type;
    std::vector<std::tuple<std::string, gen_type *, bool>> params;


    func(DataType t, std::vector<std::tuple<std::string, gen_type *, bool>> p):  type(t), params(p) {}

    int par_num() {return params.size();}
    DataType func_type() {return type;}

};


struct STEntry {
  gen_type *type;
  // Not null if stentry is a function
  func *f;

  //This field is to check wether this entry (parameter) is passed by val or by ref.
  bool by_ref;


  STEntry() {}
  STEntry(DataType t) {
    type = new gen_type(t);
    f = nullptr;
    by_ref = true;
  }

  STEntry(DataType t, bool b) {
    type = new gen_type(t);
    f = nullptr;
    by_ref = b;
  }

  STEntry(DataType t, std::vector<int> size) {
    type = new gen_type(t, size);
    f = nullptr;
    by_ref = true;
  }

  STEntry(DataType t, std::vector<std::tuple<std::string, gen_type *, bool>> p) {
    type = new gen_type(TYPE_func);
    f = new func(t, p);
    by_ref = false;
  }


};

class Scope {
 public:
  Scope() {}
  // Insert in case of array and simple type
  void insert(std::string v, DataType t, std::vector<int> size = {}) {
    if (locals.find(v) != locals.end()) yyerror("Duplicate variable declaration: " + v);
    /*If var is an array, DataType is the base_type and size is not empty.*/
    if (!size.empty()) locals[v] = STEntry(t, size);
    else locals[v] = STEntry(t);
  }
  void insert(std::string v, DataType t, bool b) {
    if (locals.find(v) != locals.end()) yyerror("Duplicate variable declaration: " + v);
    /*In this case the type is base_type so its safe to */
    locals[v] = STEntry(t, b);
  }
  //Insert in case of function:
  void insert(std::string v, DataType t, std::vector<std::tuple<std::string, gen_type *, bool>> par){
    if (locals.find(v) != locals.end()) yyerror("Duplicate variable declaration: " + v);
    locals[v] = STEntry(t, par);
  }


  STEntry *lookup(std::string v) {
    if (locals.find(v) == locals.end()) {
      return nullptr;
    }
    return &(locals[v]);
  }

  void del_loop(std::string name) {
    if(locals.find(name) != locals.end()) {
      if(locals[name].type->dtype != TYPE_loop) yyerror("Symbol Table Entry not loop: " + name);
      locals.erase(name);
    }
    else {/*do bothing*/}
  }
 private:
  std::map<std::string, STEntry> locals;
};

class SymbolTable {
 public:
  //Case of simple type or array
   void insert(std::string v, DataType t, bool b) {
    
    scopes.back().insert(v, t, b); 
  }


  void insert(std::string v, DataType t, std::vector<int> s = {}) {
    
    if (s.empty()) scopes.back().insert(v, t);
    else scopes.back().insert(v, t, s);
     
  }
  //Case of function
  void insert(std::string v, DataType t, std::vector<std::tuple<std::string, gen_type *, bool>> p) {
    scopes.back().insert(v, t, p);
  }

  STEntry *lookup(std::string v) {
    for (auto s = scopes.rbegin(); s != scopes.rend(); ++s) {
      STEntry *e = s->lookup(v);
      if (e != nullptr) return e;
    }
    std::string msg = "Variable " + v + " not found";
    yyerror(msg);
    return nullptr;
  }
  void enterScope() {
    scopes.push_back(Scope());
  }
  void exitScope() {
    scopes.pop_back();
  }

  void del_loop(std::string name) {
    scopes.back().del_loop(name);
  }


 private:
  std::vector<Scope> scopes;
};



extern SymbolTable st;


/*This class is used for semantic analysis on loops*/
class LoopStack {
  public:
    void insert(std::string n) {
      /*If loop has an identifier we enter a new scope and insert it on the ST.
        We do that because when we want to break a loop, we can just exitScope.*/
      if (n != "") {
        st.insert(n, TYPE_loop);
        }
      loops.push_back(n);
      }

    int find(std::string n) {
      //STEntry *e = nullptr;
      //if (n != "" ) e = st.lookup(n);

      int len = loops.size();
      
      for (int i = len-1; i >= 0; i--) {
        if (loops[i] == n) {
          return i;
        }
      }
      /*This will happen only if there is no loop*/
      return -1;
    }

    void break_loop(std::string n = "") {
      if (n == "") {
        if(loops.empty()) yyerror("Break statement is used outside of a loop");
      }
      else {
        int i = find(n);
        if (i == -1) yyerror("No active loop with label '" + n + "' found");
      }
    }

    void cont_loop(std::string n = "") {
      std::clog << "Entered Continue loop for: " << n << std::endl;
      if (n == "") {
        if(loops.empty()) yyerror("Continue statement is used outside of a loop");
      }
      else {
        int i = find(n);
        if (i == -1) yyerror("No active loop with label '" + n + "' found");
      }
    }

    void pop(std::string n = "") {
      if(n != "") st.del_loop(n);
      if (loops.back() == n) loops.pop_back();
    }


  private:
    std::vector<std::string> loops;
};

extern LoopStack lp;



#endif