#include "ast.hpp"
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/TargetParser/Triple.h>
#include <unistd.h>
#include <cstdlib>
#include <iostream>
#include <string>

LLVMContext AST::TheContext;
IRBuilder<> AST::Builder(AST::TheContext);
std::unique_ptr<Module> AST::TheModule;
std::unique_ptr<legacy::FunctionPassManager> AST::TheFPM;

GlobalVariable *AST::TheVars;
GlobalVariable *AST::TheNL;
Function *AST::TheWriteInteger;
Function *AST::TheWriteString;
Function *AST::TheWriteChar;
Function *AST::TheWriteByte;

Function *AST::TheReadInteger;
Function *AST::TheReadString;
Function *AST::TheReadChar;
Function *AST::TheReadByte;

Function *AST::TheExtend;
Function *AST::TheShrink;

Function *AST::TheStrCat;
Function *AST::TheStrCmp;
Function *AST::TheStrCpy;
Function *AST::TheStrLen;


Type *AST::i8;
Type *AST::i32;
Type *AST::i64;

Type *AST::i8_ptr;
Type *AST::void_type;

extern SymbolTable st;

std::vector<std::map<std::string, AllocaInst*>> NamedValues;
std::vector<std::map<std::string, Type*>> argBaseTypes;

std::map<std::string, std::pair<BasicBlock *, BasicBlock *>> NamedLoops;
std::vector<std::pair<BasicBlock *, BasicBlock *>> Loops;

std::vector<std::vector<func_details *>> func_names;

// Map from function name to its captured variables (for nested functions)
std::map<std::string, std::vector<std::pair<std::string, Type*>>> captured_variables;

const std::string libfuncs[] = {"writeString", "writeInteger", "writeByte", "writeChar", 
                                            "readString", "readInteger", "readByte", "readChar",
                                            "extend", "shrink",
                                            "strcat", "strcmp", "strlen", "strcpy"};

AllocaInst *lookup(std::string v) {
    // Search for the variable starting from the current lexical scope and moving outwards.
    // The func_names vector of vectors acts as our scope stack.
    for (int i = func_names.size() - 1; i >= 0; --i) {
        if (func_names[i].empty()) continue;

        // The last function added at a given scope level is the current one.
        std::string current_func_name = func_names[i].back()->name;
        std::string mangled_name = current_func_name + "_" + v;

        // Search through the maps of allocated variables for the mangled name.
        // We must check all maps because a new scope (e.g., for function parameters)
        // might have been pushed to NamedValues.
        for (auto s = NamedValues.rbegin(); s != NamedValues.rend(); ++s) {
            if (s->count(mangled_name)) {
                return s->at(mangled_name);
            }
        }
    }
    // If the variable is not found in any accessible scope, return nullptr.
    return nullptr;
}


bool is_libfunc(std::string s) {
  for (int i = 0; i < 14; i++) {  // Changed from 12 to 14 to include extend and shrink
    if (libfuncs[i] == s) return true;
  }
  return false;
}

void initialize_st() {
  st.insert("writeString", TYPE_void, {std::make_tuple("s", new gen_type(TYPE_byte, {-1}), true)});
  st.insert("writeInteger", TYPE_void, {std::make_tuple("n", new gen_type(TYPE_int), false)});
  st.insert("writeByte", TYPE_void, {std::make_tuple("b", new gen_type(TYPE_byte), false)});
  st.insert("writeChar", TYPE_void, {std::make_tuple("c", new gen_type(TYPE_byte), false)});
  st.insert("readString", TYPE_void, {std::make_tuple("size", new gen_type(TYPE_int), false), std::make_tuple("buffer", new gen_type(TYPE_byte, {-1}), true)}); 
  st.insert("readInteger", TYPE_int, {std::make_tuple("", new gen_type(TYPE_void), false)});
  st.insert("readByte", TYPE_byte, {std::make_tuple("", new gen_type(TYPE_void), false)});
  st.insert("readChar", TYPE_byte, {std::make_tuple("", new gen_type(TYPE_void), false)});
  st.insert("extend", TYPE_int, {std::make_tuple("b", new gen_type(TYPE_byte), false)});
  st.insert("shrink", TYPE_byte, {std::make_tuple("i", new gen_type(TYPE_int), false)});
  st.insert("strcat", TYPE_void, {std::make_tuple("dest", new gen_type(TYPE_byte, {-1}), true), std::make_tuple("src", new gen_type(TYPE_byte, {-1}), true)});
  st.insert("strcmp", TYPE_int, {std::make_tuple("s1", new gen_type(TYPE_byte, {-1}), true), std::make_tuple("s2", new gen_type(TYPE_byte, {-1}), true)});
  st.insert("strlen", TYPE_int, {std::make_tuple("s", new gen_type(TYPE_byte, {-1}), true)});
  st.insert("strcpy", TYPE_void, {std::make_tuple("dest", new gen_type(TYPE_byte, {-1}), true), std::make_tuple("src", new gen_type(TYPE_byte, {-1}), true)});
}

func_details *get_parent() {
  if (func_names.back().empty()) return func_names[func_names.size()-2].back();
  else return func_names.back().back();
}

// Helper function to get base filename without extension
std::string get_base_filename(const std::string& filepath) {
    size_t last_slash = filepath.find_last_of("/\\");
    size_t last_dot = filepath.find_last_of(".");
    
    std::string filename;
    if (last_slash == std::string::npos) {
        filename = filepath;
    } else {
        filename = filepath.substr(last_slash + 1);
    }
    
    if (last_dot != std::string::npos && last_dot > last_slash) {
        filename = filename.substr(0, last_dot - (last_slash == std::string::npos ? 0 : last_slash + 1));
    }
    
    return filename;
}

// Helper function to get directory from filepath
std::string get_directory(const std::string& filepath) {
    size_t last_slash = filepath.find_last_of("/\\");
    if (last_slash == std::string::npos) {
        return ".";
    }
    return filepath.substr(0, last_slash);
}

// Main compilation function
void AST::compile(const std::string& input_file, bool flag_i, bool flag_f, 
                 bool flag_O, const std::string& output_file) {
    
    // Initialize targets
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();
    
    // Generate LLVM IR
    LLVM_IR_gen(flag_O);
    
    // Determine output paths
    std::string imm_file, asm_file;
    
    if (!input_file.empty()) {
        std::string dir = get_directory(input_file);
        std::string base = get_base_filename(input_file);
        imm_file = dir + "/" + base + ".imm";
        asm_file = dir + "/" + base + ".asm";
    }
    
    // Handle -i flag: output intermediate code to stdout
    if (flag_i) {
        TheModule->print(outs(), nullptr);
        return;
    }
    
        // Handle -f flag: output final code to stdout
    if (flag_f) {
        // Generate assembly to stdout using llc
        // First write IR to a temp file
        std::string temp_ir = "/tmp/dana_temp_" + std::to_string(getpid()) + ".ll";
        std::error_code EC;
        raw_fd_ostream temp_dest(temp_ir, EC, sys::fs::OF_None);
        if (EC) {
            errs() << "Error: Could not create temp file\n";
            exit(1);
        }
        TheModule->print(temp_dest, nullptr);
        temp_dest.close();
        
        // Use llc to generate assembly to stdout
        std::string llc_cmd = "llc-18 \"" + temp_ir + "\" -o -";
        int result = system(llc_cmd.c_str());
        
        // Clean up temp file
        remove(temp_ir.c_str());
        
        if (result != 0) {
            errs() << "Error: Failed to generate assembly code\n";
            exit(1);
        }
        return;
    }
    
    // Normal mode: write .imm and .asm files
    if (!input_file.empty()) {
        // Write intermediate code to .imm file
        std::error_code EC;
        raw_fd_ostream imm_dest(imm_file, EC, sys::fs::OF_None);
        if (EC) {
            errs() << "Error: Could not open file " << imm_file << ": " << EC.message() << "\n";
            exit(1);
        }
        TheModule->print(imm_dest, nullptr);
        imm_dest.flush();
        
        // Write assembly code to .asm file
        // Use llc command to generate assembly from LLVM IR
        std::string llc_cmd = "llc-18 \"" + imm_file + "\" -o \"" + asm_file + "\"";
        int llc_result = system(llc_cmd.c_str());
        if (llc_result != 0) {
            errs() << "Error: Failed to generate assembly code\n";
            exit(1);
        }
        
        // Now compile to executable using clang
        // First check if there's a main function, otherwise just compile to object
        bool has_main = false;
        for (auto &F : TheModule->functions()) {
            if (F.getName() == "main") {
                has_main = true;
                break;
            }
        }
        
        // Get the directory of the current executable to find lib.a
        char exe_path[1024];
        ssize_t len = readlink("/proc/self/exe", exe_path, sizeof(exe_path) - 1);
        std::string lib_path;
        if (len != -1) {
            exe_path[len] = '\0';
            std::string exe_dir = std::string(exe_path);
            size_t last_slash = exe_dir.find_last_of("/\\");
            if (last_slash != std::string::npos) {
                exe_dir = exe_dir.substr(0, last_slash);
            }
            lib_path = exe_dir + "/src/libs/lib.a";
        } else {
            lib_path = "src/libs/lib.a";  // Fallback to relative path
        }
        std::string compile_cmd;
        
        if (has_main) {
            // Compile and link to executable
            compile_cmd = "clang -no-pie -o " + output_file + " " + asm_file + " " + lib_path + " 2>&1";
        } else {
            // Just compile to object file (no main function)
            compile_cmd = "clang -c -o " + output_file + ".o " + asm_file + " 2>&1";
        }
        
        int result = system(compile_cmd.c_str());
        if (result != 0 && has_main) {
            errs() << "Error: Failed to link executable\n";
            exit(1);
        }
    }
}