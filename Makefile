.PHONY: clean distclean default

LLVMCONFIG=llvm-config-18

CXX=g++
CXXFLAGS=-Wall -g `$(LLVMCONFIG) --cxxflags` -std=c++17 -fexceptions
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: dc

src/llvm/lexer.cpp: src/llvm/lexer.l
	flex -s -o src/llvm/lexer.cpp src/llvm/lexer.l

src/llvm/lexer.o: src/llvm/lexer.cpp src/llvm/lexer.hpp src/llvm/parser.hpp src/llvm/ast.hpp src/llvm/symbol.hpp
	$(CXX) $(CXXFLAGS) -c -o src/llvm/lexer.o src/llvm/lexer.cpp

src/llvm/parser.hpp src/llvm/parser.cpp: src/llvm/parser.y
	bison -dv -o src/llvm/parser.cpp src/llvm/parser.y

src/llvm/parser.o: src/llvm/parser.cpp src/llvm/lexer.hpp src/llvm/ast.hpp src/llvm/symbol.hpp
	$(CXX) $(CXXFLAGS) -c -o src/llvm/parser.o src/llvm/parser.cpp

src/llvm/ast.o: src/llvm/ast.cpp src/llvm/ast.hpp src/llvm/symbol.hpp
	$(CXX) $(CXXFLAGS) -c -o src/llvm/ast.o src/llvm/ast.cpp

dc: src/llvm/lexer.o src/llvm/parser.o src/llvm/ast.o
	$(CXX) $(CXXFLAGS) -o dc $^ $(LDFLAGS)
	@echo "Dana Compiler built successfully."

clean:
	$(RM) src/llvm/lexer.cpp src/llvm/parser.cpp src/llvm/parser.hpp src/llvm/parser.output src/llvm/*.o *~

distclean: clean
	$(RM) dc
