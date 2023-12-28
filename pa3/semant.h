#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map> 
#include <set>
#include <list>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;
typedef SymbolTable<Symbol, method_class> MethodTable;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

public:
  std::map<Symbol, Class_> m_classes; 
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  Symbol lub (Symbol A, Symbol B, const TypeEnvironment& typeenv);
  std::list<Symbol> get_parents(Symbol A, const TypeEnvironment& typeenv);
};

struct TypeEnvironment {
  Class_ curr_class;
  SymbolTable<Symbol, Symbol>* O;
  std::map<Symbol, MethodTable>* M;
  ClassTable* class_table;
};

#endif
