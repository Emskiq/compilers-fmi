#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map> 
#include <set>
#include <vector>
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

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
  std::map<Symbol, Symbol> inh_graph;
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  void add_to_class_table(Class_ c);
  bool is_valid();
  bool is_child(Symbol child, Symbol parent);
  Symbol lub(Symbol c1, Symbol c2);
  Class_ get_class(Symbol class_name);
  Formals get_formals(Symbol class_name, Symbol method_name);
  Symbol get_return_type(Symbol class_name, Symbol method_name);
  Symbol get_ancestor_method_class(Symbol class_name, Symbol method_name);
  bool check_method_signature(Symbol c1, Symbol c2, Symbol method_name);
};

struct TypeEnvironment {
  Class_ curr_class;
  SymbolTable<Symbol, Symbol>* O;
  ClassTableP class_table;
};


#endif

