#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map>
#include <list>
#include <set>
#include <vector>
#include <stack>

#define TRUE 1
#define FALSE 0

// cout wrapper incase this fucks with grading later
#define CLASSTABLE_LOGIT
#ifdef CLASSTABLE_LOGIT
  #define LOGIT(s) std::cout << s
#else
  #define LOGIT(s)
#endif

#define BIT(n) 1 << n

class ClassTable;
typedef ClassTable *ClassTableP;

// Just use this as a lookup table
// typedef std::map<Symbol, Feature> Scope;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class Scope {
public:
  std::string name;
  std::set<Symbol> vars;
  Scope(){}
  Scope(std::string nam): name(nam) {}
  void Add(Symbol name);
  bool Has(Symbol name) const;
};

class ScopeContainer {
protected:
  std::map<Symbol, std::stack<Symbol> > vars;
  std::stack<Scope> scopes;
public:
  // Enter a new scope
  void Enter(std::string name = "");
  // Exit current scope
  void Exit();
  // Add the symbol to the table with the associated type
  void Add(Symbol name, Symbol type);
  // Get the top most scope in the stack
  const Scope& GetCurrentScope() const;
  // Check if the current scope has already defined this name
  bool IsDefinedInScope(Symbol name) const;
  // Check if entire scope has this symbol
  bool Has(Symbol name) const;
  // Get the current type binding for variable name
  Symbol GetType(Symbol name);
};

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
protected:
  bool DeclareClasses(Classes& classes);
  bool DefineClasses(Classes& classes);
  bool DeclareFeatures(Classes& classes);
  bool DefineFeatures(Class_ c);
};


#endif

