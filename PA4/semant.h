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
#include <vector>

#define TRUE 1
#define FALSE 0

// cout wrapper incase this fucks with grading later
#define CLASSTABLE_LOGIT
#ifdef CLASSTABLE_LOGIT
  #define LOGIT(s) s
#else
  #define LOGIT(s)
#endif

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
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  
protected:
  std::map<Symbol, Class_> mTable;
  std::map<Symbol, std::map<Symbol, Feature> > mClassFeatures;
protected:
  bool DefineClasses(Classes& classes);
  bool LinkClasses(Classes& classes);
  bool DefineFeatures(Classes& classes);
  bool LinkFeatures(Classes& classes);
};


#endif

