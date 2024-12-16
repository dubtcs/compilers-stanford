
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <sstream>
#include <stack>

#include <set>

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}


// This is absolutely insane
static ClassTable* _tb;
static Symbol ComTypes(Symbol e1, Symbol e2);
static std::stack<Class_> cstack;

bool Scope::Has(Symbol name) const { return vars.count(name); }
void Scope::Add(Symbol name)
{
    if(!Has(name))
        vars.insert(name);
}

void ScopeContainer::Enter(std::string name)
{
    LOGIT("Entering scope: " << name << '\n');
    scopes.push(Scope(name));
}
void ScopeContainer::Exit()
{
    if(!scopes.empty())
    {
        LOGIT("Popping scope " << scopes.top().name << '\n');
        Scope& s = scopes.top();
        // for(std::list<Symbol>::iterator it = s.vars.begin(); it != s.vars.end(); it++)
        // for(const Symbol& sym : s.vars)
        for(std::set<Symbol>::iterator it = s.vars.begin(); it != s.vars.end(); it++)
        {
            // std::stack<Symbol>& st = vars.at(sym);
            std::stack<Symbol>& st = vars.at(*it);
            st.pop();
            if(st.empty())
                vars.erase(*it);
        }
        scopes.pop();
    }
}
void ScopeContainer::Add(Symbol name, Symbol type)
{
    if(scopes.empty())
    {
        _tb->semant_error(cstack.top()) << "No scope defined.\n";
        return;
    }
    Scope& s = scopes.top();
    std::stack<Symbol>& st = vars.at(name);
    // pop top item to replace it in current scope
    // as all redefinitions in a scope hide the previous
    // this should probably throw an error instead
    if(s.Has(name))
        st.pop();
    st.push(type);
    s.Add(name);
}
Symbol ScopeContainer::GetType(Symbol name)
{
    if(!scopes.empty())
    {
        if(vars.count(name)) {
            return vars.at(name).top();
        }
        _tb->semant_error(cstack.top()) << "No variable defined: " << name << "\n";
    }
    _tb->semant_error(cstack.top()) << "No active scopes.\n";
    return No_type;
}
const Scope& ScopeContainer::GetCurrentScope() const { return scopes.top(); }
bool ScopeContainer::IsDefinedInScope(Symbol name) const
{
    if(!scopes.empty())
        return GetCurrentScope().Has(name);        
    return false;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    DefineClasses(classes);
    LinkClasses(classes);
    DefineFeatures(classes);
    // LinkFeatures(classes);
}

bool ClassTable::DefineClasses(Classes& cs)
{
    LOGIT(type_name->get_string() << '\n');
    bool mainFound = false;
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        Class_ cur = cs->nth(i);
        Symbol id = cur->GetID();
        if(mTable.count(id))
        {
            semant_error(cur) << "Class already defined. " << id << '\n';
            return false;
        }
        else
        {
            mainFound |= (id == Main);
            mTable.insert(std::make_pair(id, cur));
        }
        LOGIT("Defined " << id << '\n');
    }
    if(!mainFound)
    {
        semant_error(cs->nth(cs->first())) << "No Main defined.\n";
        return false;
    }
    return true;
}

// This isn't actually linking translation units but whatever. First name that came to mind
bool ClassTable::LinkClasses(Classes& cs)
{
    // check for cycling inheritance
    // no extending base types
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        std::set<Symbol> found;
        Class_ cur = cs->nth(i);
        Symbol id = cur->GetID();
        found.insert(id);

        Class_ seeker = cur;
        Symbol parent = cur->GetParent();
        if(!mTable.count(parent) && parent != Object)
        {
            semant_error(cur) << "Parent class not defined " << id << " : " << parent << '\n';
            return false;
        }

        // Check for inheritance cycles
        while(parent != Object && mTable.count(parent))
        {
            if(found.count(parent))
            {
                semant_error(seeker) << "Cyclic inheritance " << seeker->GetID() << " : " << parent << '\n';
                break;
            }
            found.insert(parent);
            seeker = mTable.at(parent);
            parent = seeker->GetParent();
        }

        LOGIT("Linked " << id << " to superclass " << parent << '\n');
    }
    return true;
}

bool ClassTable::DefineFeatures(Classes& cs)
{
    // methods and attributes
    // both derive Feature_class in cool-tree.h
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        Class_ cur = cs->nth(i);
        Features fs = cur->GetFeatures();
        std::map<Symbol, Feature>& table = mClassFeatures[cur->GetID()];
        for(int b = fs->first(); fs->more(b); b = fs->next(b))
        {
            Feature fe = fs->nth(b);
            table.insert(std::make_pair(fe->GetID(), fe));
        }
        LOGIT("Defined class features " << cur->GetID() << ". Total: " <<  table.size() << '\n');
        ClassFeatures(cur);
    }
    return true;
}

bool ClassTable::LinkFeatures(Classes& cs)
{
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        Class_ cur = cs->nth(i);
        cstack.push(cur);

        std::map<Symbol, Feature> table = mClassFeatures.at(cur->GetID());
        LOGIT("Linking " << cur->GetID() << " features.\n");
        // check for definition errors or some shit
        for(std::map<Symbol, Feature>::iterator it = table.begin(); it != table.end(); it++)
        {
            std::pair<const Symbol, Feature>& p = *it;
            if(method_class* m = dynamic_cast<method_class*>(p.second))
            {
                LOGIT(m->GetID() << " is a method.\n");
                // Check for return types
            }
            else
            {
                attr_class* a = dynamic_cast<attr_class*>(p.second);
                LOGIT(a->GetID() << " is an attribute.\n");
                const Expression& init = a->GetInit();
                if(no_expr_class* noe = dynamic_cast<no_expr_class*>(init))
                {
                    LOGIT("No initializer, continuing.\n");
                    continue;
                }
                LOGIT("Has initializer.\n");

                //Make a way to check sub expressions and error when needed
                if(a->GetType() != init->GetType())
                {
                    semant_error(cur) << "Type mismatch. \"" << p.first << "\" requires " << a->GetType() << " but found " << init->GetType() <<'\n';
                }
                // ComTypes(a->GetType(), init->GetType());
            }
        }
        LOGIT('\n');
        cstack.pop();
    }
    return true;
}

bool ClassTable::ClassFeatures(Class_ c)
{
    LOGIT("Checking features for class: " << c->GetID() << '\n');
    scopes.Enter(c->GetID()->get_string());
    std::map<Symbol, Feature>& features = mClassFeatures.at(c->GetID());
    for(std::map<Symbol, Feature>::iterator it = features.begin(); it != features.end(); it++)
    {
        std::pair<const Symbol, Feature>& pair = *it;
        if(pair.second->IsMethod())
        {
            LOGIT(pair.first->get_string() << " is a method.\n");
            method_class* mt = static_cast<method_class*>(pair.second);
        }
        else
        {
            LOGIT(pair.first->get_string() << " is an attribute.\n");
            attr_class* at = static_cast<attr_class*>(pair.second);
            scopes.Add(pair.first, at->GetType());
            at->GetInit()->Validate(at->GetType());
        }
    }
    scopes.Exit();
    return true;
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    // Holy shit I'm doing this just to get logging without changing function args
    ClassTable *classtable = _tb = new ClassTable(classes);

    /* some semantic analysis code may go here */

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

Symbol ComTypes(Symbol e1, Symbol e2) {
    if(e1 == e2)
        return e1;
    _tb->semant_error(cstack.top()) << "Type mismatch. " << e1 << " and " << e2 << '\n';
    return No_type;
}

static bool CheckArCoOp(Expression lhs, Expression rhs)
{
    bool broken = false;
    if(lhs->GetType() != Int) {
        _tb->semant_error(cstack.top()) << "Left side of expression must be Int but found " << lhs->GetType() << '\n';
        broken = true;
    }
    if(rhs->GetType() != Int) {
        _tb->semant_error(cstack.top()) << "Right side of expression must be Int but found " << rhs->GetType() << '\n';
        broken = true;
    }
    return !broken;
}

static Symbol CheckComparisonOperation(Expression lhs, Expression rhs) {
    // return CheckArCoOp(lhs, rhs) ? Bool : No_type;
    CheckArCoOp(lhs, rhs);
    return Int;
}

static Symbol CheckArithOperation(Expression lhs, Expression rhs) {
    // return CheckArCoOp(lhs, rhs) ? Int : No_type;
    CheckArCoOp(lhs, rhs);
    return Int;
}

bool attr_class::IsMethod() const { return false; }
bool method_class::IsMethod() const { return true; }

// bs I need to add bc static definitions in this file
Symbol int_const_class::GetType() const { return Int; }
Symbol bool_const_class::GetType() const { return Bool; }
Symbol string_const_class::GetType() const { return Str; }

Symbol leq_class::GetType() const { return CheckComparisonOperation(e1, e2); }
Symbol lt_class::GetType() const { return CheckComparisonOperation(e1, e2); }

// bool leq_class::Validate(Symbol rv) const { return CheckComparisonOperation(e1, e2); }
// bool leq_class::Validate(Symbol rv) const { return CheckComparisonOperation(e1, e2); }

Symbol divide_class::GetType() const { return CheckArithOperation(e1, e2); }
Symbol mul_class::GetType() const { return CheckArithOperation(e1, e2); }
Symbol sub_class::GetType() const { return CheckArithOperation(e1, e2); }
Symbol plus_class::GetType() const { return CheckArithOperation(e1, e2); }

Symbol neg_class::GetType() const { return Int; }//return e1->GetType() == Int ? Int : No_type; }

Symbol isvoid_class::GetType() const { return Bool; }

Symbol eq_class::GetType() const {
    Symbol t1 = e1->GetType();
    if(t1 == Int || t1 == Bool || t1 == Str)
    {
        if(t1 != e2->GetType())
        {
            _tb->semant_error(cstack.top()) << "Type mismatch for eq. " << t1 << " and " << e2->GetType() << '\n';
            // return No_type;
        }
    }
    // if neither is a built in type, the pointers will be compared, so itll work regardless
    return Bool;
}

Symbol comp_class::GetType() const {
    if(e1->GetType() != Int) {
        _tb->semant_error(cstack.top()) << "Compliment requires type Int but found " << e1->GetType() << '\n';
        // return No_type;
    }
    return Int;
}

Symbol new__class::GetType() const {
    if(type_name == SELF_TYPE)
        return cstack.top()->GetID();
    return type_name;
}

Symbol let_class::GetType() const {
    // oh boy
    // variables declared here are then visible in the scope of the body

    return No_type;
}

// Used for marking empty expressions
Symbol Expression_class::GetType() const { return No_type; }
bool Expression_class::Validate(Symbol rtype) const { return true; }
