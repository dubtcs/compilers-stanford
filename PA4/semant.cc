
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

#include <sstream>
#include <stack>
#include <queue>

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

struct MethodContainer {
    Symbol name;
    Formals args;
    Symbol rv;
};

// Adjacency list of inheritance
// Keys are a class, value is the class it inherits from
// Any class not present in this map are classes that do not inherit anything
static std::map<Symbol, Symbol> inheritanceGraph;

// This is absolutely insane
static ClassTable* _tb;
static std::stack<Class_> cstack;
static ostream& serr();

static void ETypeMismatch(Symbol expected, Symbol actual);

// christ this is ugly
static ScopeContainer scopes;
static std::map<Symbol, Class_> mTable;
static std::map<Symbol, std::map<Symbol, Feature> > mClassFeatures;

static std::set<Symbol> usertypes;

bool Scope::Has(Symbol name) const { return vars.count(name); }
void Scope::Add(Symbol name)
{
    if(!Has(name))
        vars.insert(name);
}

void ScopeContainer::EnterClass(Symbol n) { currentClass = n; }
Symbol ScopeContainer::CurrentClass() { return currentClass; }
void ScopeContainer::ExitClass() { currentClass = NULL; }
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
    std::stack<Symbol>& st = vars[name]; // will construct if it doesn't exist
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
bool ScopeContainer::Has(Symbol name) const {
    if(!scopes.empty()) {
        return vars.count(name);
    }
    return false;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    DeclareClasses(classes);
    DefineClasses(classes);
    DeclareFeatures(classes);
    // LinkFeatures(classes);
}

bool ClassTable::DeclareClasses(Classes& cs)
{
    usertypes.insert(Str);
    usertypes.insert(Int);
    usertypes.insert(Bool);
    
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
        usertypes.insert(id);
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
bool ClassTable::DefineClasses(Classes& cs)
{
    // check for cycling inheritance
    // no extending base types
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        std::set<Symbol> found;
        Class_ cur = cs->nth(i);
        Symbol id = cur->GetID();
        found.insert(id);
        scopes.EnterClass(id);

        Class_ seeker = cur;
        Symbol parent = cur->GetParent();
        if(!mTable.count(parent) && parent != Object)
        {
            semant_error(cur) << "Parent class not defined " << id << " : " << parent << '\n';
            return false;
        }

        // Check for inheritance cycles
        bool broken = false;
        
        // The stack is just the current class' inheritance chain
        std::stack<Symbol> typeStack;
        // Push the current ID to the bottom
        typeStack.push(seeker->GetID());
        while(parent != Object && mTable.count(parent))
        {
            if(found.count(parent))
            {
                semant_error(seeker) << "Cyclic inheritance " << seeker->GetID() << " : " << parent << '\n';
                broken = true;
                break;
            }
            found.insert(parent);
            typeStack.push(parent);
            seeker = mTable.at(parent);
            parent = seeker->GetParent();
        }
        if(!broken && !typeStack.empty()) {
            // The top of the stack will always be the lowest possible type
            Symbol inheritsFrom = typeStack.top();
            typeStack.pop();
            // While there are types in the inheritance chain
            while(!typeStack.empty()) {
                Symbol cur = typeStack.top();
                // Check if the definition exists
                if(!inheritanceGraph.count(cur)) {
                    // Create an entry if it doesn't exist yet
                    inheritanceGraph.insert(std::make_pair(cur, inheritsFrom));
                }
                inheritsFrom = typeStack.top();
                typeStack.pop();
            }
        }

        LOGIT("Linked " << id << " to superclass " << parent << '\n');
        scopes.ExitClass();
        LOGIT("EXITED\n");
    }
    return true;
}

bool ClassTable::DeclareFeatures(Classes& cs)
{
    // methods and attributes
    // both derive Feature_class in cool-tree.h
    LOGIT("Declaring class features\n");
    for(int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        LOGIT("Class" << cs->nth(i)->GetID());
        Class_ cur = cs->nth(i);
        scopes.EnterClass(cur->GetID());
        scopes.Enter(cur->GetID()->get_string());
        Features fs = cur->GetFeatures();
        std::map<Symbol, Feature>& table = mClassFeatures[cur->GetID()];
        for(int b = fs->first(); fs->more(b); b = fs->next(b))
        {
            Feature fe = fs->nth(b);
            if(fe->GetID()->get_string() == "self") {
                serr() << "Cannot declare attribute as self.\n";
                continue;
            }
            table.insert(std::make_pair(fe->GetID(), fe));
            // Add class variables to the current scope
            if(!fe->IsMethod()) {
                scopes.Add(fe->GetID(), fe->GetType());
            }
        }
        LOGIT("Declared class features " << cur->GetID() << ". Total: " <<  table.size() << '\n');
        DefineFeatures(cur);
        scopes.Exit();
        scopes.ExitClass();
    }
    LOGIT("Done with features\n");
    return true;
}

bool ClassTable::DefineFeatures(Class_ c)
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
            if(usertypes.count(at->GetType()) == 0)
                semant_error(c) << "No class defined: " << at->GetType() << '\n';
            Symbol rv = at->GetInit()->Validate();
            if(rv != No_type && rv != at->GetType())
                semant_error(c) << "Expected " << at->GetType() << " but found " << rv << '\n';
            scopes.Add(pair.first, at->GetType());
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

static bool ValidateComparison(Expression lhs, Expression rhs)
{
    bool broken = false;
    Symbol l = lhs->Validate();
    Symbol r = rhs->Validate();
    if(l != Bool)
    {
        serr() << "Expected LHS Int but found " << l;
        broken = true;
    }
    if(r != Bool)
    {
        serr() << "Expected RHS Int but found " << r;
        broken = true;
    }
    return !broken;
}

static void ValidateArith(Expression lhs, Expression rhs)
{
    Symbol t = lhs->Validate();
    if(t != Int)
        serr() << "Expected LHS Int but found " << t << '\n';
    t = rhs->Validate();
    if(t != Int)
        serr() << "Expected RHS Int but found " << t << '\n';
    return;
}

ostream& serr() { return _tb->semant_error(cstack.top()); }

bool attr_class::IsMethod() const { return false; }
bool method_class::IsMethod() const { return true; }

Symbol int_const_class::Validate() const { return Int; }
Symbol bool_const_class::Validate() const { return Bool; }
Symbol string_const_class::Validate() const { return Str; }
Symbol isvoid_class::Validate() const { return Bool; }

Symbol leq_class::Validate() const {
    LOGIT("Less than equal expression\n");
    ValidateComparison(e1, e2);
    return Bool;
}
Symbol lt_class::Validate() const {
    LOGIT("Less than expression\n");
    ValidateComparison(e1, e2);
    return Bool;
}
Symbol eq_class::Validate() const {
    LOGIT("Equal expression\n");
    Symbol ty = e1->Validate();
    Symbol t2 = e2->Validate();
    if(ty == Int || ty == Bool || ty == Str) {
        if(ty != t2) {
            serr() << "Type mismatch: " << ty->get_string() << " and " << t2->get_string() << '\n';
        }
    }
    return Bool;
}
Symbol plus_class::Validate() const {
    LOGIT("Addition expression\n");
    ValidateArith(e1,e2);
    return Int;
}
Symbol sub_class::Validate() const {
    LOGIT("Subtraction expression\n");
    ValidateArith(e1,e2);
    return Int;
}
Symbol mul_class::Validate() const {
    LOGIT("Multiply expression\n");
    ValidateArith(e1,e2);
    return Int;
}
Symbol divide_class::Validate() const {
    LOGIT("Divide expression\n");
    ValidateArith(e1, e2);
    return Int;
}
Symbol neg_class::Validate() const {
    LOGIT("Negate expression\n");
    Symbol t = e1->Validate();
    if(t != Int)
        serr() << "Negate requires Int but found " << t << '\n';
    return Int;
}
Symbol comp_class::Validate() const {
    LOGIT("Compliment expression\n");
    Symbol t = e1->Validate();
    if(t != Int) {
        serr() << "Compliment requires type Int but found " << t << '\n';
    }
    return Int;
}
Symbol new__class::Validate() const {
    LOGIT("New expression\n");
    if(type_name == SELF_TYPE)
        return cstack.top()->GetID(); // ID is the name of the class, so the type name.
    return type_name;
}

Symbol object_class::Validate() const {
    LOGIT("Object expression\n");
    if(name == self)
        return SELF_TYPE;
    if(usertypes.count(name))
        return name;
    serr() << "No class defined: " << name << '\n';
    return Object;
}

Symbol assign_class::Validate() const {
    LOGIT("Assignment expression\n");
    Symbol et = expr->Validate();
    if(scopes.Has(name)) {
        if(scopes.GetType(name) != et)
            serr() << "Assignment expected " << scopes.GetType(name) << " but found " << et << '\n';
    }
    return et;
}

static Symbol ValidateMethod(Symbol callClass, Symbol methodName, Expressions arguments) {
    // callClass should always exist in this map bc how tf else would the expression compile to that outcome
    if(mClassFeatures.at(callClass).count(methodName)) {
        Feature f = mClassFeatures.at(callClass).at(methodName);
        if(f->IsMethod()) { // can also dynamic_cast
            Symbol ftype = f->GetType();
            if(callClass != ftype) {
                serr() << "Type mismatch. Expected " << callClass << " but found " << ftype << '\n';
            }
            method_class* me = static_cast<method_class*>(f);
            // Add all expressions' types into a queue
            std::queue<Symbol> args;
            for(int i = arguments->first(); arguments->more(i); i = arguments->next(i)) {
                Expression cur = arguments->nth(i);
                Symbol ty = cur->Validate();
                args.push(ty);
            }
            Formals ff = me->GetFormals();
            // Check the arg queue with the formals list of the method
            for(int i = ff->first(); ff->more(i); i = ff->next(i)) {
                if(args.empty()) {
                    serr() << "Not enough arguments supplied for method declaration.\n";
                    break;
                }
                Symbol ty = ff->nth(i)->GetType();
                if(ty != args.front()) {
                    serr() << "Type mismatch for agument " << i << ". Expected " << ty << " but found " << args.front() << '\n';
                }
                args.pop();
            }
            if(args.size()) {
                serr() << "Too many arguments supplied for method declaration.\n";
            }
            return ftype;
        } else {
            serr() << "Attribute " << f->GetID() << " is not a method.\n";
        }
    }
    else {
        // WAIT
        // Check inheritance list for overrides n shit
        // No. That is not this functions problem
        serr() << "No method " << methodName << " found in class " << callClass << '\n';
    }
    return No_type;
}

static Symbol GetLowestCommonType(Symbol t1, Symbol t2)
{
    if(!inheritanceGraph.count(t1) || !inheritanceGraph.count(t2)) {
        LOGIT("No common ancestors " << t1 << ", " << t2 << '\n');
        return Object;
    }
    if(t1 == t2) {
        return t1;
    }
    std::set<Symbol> types;
    Symbol seeker = t1;
    while(inheritanceGraph.count(seeker)) {
        if(inheritanceGraph.count(seeker)) {
            Symbol the = inheritanceGraph.at(seeker);
            types.insert(the);
            seeker = the;
        } else {
            break;
        }
    }
    seeker = t2;
    while(inheritanceGraph.count(seeker)) {
        Symbol the = inheritanceGraph.at(seeker);
        if(types.count(the)) {
            return the;
        }
        seeker = the;
    }
    return Object;
}

static Symbol GetSubclassWithAttribute(Symbol currentClass, Symbol att)
{
    if(!mClassFeatures.at(currentClass).count(att)) {
        Symbol ty = Object;
        Symbol seeker = currentClass;
        while(inheritanceGraph.count(seeker)) {
            if(mClassFeatures.at(seeker).count(att)) {
                Feature f = mClassFeatures.at(seeker).at(att);
                if(f->IsMethod()) {
                    ty = seeker;
                    break;
                }
            }
            if(inheritanceGraph.count(seeker)) {
                seeker = inheritanceGraph.at(seeker);
            } else {
                break;
            }
        }
        return ty;
    }
    return currentClass;
}

static bool TypeInheritsFrom(Symbol ty, Symbol from)
{
    Symbol seeker = ty;
    while(inheritanceGraph.count(seeker)) {
        if(seeker == from) {
            return true;
        }
        seeker = inheritanceGraph.at(seeker);
    }
    return false;
}

Symbol dispatch_class::Validate() const {
    LOGIT("Dispatch expression\n");
    Symbol t1 = expr->Validate();
    // Symbol t3 = actual->Validate();
    // what if t1 validation fails and returns type Object?
    if(t1 == No_type)
        t1 = scopes.CurrentClass();
    // Now check if the function is an override or not
    Symbol cc = GetSubclassWithAttribute(t1, name);
    Symbol rt = ValidateMethod(t1, name, actual);
    return rt;
}

Symbol static_dispatch_class::Validate() const {
    LOGIT("Static dispatch expression\n");
    Symbol t1 = expr->Validate();
    if(TypeInheritsFrom(t1, type_name)) {
        Symbol rt = ValidateMethod(type_name, name, actual);
        return ValidateMethod(type_name, name, actual);
    }
    serr() << "Type redefinition " << type_name << " must be a superclass of " << t1 << '\n';
    return No_type;
}

Symbol cond_class::Validate() const {
    LOGIT("Conditional expression\n");
    Symbol pt = pred->Validate();
    if(pt != Bool) {
        ETypeMismatch(Bool, pt);
    }
    // the type is the lowest common ancestor
    // do this shit
    Symbol thenType = then_exp->Validate();
    Symbol elseType = else_exp->Validate();
    Symbol common = GetLowestCommonType(thenType, elseType);
    return common;
}

Symbol loop_class::Validate() const {
    LOGIT("Loop expression\n");
    Symbol pt = pred->Validate();
    if(pt != Bool) {
        serr() << "Loop predicate must be a boolean expression, but is " << pt << '\n';
    }
    return Object;
}

Symbol block_class::Validate() const {
    LOGIT("Block expression\n");
    Symbol rt = No_type;
    for(int i = body->first(); body->more(i); i = body->next(i)) {
        rt = body->nth(i)->Validate();
    }
    return rt;
}

Symbol let_class::Validate() const {
    // oh boy
    // variables declared here are then visible in the scope of the body
    LOGIT("Let expression.\n");
    scopes.Enter();
    scopes.Add(identifier, type_decl);
    Symbol it = init->Validate();
    if(it != No_type) {
        if(!TypeInheritsFrom(type_decl, it)) {
            serr() << "Initializer expects " << type_decl << " but found " << it << '\n';
        }
    }
    Symbol rt = body->Validate();
    scopes.Exit();
    return rt;
}

Symbol typcase_class::Validate() const {
    LOGIT("Type case\n");
    Symbol maintype = expr->Validate();
    Symbol rt = No_type;
    for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
        scopes.Enter();
        branch_class* b = static_cast<branch_class*>(cases->nth(i));
        bool valid = TypeInheritsFrom(b->GetType(), maintype);
        if(!valid) { 
            serr() << "Type redefinition " << b->GetType() << " must be a superclass of " << maintype << '\n';
        }
        Symbol ty = b->GetExpression()->Validate();
        if(i) {
            rt = GetLowestCommonType(rt, ty);
        } else {
            rt = ty;
        }
        scopes.Exit();
    }
    return rt;
}

Symbol formal_class::GetType() const { return type_decl; }

// Used for marking empty expressions
Symbol Expression_class::Validate() const { return No_type; }

void ETypeMismatch(Symbol expected, Symbol actual) {
    serr() << "Type mismatch. Expected " << expected << " but found " << actual << '\n';
}

