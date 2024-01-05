

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"

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
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

// Helper functions

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
    install_basic_classes();
    /* Fill this in */
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        add_to_class_table(classes->nth(i));
    }
}

void ClassTable::install_basic_classes()
{

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
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

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

    add_to_class_table(Object_class);
    add_to_class_table(IO_class);
    add_to_class_table(Int_class);
    add_to_class_table(Bool_class);
    add_to_class_table(Str_class);
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

ostream &ClassTable::semant_error(Class_ c)
{
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error()
{
    semant_errors++;
    return error_stream;
}

void ClassTable::add_to_class_table(Class_ c)
{
    Symbol name = c->get_name();
    Symbol parent = c->get_parent();
    if (parent == Bool || parent == SELF_TYPE || parent == Str)
    {
        this->semant_error(c) << "Class " << name << " cannot inherit class " << parent << std::endl;
    }
    else if (name == SELF_TYPE)
    {
        this->semant_error(c) << "Redefinition of basic class " << name << "." << std::endl;
    }
    else if (m_classes.count(name) == 0 && inh_graph.count(name) == 0)
    {
        m_classes[name] = c;
        inh_graph[name] = parent;
    }
    else
    {
        this->semant_error(c) << "Class " << name << " has already been defined." << std::endl;
    }
}

bool ClassTable::is_valid()
{
    bool is_main_defined = false;
    for (std::map<Symbol, Symbol>::iterator iter = inh_graph.begin();
         iter != inh_graph.end(); ++iter)
    {
        Symbol child = iter->first;
        Symbol parent = iter->second;
        if (child == Main)
            is_main_defined = true;
        while (parent != No_class)
        {
            if (parent == child)
            {
                // Error - cycle detected
                this->semant_error(m_classes[child]) << "Class " << child << " inherits from itself.\n";
                return false;
            }
            else if (inh_graph.count(parent) == 0)
            {
                // Error - parent not found
                this->semant_error(m_classes[child]) << "Class " << child << " inherits from undefined class "
                           << parent << ".\n";
                return false;
            }
            else
                parent = inh_graph[parent];
        }
    }
    if (is_main_defined == false)
    {
        this->semant_error() << "Class Main is not defined.\n";
        return false;
    }
    return true;
}

Symbol ClassTable::lub(Symbol class1, Symbol class2)
{
    Symbol c1 = class1;
    Symbol parent = Object;
    while (c1 != Object)
    {
        Symbol c2 = class2;
        while (c2 != Object)
        {
            if (c1 == c2)
            {
                return c1;
            }
            c2 = inh_graph[c2];
        }
        c1 = inh_graph[c1];
    }
    return Object;
}

bool ClassTable::is_child(Symbol child, Symbol parent)
{
    if (parent == Object)return true;
    if (child == parent) return true;
    else return is_child(inh_graph[child], parent);
}

Formals ClassTable::get_formals(Symbol class_name, Symbol method_name)
{
    Symbol cname = class_name;
    while (cname != No_class)
    {
        Class_ c = m_classes[cname];
        Formals f = c->get_formals(method_name);
        if (f != NULL)
            return f;
        cname = inh_graph[cname];
    }
    return NULL;
}

Symbol ClassTable::get_return_type(Symbol class_name, Symbol method_name) {
    Symbol cname = class_name;
    while (cname != No_class) {
        Class_ c = m_classes[cname];
        Symbol r = c->get_return_type(method_name);
        if (r != NULL)
            return r;
        cname = inh_graph[cname];
    }
    return NULL;
}

Symbol ClassTable::get_ancestor_method_class(Symbol class_name, Symbol method_name) {
    Symbol cname = inh_graph[class_name];
    while (cname != No_class) {
        Class_ c = m_classes[cname];
        if (c->get_return_type(method_name) != NULL)
            return c->get_name();
        cname = inh_graph[cname];
    }
    return NULL;
}

bool ClassTable::check_method_signature(Symbol c1, Symbol c2, Symbol method_name) {
    Class_ class1 = m_classes[c1];
    Class_ class2 = m_classes[c2];
    Formals f1 = class1->get_formals(method_name);
    Formals f2 = class2->get_formals(method_name);
    Symbol ret1 = class1->get_return_type(method_name);
    Symbol ret2 = class2->get_return_type(method_name);
    int i = f1->first();
    int j = f2->first();
    // Check formals
    while (f1->more(i) && f2->more(j)) {
        if (f1->nth(i)->get_type() != f2->nth(j)->get_type())
            return false;
        i = f1->next(i);
        j = f2->next(j);
    }
    if (f1->more(i) || f2->more(j))
        return false;
    // Check return type
    if (ret1 != ret2)
        return false;
    return true;
}

void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *class_table = new ClassTable(classes);
    if (!class_table->errors() && class_table->is_valid()) {
        SymbolTable<Symbol, Symbol> O;
        TypeEnvironment typeenv{NULL, &O, class_table};

        assign_types(typeenv);
    }

    /* some semantic analysis code may go here */
    
    if (class_table->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void program_class::assign_types(TypeEnvironment typeenv)
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        typeenv.O->enterscope();
        typeenv.curr_class = classes->nth(i);
        classes->nth(i)->init_class(typeenv);
        classes->nth(i)->assign_types(typeenv);
        typeenv.O->exitscope();
    }
}

void class__class::init_class(TypeEnvironment typeenv)
{
    if (name != Object)
    {
        typeenv.class_table->m_classes[parent]->init_class(typeenv);
    }

    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        features->nth(i)->add_to_environment(typeenv);
    }
}

Formals class__class::get_formals(Symbol method) {
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        if (feature->isMethod() && feature->get_name() == method)
            return feature->get_formals();
    }
    return NULL;
}

Symbol class__class::get_return_type(Symbol method) {
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature feature = features->nth(i);
        if (feature->isMethod() && feature->get_name() == method)
            return feature->get_return_type();
    }
    return NULL;
}

bool method_class::isMethod() { return true; }
bool attr_class::isMethod() { return false; }

Formals method_class::get_formals() { return formals; }
Symbol method_class::get_return_type() { return return_type; }

Formals attr_class::get_formals() { return NULL; }
Symbol attr_class::get_return_type() { return NULL; }

Symbol method_class::get_name() { return name; };
Symbol attr_class::get_name() { return name; };

void method_class::add_to_environment(TypeEnvironment typeenv)
{ /* skip */
}
void attr_class::add_to_environment(TypeEnvironment typeenv)
{
    if (typeenv.O->probe(name) == NULL) {
        typeenv.O->addid(name, &type_decl);
    } else {
        typeenv.class_table->semant_error(typeenv.curr_class) 
            << "Unable to add attribute "
            << name << " to object map (already defined)." << std::endl;
    }
}


void class__class::assign_types(TypeEnvironment typeenv)
{
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        features->nth(i)->assign_types(typeenv);
    }
}


void method_class::assign_types(TypeEnvironment typeenv)
{
    typeenv.O->enterscope();
    Symbol curr_class = typeenv.curr_class->get_name();
    typeenv.O->addid(self, &curr_class);
    for (int i = formals->first(); formals->more(i); i = formals->next(i))
    {
        formals->nth(i)->assign_types(typeenv);
    }

    Symbol exprS = expr->assign_types(typeenv);
    Symbol anc = NULL;

    if ((anc = typeenv.class_table->get_ancestor_method_class(curr_class, name)) != NULL) {
        if (!typeenv.class_table->check_method_signature(anc, curr_class, name)) {
            typeenv.class_table->semant_error(typeenv.curr_class) << "Overriding method signature of " << name << " for class "
                       << curr_class << " doesn't match method signature for ancestor "
                       << anc << "." << std::endl;
        }
    }

    if (return_type == SELF_TYPE)
    {
        if (exprS != SELF_TYPE)
        {
            typeenv.class_table->semant_error(typeenv.curr_class) << "Inferred return type "
                                                                  << exprS << " to method " << name
                                                                  << " does not conform to declared type " << return_type << "." << std::endl;
        }
    }
    else if (!(typeenv.class_table->inh_graph.count(return_type) > 0))
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Undefined return type "
                                                              << return_type << " in method " << name << "." << std::endl;
    }
    else
    {
        if (exprS == SELF_TYPE)
        {
            exprS = typeenv.curr_class->get_name();
        }
        if (!typeenv.class_table->is_child(exprS, return_type))
        {
            typeenv.class_table->semant_error(typeenv.curr_class) << "Method initialization "
                                                                  << exprS << " is not a subclass of " << return_type << "." << std::endl;
        }
    }
    typeenv.O->exitscope();
}



void attr_class::assign_types(TypeEnvironment typeenv)
{
    typeenv.O->enterscope();
    Symbol curr_class = typeenv.curr_class->get_name();
    typeenv.O->addid(self, &curr_class);
    Symbol t1 = init->assign_types(typeenv);
    typeenv.O->exitscope();
    if (t1 == SELF_TYPE)
        t1 = typeenv.curr_class->get_name();
    if (t1 != No_type) {
        if (!(typeenv.class_table->is_child(t1, type_decl))) {
            ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
            err_stream << "Attribute initialization " << t1
                       << " is not a subclass of " << type_decl << ".\n";
        }
    }
    if (name == self) {
        ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
        err_stream << "'self' cannot be the name of an attribute.\n";
    }
}



void formal_class::assign_types(TypeEnvironment typeenv)
{
    if (typeenv.O->probe(name) != NULL) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Duplicate formal " << name << "." << std::endl;
    } else if (type_decl == SELF_TYPE) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Formal parameter " << name << " cannot have type SELF_TYPE." << std::endl;
    } else {
        typeenv.O->addid(name, &type_decl);
    }
}

Symbol branch_class::assign_types(TypeEnvironment typeenv) {
    if (typeenv.O->probe(name) != NULL) {
        ostream &err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
        err_stream << "Identifier " << name << " already defined in current scope.\n";
        return Object;
    }
    typeenv.O->addid(name, &type_decl);
    return expr->assign_types(typeenv);
}

Symbol assign_class::assign_types(TypeEnvironment typeenv) {
    Symbol t1 = *typeenv.O->lookup(name);
    Symbol t2 = expr->assign_types(typeenv);
    if (t2 == SELF_TYPE) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Cannot assign to 'self'.\n";
        type = Object;
    }
    else if (typeenv.class_table->is_child(t2, t1))
        type = t2;
    else {
        typeenv.class_table->semant_error(typeenv.curr_class) << t2 << " is not a subclass of " << t1 << ".\n";
        type = Object;
    }
    return type;
}

Symbol static_dispatch_class::assign_types(TypeEnvironment typeenv) {
    std::vector<Symbol> eval_types; // Vector of parameter types after evaluation
    Symbol t0 = expr->assign_types(typeenv);
    if (t0 == SELF_TYPE)
        t0 = typeenv.curr_class->get_name();
    if (typeenv.class_table->is_child(t0, type_name)) {
        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            Symbol tn = actual->nth(i)->assign_types(typeenv);
            if (tn == SELF_TYPE)
                tn = typeenv.curr_class->get_name();
            eval_types.push_back(tn);
        }
        Formals formals = typeenv.class_table->get_formals(t0, name);     // Declared formal types
        Symbol ret_type = typeenv.class_table->get_return_type(t0, name); // Declared return type
        if (formals == NULL || ret_type == NULL) {
            ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
            err_stream << "Dispatch to undefined method " << name << ".\n";
            type = Object;
            return type;
        }
        // Type check formal parameters
        std::vector<Symbol>::iterator iter = eval_types.begin();
        int fi = formals->first();
        while (iter != eval_types.end() && formals->more(fi)) {
            Symbol eval_type = *iter;
            Symbol declared_type = formals->nth(fi)->get_type();
            if (declared_type == SELF_TYPE) {
                ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
                err_stream << "Formal parameter cannot have type SELF_TYPE.\n";
            }
            else if (!typeenv.class_table->is_child(eval_type, declared_type)) {
                ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
                err_stream << "Formal parameter declared type " << declared_type
                           << " is not a subclass of " << eval_type << ".\n";
            }
            ++iter;
            fi = formals->next(fi);
        }
        if (iter != eval_types.end() || formals->more(fi)) {
            // If we're here, means the number of parameters didn't match
            // the expected number from the function definition. This should
            // not be possible as long as lexing and parsing is correct.
            ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
            err_stream << "Number of declared formals doesn't match number checked.\n";
        }

        if (ret_type == SELF_TYPE)
            type = t0;
        else
            type = ret_type;
    }
    else {
        ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
        err_stream << "Evaluated class " << t0 << " must be a child of declared class "
                   << type_name << " in static dispatch.\n";
        type = Object;
    }
    return type;
}

Symbol dispatch_class::assign_types(TypeEnvironment typeenv) {
    std::vector<Symbol> eval_types; // Vector of parameter types after evaluation
    Symbol t0 = expr->assign_types(typeenv);
    Symbol curr_class = t0;
    if (t0 == SELF_TYPE)
        curr_class = typeenv.curr_class->get_name();
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol tn = actual->nth(i)->assign_types(typeenv);
        if (tn == SELF_TYPE)
            tn = typeenv.curr_class->get_name();
        eval_types.push_back(tn);
    }
    Formals formals = typeenv.class_table->get_formals(curr_class, name);     // Declared formal types
    Symbol ret_type = typeenv.class_table->get_return_type(curr_class, name); // Declared return type
    if (formals == NULL || ret_type == NULL) {
        ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
        err_stream << "Dispatch to undefined method " << name << ".\n";
        type = Object;
        return type;
    }
 
    // Type check formal parameters
    std::vector<Symbol>::iterator iter = eval_types.begin();
    int fi = formals->first();
    while (iter != eval_types.end() && formals->more(fi)) {
        Symbol eval_type = *iter;
        Symbol declared_type = formals->nth(fi)->get_type();
        if (declared_type == SELF_TYPE) {
            ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
            err_stream << "Formal parameter cannot have type SELF_TYPE.\n";
        }
        else if (!typeenv.class_table->is_child(eval_type, declared_type)) {
            ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
            err_stream << "Formal parameter declared type " << declared_type
                       << " is not a subclass of " << eval_type << ".\n";
        }
        ++iter;
        fi = formals->next(fi);
    }
    if (iter != eval_types.end() || formals->more(fi)) {
        // If we're here, means the number of parameters didn't match
        // the expected number from the function definition. This should
        // not be possible as long as lexing and parsing is correct.
        ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
        err_stream << "Number of declared formals doesn't match number checked.\n";
    }

    if (ret_type == SELF_TYPE)
        type = t0;    
    else
        type = ret_type;
    return type;
}


Symbol cond_class::assign_types(TypeEnvironment typeenv) {
    Symbol t1 = pred->assign_types(typeenv);
    Symbol t2 = then_exp->assign_types(typeenv);
    if (t2 == SELF_TYPE)
        t2 = typeenv.curr_class->get_name();
    Symbol t3 = else_exp->assign_types(typeenv);
    if (t3 == SELF_TYPE)
        t3 = typeenv.curr_class->get_name();
    if (t1 == Bool)
        type = typeenv.class_table->lub(t2, t3);
    else {
        ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
        err_stream << "If condition did not evaluate to a boolean.\n";
        type = Object;
    }
    return type;
}

Symbol loop_class::assign_types(TypeEnvironment typeenv)
{
    if (pred->assign_types(typeenv) != Bool)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Type of pred is not Bool." << std::endl;
    }
    body->assign_types(typeenv);
    type = Object;
    return type;
}

Symbol typcase_class::assign_types(TypeEnvironment typeenv) {
    Symbol t0 = expr->assign_types(typeenv);

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        for (int j = cases->first(); cases->more(j); j = cases->next(j)) {
            if (i != j && cases->nth(i)->get_type() == cases->nth(j)->get_type()) {
                ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
                err_stream << "Duplicate branch " << cases->nth(i)->get_type()
                           << " in case statement.\n";
                type = Object;
                return type;
            }
        }
    }

    Symbol tn = cases->nth(cases->first())->assign_types(typeenv);
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        if (i != cases->first()) {
            typeenv.O->enterscope();
            tn = typeenv.class_table->lub(tn, cases->nth(i)->assign_types(typeenv));
            typeenv.O->exitscope();
        }
    }
    type = tn;
    return type;
}


Symbol block_class::assign_types(TypeEnvironment typeenv)
{
    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        type = body->nth(i)->assign_types(typeenv);
        
    }
    return type;
}

Symbol let_class::assign_types(TypeEnvironment typeenv)
{
    if (identifier == self)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! self in let binding." << std::endl;
        
    }
    Symbol t0 = type_decl;
    typeenv.O->enterscope();
    typeenv.O->addid(identifier, &t0);
    Symbol t1 = init->assign_types(typeenv);
    // No init
    if (t1 != No_type) {
        if (!typeenv.class_table->is_child(t1, t0)) {
            ostream& err_stream = typeenv.class_table->semant_error(typeenv.curr_class->get_filename(), this);
            err_stream << "Expression must evaluate to a child of " << t0 << ".\n";
        }
    }
    type = body->assign_types(typeenv);
    typeenv.O->exitscope();
    return type;
}


Symbol plus_class::assign_types(TypeEnvironment typeenv)
{
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '+' meets non-Int value." << std::endl;
        type = Object;
    }
    else
    {
        type = Int;
    }
    return type;
}

Symbol sub_class::assign_types(TypeEnvironment typeenv)
{
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '-' meets non-Int value." << std::endl;
        type = Object;
    }
    else
    {
        type = Int;
    }
    return type;
}

Symbol mul_class::assign_types(TypeEnvironment typeenv)
{
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '*' meets non-Int value." << std::endl;
        type = Object;
    }
    else
    {
        type = Int;
    }
    return type;
}

Symbol divide_class::assign_types(TypeEnvironment typeenv)
{
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '/' meets non-Int value." << std::endl;
        type = Object;
    }
    else
    {
        type = Int;
    }
    return type;
}

Symbol neg_class::assign_types(TypeEnvironment typeenv)
{
    if (e1->assign_types(typeenv) != Int)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '~' meets non-Int value." << std::endl;
        type = Object;
    }
    else
    {
        type = Int;
    }
    return type;
}

Symbol lt_class::assign_types(TypeEnvironment typeenv)
{
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '<' meets non-Int value." << std::endl;
        type = Object;
    }
    else
    {
        type = Bool;
    }
    return type;
}

Symbol eq_class::assign_types(TypeEnvironment typeenv)
{
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type == Int || e2_type == Int || e1_type == Bool || e2_type == Bool || e1_type == Str || e2_type == Str)
    {
        if (e1_type != e2_type)
        {
            typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '=' meets different types." << std::endl;
            type = Object;
        }
        else
        {
            type = Bool;
        }
    }
    else
    {
        type = Bool;
    }
    return type;
}

Symbol leq_class::assign_types(TypeEnvironment typeenv)
{
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '<=' meets non-Int value." << std::endl;
        type = Object;
    }
    else
    {
        type = Bool;
    }
    return type;
}

Symbol comp_class::assign_types(TypeEnvironment typeenv)
{
    if (e1->assign_types(typeenv) != Bool)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! 'not' meets non-Bool value." << std::endl;
        type = Object;
    }
    else
    {
        type = Bool;
    }
    return type;
}

Symbol int_const_class::assign_types(TypeEnvironment typeenv)
{
    type = Int;
    return type;
}

Symbol bool_const_class::assign_types(TypeEnvironment typeenv)
{
    type = Bool;
    return type;
}

Symbol string_const_class::assign_types(TypeEnvironment typeenv)
{
    type = Str;
    return type;
}

Symbol new__class::assign_types(TypeEnvironment typeenv)
{
    if (type_name != SELF_TYPE && (typeenv.class_table->inh_graph.count(type_name) > 0))
    {
        type = type_name;
    } else {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! type " << type_name << " doesn't exist." << std::endl;
        type = Object;
    }
    return type;
}

Symbol isvoid_class::assign_types(TypeEnvironment typeenv)
{
    e1->assign_types(typeenv);
    type = Bool;
    return type;
}

Symbol no_expr_class::assign_types(TypeEnvironment typeenv)
{
    return No_type;
}


Symbol object_class::assign_types(TypeEnvironment typeenv)
{
    if (name == self)
    {
        type = SELF_TYPE;
        return type;
    }

    Symbol *found_type = typeenv.O->lookup(name);

    if (found_type == NULL)
    {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Cannot find object " << name << std::endl;
        type = Object;
    }
    else
    {
        type = *found_type;
    }

    return type;
}





