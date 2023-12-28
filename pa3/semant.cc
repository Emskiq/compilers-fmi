

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <list>
#include <vector>
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

ClassTable::ClassTable(Classes classes) : semant_errors(0), error_stream(cerr)
{
    install_basic_classes();
    /* Fill this in */

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

    m_classes.insert(std::make_pair(Object, Object_class));
    m_classes.insert(std::make_pair(IO, IO_class));
    m_classes.insert(std::make_pair(Int, Int_class));
    m_classes.insert(std::make_pair(Bool, Bool_class));
    m_classes.insert(std::make_pair(Str, Str_class));
}

static void add_class_methods(std::map<Symbol, MethodTable>& method_tables, ClassTable& class_table)
{
    for (auto iter = class_table.m_classes.begin(); iter != class_table.m_classes.end(); ++iter) {
        Symbol class_name = iter->first;
        method_tables[class_name].enterscope();
        Features curr_features = class_table.m_classes[class_name]->get_features();
        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
             Feature curr_feature = curr_features->nth(j);
             curr_feature->add_method_to_table(method_tables, class_name);
        }
    }
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

std::list<Symbol> ClassTable::get_parents(Symbol A, const TypeEnvironment& typeenv)
{
    if (A == SELF_TYPE) {
        A = typeenv.curr_class->get_name();
    }

    std::list<Symbol> parents;

    // note that Object's father is No_class
    for (; A != No_class; A = m_classes[A]->get_parent()) {
        parents.push_front(A);
    }

    return parents;
}

Symbol ClassTable::lub(Symbol A, Symbol B, const TypeEnvironment& typeenv)
{
    std::list<Symbol> path1 = get_parents(A, typeenv);
    std::list<Symbol> path2 = get_parents(B, typeenv);

    Symbol ret;
    std::list<Symbol>::iterator iter1 = path1.begin(),
        iter2 = path2.begin();

    while (iter1 != path1.end() && iter2 != path2.end()) {
        if (*iter1 == *iter2) {
            ret = *iter1;
        } else {
            break;
        }

        iter1++;
        iter2++;
    }

    return ret;
}

bool ClassTable::check_inheritance(Symbol ancestor, Symbol child, const TypeEnvironment& typeenv)
{
    if (ancestor == SELF_TYPE) {
        return child == SELF_TYPE;
    }

    if (child == SELF_TYPE) {
        child = typeenv.curr_class->get_name();
    }

    for (; child != No_class; child = m_classes.find(child)->second->get_parent()) {
        if (child == ancestor) {
            return true;
        }
    }
    return false;
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

void program_class::assign_types(TypeEnvironment typeenv)
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {

        if (classes->nth(i)->get_name() == SELF_TYPE) {
            typeenv.class_table->semant_error(classes->nth(i)) << "Error! SELF_TYPE redeclared!" << std::endl;
        }

        //classes->nth(i)->assign_types(typeenv);

        if (typeenv.class_table->m_classes.find(classes->nth(i)->get_name()) == typeenv.class_table->m_classes.end()) {
            typeenv.class_table->m_classes.insert(std::make_pair(classes->nth(i)->get_name(), classes->nth(i)));
        } else {
            typeenv.class_table->semant_error(classes->nth(i)) << "Error! Class " << classes->nth(i)->get_name() << " has been defined!" << std::endl;
            return;
        }
    }

    if (typeenv.class_table->m_classes.find(Main) == typeenv.class_table->m_classes.end()) {
        typeenv.class_table->semant_error() << "Class Main is not defined." << std::endl;
    }

    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        typeenv.curr_class = classes->nth(i);

        Symbol parent_name = typeenv.curr_class->get_parent();
        while (parent_name != Object && parent_name != classes->nth(i)->get_name()) {

            if (typeenv.class_table->m_classes.find(parent_name) == typeenv.class_table->m_classes.end()) {
                typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Cannot find class " << parent_name << std::endl;
                return;
            }

            if (parent_name == Int || parent_name == Str || parent_name == SELF_TYPE || parent_name == Bool) {
                typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Class " << typeenv.curr_class->get_name() << " cannot inherit from " << parent_name << std::endl;
                return;
            }

            typeenv.curr_class = typeenv.class_table->m_classes[parent_name];
            parent_name = typeenv.curr_class->get_parent();

        }

        if (parent_name != Object) {
            typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Cycle inheritance!" << std::endl;
            return;
        }
    }

    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        // Emo: Add class attributes to Type Env (O) and then to the
        //      type analisys
        typeenv.curr_class = classes->nth(i);

        // Get the inheritance parents, add all the attributes.
        std::list<Symbol> parents = typeenv.class_table->get_parents(typeenv.curr_class->get_name(), typeenv);
        for (auto iter = parents.begin(); iter != parents.end(); iter++) {
            typeenv.curr_class = typeenv.class_table->m_classes[*iter];
            Features curr_features = typeenv.curr_class->get_features();
            typeenv.O->enterscope();
            for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
                Feature curr_feature = curr_features->nth(j);
                curr_feature->add_attribute_to_table(typeenv);
            }
        }

        typeenv.curr_class = classes->nth(i);
        Features curr_features = typeenv.curr_class->get_features();

        // Check all features.
        // Emo: Actuall type analysis
        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
            Feature curr_feature = curr_features->nth(j);
            curr_feature->assign_types(typeenv);
        }

        for (int j = 0; j < parents.size(); ++j) {
            typeenv.O->exitscope();
        }
    }

    // for (int i = classes->first(); classes->more(i); i = classes->next(i))
    // {
    //     typeenv.curr_class = classes->nth(i);
    //     classes->nth(i)->assign_types(typeenv);
    // }
}

void class__class::assign_types(TypeEnvironment typeenv)
{
    for (int i = features->first(); features->more(i); i = features->next(i))
    {
        features->nth(i)->assign_types(typeenv);
    }
}

void method_class::add_method_to_table(std::map<Symbol, MethodTable>& method_table, Symbol class_name)
{
    method_table[class_name].addid(name, new method_class(copy_Symbol(name), formals->copy_list(), copy_Symbol(return_type), expr->copy_Expression()));
}

void method_class::assign_types(TypeEnvironment typeenv)
{
    // Symbol target_class_name = NULL;

    // for (const auto& kv : typeenv.class_table->m_classes) {
    //     if (kv.second == typeenv.curr_class) {
    //         target_class_name = kv.first;
    //         break; 
    //     }
    // }

    // (*typeenv.M)[target_class_name].addid(name, new method_class(copy_Symbol(name), formals->copy_list(), copy_Symbol(return_type), expr->copy_Expression()));

    // if (typeenv.class_table->m_classes.find(return_type) == typeenv.class_table->m_classes.end() && return_type != SELF_TYPE) {
    //     typeenv.class_table->semant_error(typeenv.curr_class) << "Error! return type " << return_type << " doesn't exist." << std::endl;
    // }

    // typeenv.O->enterscope();
    // std::set<Symbol> used_names;
    // for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
    //     Symbol name = formals->nth(i)->get_name();
    //     if (used_names.find(name) != used_names.end()) {
    //         typeenv.class_table->semant_error(typeenv.curr_class) << "Error! formal name duplicated. " << std::endl;
    //     } else {
    //         used_names.insert(name);
    //     }

    //     Symbol type = formals->nth(i)->get_type();
    //     if (typeenv.class_table->m_classes.find(type) == typeenv.class_table->m_classes.end()) {
    //         typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Cannot find class " << type << std::endl;
    //     }
    //     if (formals->nth(i)->get_name() == self) {
    //         typeenv.class_table->semant_error(typeenv.curr_class) << "Error! self in formal " << std::endl;
    //     }
    //     typeenv.O->addid(formals->nth(i)->get_name(), new Symbol(formals->nth(i)->get_type()));
    // }
    
    // Symbol expr_type = expr->assign_types(typeenv);
    // // if (typeenv.class_table->CheckInheritance(return_type, expr_type) == false) {
    // //     typeenv.class_table->semant_error(curr_class) << "Error! return type is not ancestor of expr type. " << std::endl;
    // // }
    // typeenv.O->exitscope();
    expr->assign_types(typeenv);
}

void attr_class::add_attribute_to_table(TypeEnvironment& typeenv)
{
    // First check for errors
    if (name == self) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! 'self' cannot be the name of an attribute in class " << typeenv.curr_class->get_name() << std::endl;
    }
    if (typeenv.O->lookup(name) != NULL) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! attribute '" << name << "' already exists!" << std::endl;
        return;
    }

    // if everything went fine add the attribute
    typeenv.O->addid(name, new Symbol(type_decl));
}

void attr_class::assign_types(TypeEnvironment typeenv)
{
    init->assign_types(typeenv);
}

Symbol block_class::assign_types(TypeEnvironment typeenv)
{
    int last = body->first();
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        body->nth(i)->assign_types(typeenv);
        last = i;
    }
    type = body->nth(last)->get_type();
    return type;
}

Symbol let_class::assign_types(TypeEnvironment typeenv)
{
    if (identifier == self) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! self in let binding." << std::endl;
    }
    typeenv.O->enterscope();
    typeenv.O->addid(identifier, &type_decl);

    Symbol init_type = init->assign_types(typeenv);
    if (init_type != No_type) {
        if (typeenv.class_table->check_inheritance(type_decl, init_type, typeenv) == false) {
            typeenv.class_table->semant_error(typeenv.curr_class) << "Error! init value is not child." << std::endl;
        }
    }

    body->assign_types(typeenv);
    type = body->get_type();
    typeenv.O->exitscope();
    return type;
}

Symbol object_class::assign_types(TypeEnvironment typeenv)
{   
    if (name == self) {
        type = SELF_TYPE;
        return type;
    }

    Symbol* found_type = typeenv.O->lookup(name);
    
    if (found_type == NULL) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Cannot find object " << name << std::endl;
        type = Object;
    } else {
        type = *found_type;
    }

    return type;
}

Symbol no_expr_class::assign_types(TypeEnvironment typeenv) {
    return No_type;
}

Symbol isvoid_class::assign_types(TypeEnvironment typeenv) {
    e1->assign_types(typeenv);
    type = Bool;
    return type;
}

Symbol new__class::assign_types(TypeEnvironment typeenv) {
    if (type_name != SELF_TYPE && typeenv.class_table->m_classes.find(type_name) == typeenv.class_table->m_classes.end()) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! type " << type_name << " doesn't exist." << std::endl;
    }
    type = type_name;
    return type;
}

Symbol string_const_class::assign_types(TypeEnvironment typeenv) {
    type = Str;
    return type;
}

Symbol bool_const_class::assign_types(TypeEnvironment typeenv) {
    type = Bool;
    return type;
}

Symbol int_const_class::assign_types(TypeEnvironment typeenv) {
    type = Int;
    return type;
}

Symbol comp_class::assign_types(TypeEnvironment typeenv) {
    if (e1->assign_types(typeenv) != Bool) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! 'not' meets non-Bool value." << std::endl;
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol leq_class::assign_types(TypeEnvironment typeenv) {
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '<=' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol eq_class::assign_types(TypeEnvironment typeenv) {
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type == Int || e2_type == Int || e1_type == Bool || e2_type == Bool || e1_type == Str || e2_type == Str) {
        if (e1_type != e2_type) {
            typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '=' meets different types." << std::endl;
            type = Object;
        } else {
            type = Bool;
        }
    } else {
        type = Bool;
    }
    return type;
}

Symbol lt_class::assign_types(TypeEnvironment typeenv) {
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '<' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol neg_class::assign_types(TypeEnvironment typeenv) {
    if (e1->assign_types(typeenv) != Int) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '~' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol divide_class::assign_types(TypeEnvironment typeenv) {
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '/' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol mul_class::assign_types(TypeEnvironment typeenv) {
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '*' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol sub_class::assign_types(TypeEnvironment typeenv) {
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '-' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol plus_class::assign_types(TypeEnvironment typeenv) {
    Symbol e1_type = e1->assign_types(typeenv);
    Symbol e2_type = e2->assign_types(typeenv);
    if (e1_type != Int || e2_type != Int) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! '+' meets non-Int value." << std::endl;
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol branch_class::assign_types(TypeEnvironment typeenv)
{
    typeenv.O->enterscope();

    typeenv.O->addid(name, new Symbol(type_decl));
    Symbol type = expr->assign_types(typeenv);

    typeenv.O->exitscope();

    return type;
}

Symbol typcase_class::assign_types(TypeEnvironment typeenv) {
    Symbol expr_type = expr->assign_types(typeenv);

    Case branch;
    std::vector<Symbol> branch_types;
    std::vector<Symbol> branch_type_decls;

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch = cases->nth(i);

        Symbol branch_type = branch->assign_types(typeenv);
        branch_types.push_back(branch_type);

        branch_type_decls.push_back(((branch_class *)branch)->get_type_decl());
    }

    for (int i = 0; i < branch_types.size() - 1; ++i) {
        for (int j = i + 1; j < branch_types.size(); ++j) {
            if (branch_type_decls[i] == branch_type_decls[j]) {
                typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Two branches have same type." << std::endl;
            }
        }
    }

    type = branch_types[0];
    for (int i = 1; i < branch_types.size(); ++i) {
        type = typeenv.class_table->lub(type, branch_types[i], typeenv);
    }
    return type;
}

Symbol loop_class::assign_types(TypeEnvironment typeenv) {
    if (pred->assign_types(typeenv) != Bool) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Type of pred is not Bool." << std::endl;
    }
    body->assign_types(typeenv);
    type = Object;
    return type;
}

Symbol cond_class::assign_types(TypeEnvironment typeenv) {
     if (pred->assign_types(typeenv) != Bool) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Predicate should be Boolean!" << std::endl;
    }

    Symbol then_type = then_exp->assign_types(typeenv);
    Symbol else_type = else_exp->assign_types(typeenv);

    if (else_type == No_type) {
        type = then_type;
    } else {
        type = typeenv.class_table->lub(then_type, else_type, typeenv);
    }
    return type;

}

Symbol dispatch_class::assign_types(TypeEnvironment typeenv) {
    bool error = false;

    Symbol expr_type = expr->assign_types(typeenv);

    // Find the method along the inheritance path.
    // We want the definition in a subclass.
    std::list<Symbol> path = typeenv.class_table->get_parents(expr_type, typeenv);
    method_class* method = NULL;

    for (auto iter = path.begin(); iter != path.end(); ++iter) {
        if ((method = typeenv.M->operator[] (*iter).lookup(name)) != NULL) {
            break;
        }
    }

    if (method == NULL) {
        error = true;
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Cannot find method '" << name << "'" << std::endl;
    }

    // Check the params.
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->assign_types(typeenv);
        if (method != NULL) {
            Symbol formal_type = method->get_formals()->nth(i)->get_type();
            if (typeenv.class_table->check_inheritance(formal_type, actual_type, typeenv) == false) {
                typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Actual type " << actual_type << " doesn't suit formal type " << formal_type << std::endl;
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    } else {
        type = method->get_return_type();
        if (type == SELF_TYPE) {
            type = expr_type;
        }
    }

    return type;
}

Symbol static_dispatch_class::assign_types(TypeEnvironment typeenv) {
    bool error = false;

    Symbol expr_class = expr->assign_types(typeenv);

    if (typeenv.class_table->check_inheritance(type_name, expr_class, typeenv) == false) {
        error = true;
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Static dispatch class is not an ancestor." << std::endl;
    }

    // Find the method along the inheritance path.
    // We want the definition in a subclass.
    std::list<Symbol> path = typeenv.class_table->get_parents(type_name, typeenv);
    method_class* method = NULL;
    for (auto iter = path.begin(); iter != path.end(); ++iter) {
        if ((method = typeenv.M->operator[] (*iter).lookup(name)) != NULL) {
            break;
        }
    }

    if (method == NULL) {
        error = true;
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Cannot find method '" << name << "'" << std::endl;
    }

    // Check the params.
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->assign_types(typeenv);
        if (method != NULL) {
            Symbol formal_type = method->get_formals()->nth(i)->get_type();
            if (typeenv.class_table->check_inheritance(formal_type, actual_type, typeenv) == false) {
                typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Actual type " << actual_type << " doesn't suit formal type " << formal_type << std::endl;
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    } else {
        type = method->get_return_type();
        if (type == SELF_TYPE) {
            type = type_name;
        }
    }

    return type;

}

Symbol assign_class::assign_types(TypeEnvironment typeenv) {
    Symbol* lvalue_type = typeenv.O->lookup(name);

    Symbol rvalue_type = expr->assign_types(typeenv);

    if (lvalue_type == NULL) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! Cannot find lvalue " << name << std::endl;
        type = Object;
        return type;
    }
    if (typeenv.class_table->check_inheritance(*lvalue_type, rvalue_type, typeenv) == false) {
        typeenv.class_table->semant_error(typeenv.curr_class) << "Error! lvalue is not an ancestor of rvalue. " << std::endl;
        type = Object;
        return type;
    }
    type = rvalue_type;
    return type;
}

void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *class_table = new ClassTable(classes);

    /* some semantic analysis code may go here */
    SymbolTable<Symbol, Symbol> O;
    std::map <Symbol, MethodTable> M;

    TypeEnvironment typeenv { nullptr, &O, &M, class_table};

    // Add all defined methods in M (method table)
    add_class_methods(M, *class_table);

    // Type analysis
    assign_types(typeenv);

    if (class_table->errors())
    {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}
