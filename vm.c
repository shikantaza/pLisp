#include <stdio.h>
#include <assert.h>
#include <dlfcn.h>

#include "plisp.h"

extern OBJECT_PTR reg_accumulator;
extern OBJECT_PTR reg_next_expression;
extern OBJECT_PTR reg_current_env;
extern OBJECT_PTR reg_current_value_rib;
extern OBJECT_PTR reg_current_stack;

extern OBJECT_PTR TRUE;
extern OBJECT_PTR NIL;

extern OBJECT_PTR CONS;
extern OBJECT_PTR EQ;
extern OBJECT_PTR ATOM;
extern OBJECT_PTR CAR;
extern OBJECT_PTR CDR;

extern OBJECT_PTR ADD;
extern OBJECT_PTR SUB;
extern OBJECT_PTR MULT;
extern OBJECT_PTR DIV;

extern OBJECT_PTR PRINT;
extern OBJECT_PTR DEFVAR;
extern OBJECT_PTR LST;
extern OBJECT_PTR LISTP;
extern OBJECT_PTR SYMBOL_VALUE;
extern OBJECT_PTR DEFMACRO;

extern OBJECT_PTR GT;
extern OBJECT_PTR GENSYM;
extern OBJECT_PTR SETCAR;
extern OBJECT_PTR SETCDR;
extern OBJECT_PTR ERROR;
extern OBJECT_PTR CREATE_PACKAGE;
extern OBJECT_PTR IN_PACKAGE;
extern OBJECT_PTR COMMA;
extern OBJECT_PTR COMMA_AT;
extern OBJECT_PTR EXPAND_MACRO;

extern OBJECT_PTR STRING;
extern OBJECT_PTR MAKE_ARRAY;
extern OBJECT_PTR ARRAY_GET;
extern OBJECT_PTR ARRAY_SET;
extern OBJECT_PTR SUB_ARRAY;
extern OBJECT_PTR ARRAY_LENGTH;
extern OBJECT_PTR PRINT_STRING;
extern OBJECT_PTR LABELS;
extern OBJECT_PTR CREATE_IMAGE;
extern OBJECT_PTR BREAK;
extern OBJECT_PTR LOAD_FOREIGN_LIBRARY;
extern OBJECT_PTR CALL_FOREIGN_FUNCTION;
extern OBJECT_PTR PRINTENV;
extern OBJECT_PTR CURRENTENV;
extern OBJECT_PTR EVAL;

extern OBJECT_PTR RESUME;

extern OBJECT_PTR BACKTRACE;

/*symbols corresponding to assembler mnemonics */
extern OBJECT_PTR HALT;                  
extern OBJECT_PTR REFER;
extern OBJECT_PTR CONSTANT;
extern OBJECT_PTR CLOSE;
extern OBJECT_PTR MACRO;
extern OBJECT_PTR TEST;
extern OBJECT_PTR ASSIGN;
extern OBJECT_PTR DEFINE;         
extern OBJECT_PTR CONTI;
extern OBJECT_PTR NUATE;
extern OBJECT_PTR FRAME;
extern OBJECT_PTR ARGUMENT;
extern OBJECT_PTR APPLY;
extern OBJECT_PTR RETURN;
extern OBJECT_PTR BACKQUOTE;
/* end symbols corresponding to assembler mnemonics */

extern OBJECT_PTR top_level_env;

extern struct node *white;

extern char **strings;

extern unsigned int current_package;
extern package_t *packages;

extern int nof_dl_handles;
extern void **dl_handles;

extern char err_buf[500];

extern BOOLEAN debug_mode;
OBJECT_PTR debug_continuation;
OBJECT_PTR debug_env;

extern OBJECT_PTR execution_stack;
extern OBJECT_PTR debug_execution_stack;

extern  BOOLEAN in_error;

OBJECT_PTR eval()
{
  OBJECT_PTR exp = reg_next_expression;
  OBJECT_PTR opcode = car(reg_next_expression);

  if(opcode == HALT)
  {
    reg_next_expression = NIL;
  }
  else if(opcode == REFER)
  {
    if(is_special_form(CADR(exp)))
      reg_accumulator = CADR(exp);
    else
    {
      OBJECT_PTR res = get_symbol_value(CADR(exp), debug_mode ? debug_env : reg_current_env);
      if(car(res) != NIL)
        reg_accumulator = cdr(res);
      else
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(CADR(exp), buf);
	sprintf(err_buf, "Symbol not bound: %s", buf);
        raise_error(err_buf);
        return NIL;
      }
    }
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONSTANT)
  {
    reg_accumulator = CADR(exp);
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CLOSE)
  {
    reg_accumulator = create_closure_object(reg_current_env, CADR(exp), CADDR(exp));
    reg_next_expression = CADDDR(exp);
  }
  else if(opcode == MACRO)
  {
    reg_accumulator = create_macro_object(reg_current_env, CADR(exp), CADDR(exp));
    reg_next_expression = CADDDR(exp);
  }
  else if(opcode == TEST)
  {
    if(reg_accumulator != NIL)
      reg_next_expression = CADR(exp);
    else
      reg_next_expression = CADDR(exp);
  }
  else if(opcode == ASSIGN)
  {
    if( update_environment(reg_current_env, CADR(exp), reg_accumulator) == NIL)
    {
      char buf[SYMBOL_STRING_SIZE];
      print_symbol(CADR(exp), buf);
      sprintf(err_buf, "Symbol not bound: %s", buf);
      raise_error(err_buf);
      return NIL;
    }
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == DEFINE)
  {
    add_to_top_level_environment(CADR(exp), reg_accumulator);
    reg_accumulator = CADR(exp);
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == CONTI)
  {
    reg_accumulator = create_current_continuation();
    reg_current_value_rib = NIL;
    reg_next_expression = CADR(exp);
  }
  else if(opcode == NUATE)
  {
    reg_current_stack = CADR(exp);
    reg_accumulator = CADDR(exp);
    reg_current_value_rib = NIL;
    reg_next_expression =  cons(RETURN, NIL);
  }
  else if(opcode == FRAME)
  {
    reg_current_stack = cons(create_call_frame(CADR(exp),
                                               reg_current_env,
                                               reg_current_value_rib),
                             reg_current_stack);

    reg_current_value_rib = NIL;
    reg_next_expression = CADDR(exp);
  }
  else if(opcode == ARGUMENT)
  {
    reg_current_value_rib = cons(reg_accumulator, reg_current_value_rib);
    reg_next_expression = CADR(exp);
  }
  else if(opcode == APPLY)
  {
    OBJECT_PTR operator = reg_accumulator;

    execution_stack = cons(cons(reg_accumulator,
                                reg_current_value_rib),
                           execution_stack);

    //primitive operators
    if(IS_SYMBOL_OBJECT(operator))
    {
      char val[SYMBOL_STRING_SIZE];
      print_symbol(operator, val);
        
      if(operator == CONS)
      {
        reg_accumulator = cons(car(reg_current_value_rib), CADR(reg_current_value_rib));
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == EQ)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("EQ expects two arguments");
          return NIL;
        }

        OBJECT_PTR v1 = car(reg_current_value_rib);
        OBJECT_PTR v2 = CADR(reg_current_value_rib);

        reg_accumulator = equal(v1, v2) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == ATOM)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("ATOM expects one argument");
          return NIL;
        }

        OBJECT_PTR v = car(reg_current_value_rib);

        reg_accumulator = is_atom(v) ? TRUE : NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == CAR)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CAR expects one argument, a CONS object");
          return NIL;
        }

        if(!IS_CONS_OBJECT(car(reg_current_value_rib)))
        {
          raise_error("Argument to CAR should be a CONS object");
          return NIL;
        }

        reg_accumulator = CAAR(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);        
      }
      else if(operator == CDR)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CDR expects one argument, a CONS object");
          return NIL;
        }

        if(!IS_CONS_OBJECT(car(reg_current_value_rib)))
        {
          raise_error("Argument to CDR should be a CONS object");
          return NIL;
        }

        reg_accumulator = CDAR(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);        
      }
      else if(operator == ADD)
      {

        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '+' requires at leaset two arguments");
          return NIL;
        }

        float sum = 0;
        OBJECT_PTR rest = reg_current_value_rib;
        BOOLEAN is_float = false;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);
          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum += get_float_value(val);
          }
          else
            sum += get_int_value(val);

          rest = cdr(rest);
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(sum);
        else
          reg_accumulator = convert_int_to_object((int)sum);

          reg_current_value_rib = NIL;
          reg_next_expression = cons(RETURN, NIL);        
      }
      else if(operator == SUB)
      {
        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '-' requires at leaset two arguments");
          return NIL;
        }

        float val;
        BOOLEAN is_float = false;
        
        OBJECT_PTR first = car(reg_current_value_rib);

        if(IS_FLOAT_OBJECT(first))
	{
          is_float = true;
          val = get_float_value(first);
        }
        else
          val = get_int_value(first);

        OBJECT_PTR rest = cdr(reg_current_value_rib);

        float sum = 0;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);

          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum += get_float_value(val);
          }
          else
            sum += get_int_value(val);

          rest = cdr(rest);
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(val - sum);
        else
          reg_accumulator = convert_int_to_object((int)(val - sum));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);        
      }
      else if(operator == MULT)
      {

        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '*' requires at leaset two arguments");
          return NIL;
        }

        float sum = 1;
        OBJECT_PTR rest = reg_current_value_rib;
        BOOLEAN is_float = false;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);

          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum *= get_float_value(val);
          }
          else
            sum *= get_int_value(val);

          rest = cdr(rest);
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(sum);
        else
          reg_accumulator = convert_int_to_object((int)sum);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);        
      }
      else if(operator == DIV)
      {

        if(length(reg_current_value_rib) < 2)
        {
          raise_error("Operator '/' requires at leaset two arguments");
          return NIL;
        }

        float val;
        BOOLEAN is_float = false;

        OBJECT_PTR first = car(reg_current_value_rib);

        if(IS_FLOAT_OBJECT(first))
	{
          is_float = true;
          val = get_float_value(first);
        }
        else
          val = get_int_value(first);

        OBJECT_PTR rest = cdr(reg_current_value_rib);

        float sum = 1;

        while(rest != NIL)
	{
          OBJECT_PTR val = car(rest);

          if(IS_FLOAT_OBJECT(val))
	  {
            is_float = true;
            sum *= get_float_value(val);
          }
          else
            sum *= get_int_value(val);

	    rest = cdr(rest);
        }

        if(sum == 0)
        {
          raise_error("Division by zero");
          return NIL;
        }

        if(is_float)
          reg_accumulator = convert_float_to_object(val / sum);
        else
          reg_accumulator = convert_int_to_object((int)(val / sum));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);        
      }
      else if(operator == ERROR)
      {
        OBJECT_PTR error_string_obj = car(reg_current_value_rib);

        fprintf(stdout, "Error: ");

        if(IS_STRING_LITERAL_OBJECT(error_string_obj))
          fprintf(stdout, "%s\n", strings[error_string_obj >> STRING_LITERAL_SHIFT]);
        else if(is_string_object(error_string_obj))
        {
          RAW_PTR ptr = error_string_obj >> ARRAY_SHIFT;

          int len = get_int_value(get_heap(ptr));

          int i;

          for(i=1; i <= len; i++)
            fprintf(stdout, "%c", get_heap(ptr + i) >> CHAR_SHIFT);
        }
        
        fprintf(stdout, "\n");

        reg_accumulator = NIL;
        reg_current_value_rib = NIL;
        reg_next_expression = NIL;
      }
      else if(operator == PRINT)
      {
        print_object(car(reg_current_value_rib));
        fprintf(stdout, "\n");
        reg_accumulator = car(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == LST)
      {
        OBJECT_PTR rest = reg_current_value_rib;

        reg_accumulator = NIL;

        while(rest != NIL)
        {
          if(reg_accumulator == NIL)
            reg_accumulator = cons(car(rest), NIL);
          else
            set_heap((last_cell(reg_accumulator) >> CONS_SHIFT) + 1, 
                     cons(car(rest), NIL));         

          rest = cdr(rest);
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);

      }
      else if(operator == BACKQUOTE)
      {
        reg_accumulator = eval_backquote(car(reg_current_value_rib));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == LISTP) //can actually do this with a macro (not (atom) ...)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("LISTP requires exactly one argument");
          return NIL;
        }

        reg_accumulator = is_atom(car(reg_current_value_rib)) ? NIL : TRUE;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == SYMBOL_VALUE)
      {

        if(length(reg_current_value_rib) != 1)
        {
          raise_error("SYMBOL-VALUE requires exactly one argument");
          return NIL;
        }

        OBJECT_PTR sym = car(reg_current_value_rib);

        if(!IS_SYMBOL_OBJECT(sym))
        {
          raise_error("Argument to SYMBOL-VALUE should be a symbol object");
          return NIL;
        }

        OBJECT_PTR res = get_symbol_value(sym, reg_current_env);

        if(car(res) == NIL)
        {
          raise_error("Symbol not bound");
          return NIL;
        }

        reg_accumulator = cdr(res);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == GT)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("> requires exactly two arguments");
          return NIL;
        }

        OBJECT_PTR v1 = car(reg_current_value_rib);
        OBJECT_PTR v2 = CADR(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(v1)) && !(IS_FLOAT_OBJECT(v1))) ||
           (!(IS_INTEGER_OBJECT(v2)) && !(IS_FLOAT_OBJECT(v2))))
        {
          raise_error("Arguments to > should be numbers (integer or float)");
          return NIL;
        }

        float val1, val2;
	  
        if(IS_FLOAT_OBJECT(v1))
          val1 = get_float_value(v1);
        else
          val1 = get_int_value(v1);

        if(IS_FLOAT_OBJECT(v2))
          val2 = get_float_value(v2);
        else
          val2 = get_int_value(v2);
	    
        reg_accumulator = (val1 > val2) ? TRUE : NIL;        

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == GENSYM)
      {
        if(reg_current_value_rib != NIL)
        {
          raise_error("GEMSYM requires no argument");
          return NIL;
        }

        reg_accumulator = gensym();
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == SETCAR)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("SETCAR requires two arguments");
          return NIL;
        }

        OBJECT_PTR car_obj = car(reg_current_value_rib);

        if((!(IS_CONS_OBJECT(car_obj))))
        {
          raise_error("First argument to SETCAR should be a CONS object");
          return NIL;
        }

        set_heap((car_obj >> CONS_SHIFT), CADR(reg_current_value_rib));

        reg_accumulator = CADR(reg_current_value_rib);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == SETCDR)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("SETCDR requires two arguments");
          return NIL;
        }

        OBJECT_PTR car_obj = car(reg_current_value_rib);

        if((!(IS_CONS_OBJECT(car_obj))))
        {
          raise_error("First argument to SETCDR should be a CONS object");
          return NIL;
        }

        set_heap((car_obj >> CONS_SHIFT) + 1, CADR(reg_current_value_rib));

        reg_accumulator = CADR(reg_current_value_rib);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == CREATE_PACKAGE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CREATE-PACKAGE requires exactly one argument");
          return NIL;
        }

        OBJECT_PTR package = car(reg_current_value_rib);

        char *package_name = (char *)convert_to_upper_case(strings[package >> STRING_LITERAL_SHIFT]);

        if(!strcmp(package_name,"CORE"))
	{
          raise_error("Core package already exists");
          return NIL;
        }

        create_package(package_name);

        reg_accumulator = package;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == IN_PACKAGE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("IN-PACKAGE requires exactly one argument");
          return NIL;
        }

        OBJECT_PTR package = car(reg_current_value_rib);
        char *package_name = (char *)convert_to_upper_case(strings[package >> STRING_LITERAL_SHIFT]);

        if(!strcmp(package_name,"CORE"))
	{
          raise_error("Core package cannot be updated");
          return NIL;
        }
        else
	{
          int index = find_package(package_name);
          if(index == NOT_FOUND)
	  {
            raise_error("Package does not exist");
            return NIL;
          }
          else
          {
            current_package = index;
            reg_accumulator = NIL;
          }
        }

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == EXPAND_MACRO)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("EXPAND-MACRO requires exactly one argument");
          return NIL;
        }

        OBJECT_PTR macro_body = car(reg_current_value_rib);
        OBJECT_PTR res = get_symbol_value(car(macro_body), reg_current_env);

        if(car(res) == NIL)
	{
          raise_error("Macro undefined");
          return NIL;
	}
        else
        {
          OBJECT_PTR obj = cdr(res);
          OBJECT_PTR args = cdr(macro_body);

          reg_current_value_rib = NIL;

          //build the value rib
          while(args != NIL)
          {
            if(reg_current_value_rib == NIL)
              reg_current_value_rib = cons(car(args), NIL);
            else
              set_heap((last_cell(reg_current_value_rib) >> CONS_SHIFT) + 1, 
                       cons(car(args), NIL));         
            args = cdr(args);
          }

          //place the macro object in the accumulator (to invoke APPLY)
          reg_accumulator = obj;
          reg_next_expression = cons(APPLY, NIL);
          
          //evaluate the macro invocation
          while(reg_next_expression != NIL)
            eval();
        }        

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == APPLY)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("APPLY requires exactly two arguments");
          return NIL;
        }        

        OBJECT_PTR obj = car(reg_current_value_rib);

        if((!(IS_SYMBOL_OBJECT(obj))) &&
           (!(IS_CLOSURE_OBJECT(obj)))     &&
           (!(IS_CONTINUATION_OBJECT(obj))))
        {
          raise_error("First argument to APPLY should be a special form, a closure or a continuation");
          return NIL;
        }

        OBJECT_PTR args = CADR(reg_current_value_rib);
        if((!(IS_CONS_OBJECT(args))))
        {
          raise_error("Second argument to APPLY should be a list of arguments");
          return NIL;
        }

        reg_accumulator = obj;
        reg_current_value_rib = args;

        reg_next_expression = cons(APPLY, NIL);
      }
      else if(operator == STRING)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("STRING requires exactly one argument, a literal string");
          return NIL;
        }        

        OBJECT_PTR string_literal = car(reg_current_value_rib);

        if((!(IS_STRING_LITERAL_OBJECT(string_literal))))
        {
          raise_error("Argument to STRING should be a literal string");
          return NIL;
        }        
 
        reg_accumulator = eval_string(string_literal);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == MAKE_ARRAY)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("MAKE-ARRAY requires exactly two arguments");
          return NIL;
        }        
        
        OBJECT_PTR size = car(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(size))))
        {
          raise_error("First argument to MAKE-ARRAY should be the size of the array (integer)");
          return NIL;
        }        

        reg_accumulator = eval_make_array(size, CADR(reg_current_value_rib));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == ARRAY_SET)
      {
        if(length(reg_current_value_rib) != 3)
        {
          raise_error("ARRAY-SET requires exactly three arguments");
          return NIL;
        }        

        OBJECT_PTR array_obj = car(reg_current_value_rib);

        if((!(IS_ARRAY_OBJECT(array_obj))))
        {
          raise_error("First argument to ARRAY-SET should be an array");
          return NIL;
        }        

        OBJECT_PTR idx = CADR(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(idx))))
        {
          raise_error("Second argument to ARRAY-SET should be an integer (index into the array)");
          return NIL;
        }        

        int array_len = get_int_value(get_heap(array_obj >> ARRAY_SHIFT));

        int index = get_int_value(idx);

        if(index < 0 || (index >= array_len))
        {
          raise_error("Array index out of bounds");
          return NIL;
        }        
 
        set_heap((array_obj >> ARRAY_SHIFT) + index + 1, CADDR(reg_current_value_rib));

        reg_accumulator = CADDR(reg_current_value_rib);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == ARRAY_GET)
      {
        if(length(reg_current_value_rib) != 2)
        {
          raise_error("ARRAY-GET requires exactly two arguments");
          return NIL;
        }        

        OBJECT_PTR array_obj = car(reg_current_value_rib);

        if((!(IS_ARRAY_OBJECT(array_obj))))
        {
          raise_error("First argument to ARRAY-GET should be an array");
          return NIL;
        }        

        OBJECT_PTR idx = CADR(reg_current_value_rib);

        if((!(IS_INTEGER_OBJECT(idx))))
        {
          raise_error("Second argument to ARRAY-GET should be an integer (index into the array)");
          return NIL;
        }        

        int array_len = get_int_value(get_heap(array_obj >> ARRAY_SHIFT));

        int index = get_int_value(idx);

        if(index < 0 || (index >= array_len))
        {
          raise_error("Array index out of bounds");
          return NIL;
        }        

        reg_accumulator = get_heap((array_obj >> ARRAY_SHIFT) + index + 1);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == SUB_ARRAY)
      {
        if(length(reg_current_value_rib) != 3)
        {
          raise_error("SUB-ARRAY requires exactly three arguments");
          return NIL;
        }        

        OBJECT_PTR array = car(reg_current_value_rib);

        if(!(IS_ARRAY_OBJECT(array)))
        {
          raise_error("First argument to SUB-ARRAY should be an ARRAY object");
          return NIL;
        }

        OBJECT_PTR start = CADR(reg_current_value_rib);

        if(!(IS_INTEGER_OBJECT(start)))
        {
          raise_error("Second argument to SUB-ARRAY should be an integer (start index)");
          return NIL;
        }

        if(!(get_int_value(start) >= 0))
        {
          raise_error("Second argument to SUB-ARRAY should be a non-negative integer");
          return NIL;
        }

        OBJECT_PTR array_length = CADDR(reg_current_value_rib);

        if(!(IS_INTEGER_OBJECT(array_length)))
        {
          raise_error("Third argument to SUB-ARRAY should be an integer (length of the sub-array)");
          return NIL;
        }

        if(!(get_int_value(array_length) >= 0))
        {
          raise_error("Third argument to SUB-ARRAY should be a non- negative integer");
          return NIL;
        }

        if((start + get_int_value(array_length)) > get_int_value(get_heap(array >> ARRAY_SHIFT)))
        {
          raise_error("Range (start, length) for SUB-ARRAY out of bounds of the array");
          return NIL;
        }

        reg_accumulator = eval_sub_array(array, start, array_length);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == ARRAY_LENGTH)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("ARRAY-LENGTH requires exactly one argument, an array object");
          return NIL;
        }        

        OBJECT_PTR array = car(reg_current_value_rib);

        if(!(IS_ARRAY_OBJECT(array)))
        {
          raise_error("Argument to ARRAY-LENGTH should be an ARRAY object");
          return NIL;
        }

        reg_accumulator = get_heap(array >> ARRAY_SHIFT);

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == PRINT_STRING)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("PRINT-STRING requires exactly one argument, a string object");
          return NIL;
        }        

        OBJECT_PTR str = car(reg_current_value_rib);

        if(!(is_string_object(str)) && (!(IS_STRING_LITERAL_OBJECT(str))))
        {
          raise_error("Argument to PRINT_STRING should be a string object");
          return NIL;
        }

        print_object(str);
        fprintf(stdout, "\n");

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == CREATE_IMAGE)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("CREATE-IMAGE requires exactly one argument, a string object denoting the file name of the image");
          return NIL;
        }        

        OBJECT_PTR file_name = car(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          raise_error("Argument to PRINT_STRING should be a string object denoting the file name of the image");
          return NIL;
        }

        create_image(strings[file_name >> STRING_LITERAL_SHIFT]);

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == LOAD_FOREIGN_LIBRARY)
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("LOAD_FOREIGN_LIBRARY requires exactly one argument, a string object denoting the library name");
          return NIL;
        }        

        OBJECT_PTR file_name = car(reg_current_value_rib);

        if(!(is_string_object(file_name)) && (!(IS_STRING_LITERAL_OBJECT(file_name))))
        {
          raise_error("Argument to LOAD_FOREIGN_LIBRARY should be a string object denoting the library name");
          return NIL;
        }

        nof_dl_handles++;
  
        void **temp = (void **)realloc(dl_handles, nof_dl_handles * sizeof(void *));

        if(temp == NULL)
        {
          raise_error("Unable to extend memory for dl_handles");
          return NIL;
        }

        dl_handles = temp;

        void *ret = dlopen(strings[file_name >> STRING_LITERAL_SHIFT], RTLD_LAZY);

        if(!ret)
        {
          raise_error("dl_open() failed");
          return NIL;
        }

        dl_handles[nof_dl_handles - 1] = ret;

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == CALL_FOREIGN_FUNCTION)
      {
        if(length(reg_current_value_rib) != 3)
        {
          raise_error("LOAD_FOREIGN_LIBRARY requires exactly three arguments");
          return NIL;
        }        

        reg_accumulator = call_foreign_function(car(reg_current_value_rib),
                                                CADR(reg_current_value_rib),
                                                CADDR(reg_current_value_rib));

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == PRINTENV)
      {
        if(reg_current_env == NIL && !debug_mode)
          print_object(top_level_env);
        else
          print_object(debug_mode ? debug_env : reg_current_env);

        fprintf(stdout, "\n");

        reg_accumulator = NIL;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == CURRENTENV)
      {
        reg_accumulator = debug_mode ? debug_env : reg_current_env;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == EVAL)
      {

        reg_next_expression = cons(FRAME, cons(cons(HALT, NIL),
                                               cons(compile(car(reg_current_value_rib), NIL),
                                                  NIL)));

        while(reg_next_expression != NIL)
          eval();

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
      else if(operator == BREAK)
      {
        debug_mode = true;
        debug_continuation = create_current_continuation();
        debug_env = reg_current_env;
        reg_next_expression = NIL;

        debug_execution_stack = cdr(execution_stack);

      }
      else if(operator == RESUME)
      {
        debug_mode = false;

        reg_current_stack = ((debug_continuation >> CONTINUATION_SHIFT) << CONS_SHIFT) + CONS_TAG;

        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);

        debug_execution_stack = NIL;

        return;
      }
      else if(operator == BACKTRACE)
      {
        print_backtrace();
        reg_next_expression = cons(RETURN, NIL);
        return;
      }
      else
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(operator, buf);
	sprintf(err_buf, "Symbol not bound: %s", buf);
        raise_error(err_buf);
        return NIL;
      }
    }
    else //user-defined operator (closure, macro, or continuation)
    {
      if(IS_CLOSURE_OBJECT(reg_accumulator) || IS_MACRO_OBJECT(reg_accumulator))
      {
        OBJECT_PTR params = get_params_object(reg_accumulator);

        //no longer valid because of &rest -- parameters may
        //not match the value rib
        /* if(length(params) != length(reg_current_value_rib)) */
        /* { */
        /*   raise_error("Arguments to function not supplied\n"); */
        /*   return NIL; */
        /* } */

        OBJECT_PTR params_env = NIL;

        OBJECT_PTR rest_params = params;
        OBJECT_PTR rest_args = reg_current_value_rib;

        while(rest_params != NIL)
        {
          int package_index = car(rest_params) >> (SYMBOL_BITS + SYMBOL_SHIFT);
          int symbol_index =  (car(rest_params) >> SYMBOL_SHIFT) & TWO_RAISED_TO_SYMBOL_BITS_MINUS_1;

          char *param_name = packages[package_index].symbols[symbol_index];

          if(!strcmp(param_name, "&REST"))
          {
            if(params_env == NIL)
              params_env = cons(cons(CADR(rest_params), rest_args), NIL);
            else
              set_heap((last_cell(params_env) >> CONS_SHIFT) + 1, 
                       cons(cons(CADR(rest_params),rest_args), NIL));            
            break;
          }
          else
          {
            if(params_env == NIL)
              params_env = cons(cons(car(rest_params), car(rest_args)), NIL);
            else
              set_heap((last_cell(params_env) >> CONS_SHIFT) + 1, 
                       cons(cons(car(rest_params),car(rest_args)), NIL));         
          }

          rest_params = cdr(rest_params);
          rest_args = cdr(rest_args);
        }

        reg_current_env = cons(params_env, get_env_list(reg_accumulator));
        reg_current_value_rib = NIL;
        reg_next_expression = get_body_object(reg_accumulator); 
      }
      else if(IS_CONTINUATION_OBJECT(reg_accumulator))
      {
        if(length(reg_current_value_rib) != 1)
        {
          raise_error("Continuations take exactly one argument\n");
          return NIL;
        }

        reg_current_stack = ((reg_accumulator >> CONTINUATION_SHIFT) << CONS_SHIFT) + CONS_TAG;

        reg_accumulator = car(reg_current_value_rib);
        reg_current_value_rib = NIL;
        reg_next_expression = cons(RETURN, NIL);
      }
    }
  }
  else if(opcode == RETURN)
  {
    OBJECT_PTR frame = car(reg_current_stack);
    reg_current_stack = cdr(reg_current_stack);

    RAW_PTR ptr = frame >> ARRAY_SHIFT;
    reg_next_expression   = get_heap(ptr+1);
    reg_current_env       = get_heap(ptr+2);
    reg_current_value_rib = get_heap(ptr+3);

    execution_stack = cdr(execution_stack);
  }

  return reg_accumulator;
}

OBJECT_PTR create_call_frame(OBJECT_PTR next_expression,
                             OBJECT_PTR env,
                             OBJECT_PTR rib)
{
  log_function_entry("create_call_frame");

  RAW_PTR ptr = object_alloc(4);

  set_heap(ptr, convert_int_to_object(3));
  set_heap(ptr+1, next_expression);
  set_heap(ptr+2, env);
  set_heap(ptr+3, rib);

  insert_node(&white, create_node((ptr << ARRAY_SHIFT) + ARRAY_TAG));

  log_function_exit("create_call_frame");

  return (ptr << ARRAY_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR create_current_continuation()
{
  return ((reg_current_stack >> CONS_SHIFT) << CONTINUATION_SHIFT) + CONTINUATION_TAG;
}

void raise_error(char *err_str)
{
  fprintf(stdout, "%s\n", err_str);
  if(cdr(execution_stack) != NIL)
  {
    fprintf(stdout, "Begin backtrace\n");
    print_backtrace();
    fprintf(stdout, "End backtrace\n");
  }
  reg_accumulator = NIL;
  reg_current_value_rib = NIL;
  reg_next_expression = NIL;

  in_error = true;
}

void print_stack()
{
  OBJECT_PTR rest = reg_current_stack;

  while(rest != NIL)
  {
    OBJECT_PTR frame = car(rest);

    RAW_PTR ptr = frame >> ARRAY_SHIFT;

    printf("---- begin frame %0x----\n, frame");
    printf("Next expression: ");
    print_object(get_heap(ptr+1));
    printf("\n");

    printf("Env: ");
    print_object(get_heap(ptr+2));
    printf("\n");

    printf("Value rib: ");
    print_object(get_heap(ptr+3));
    printf("\n");
    printf("---- end frame %0x----\n", frame);

    rest = cdr(rest);
  }
}

OBJECT_PTR eval_backquote(OBJECT_PTR form)
{
  assert(is_valid_object(form));

  if(is_atom(form))
    return form;

  OBJECT_PTR car_obj = car(form);

  assert(is_valid_object(car_obj));

  if(IS_SYMBOL_OBJECT(car_obj))
  {
    char buf[SYMBOL_STRING_SIZE];
    print_symbol(car_obj, buf);

    if(car_obj == COMMA)
    {
      reg_next_expression = cons(FRAME, cons(cons(HALT, NIL),
                                             cons(compile(CADR(form), NIL),
                                                  NIL)));

      reg_current_value_rib = NIL;

      while(reg_next_expression != NIL)
        eval();

      reg_next_expression = cons(RETURN, NIL);
      reg_current_value_rib = NIL;

      return reg_accumulator;
    }
  }

  if(form_contains_comma_at(form))
  {
    //1. loop through elements in form
    //2. if element is not comma-at, call eval_backquote on
    //   it and append it to the result list without splicing
    //3. if it is comma-at, get its symbol value and
    //   splice the value to the result list
    //4. return the result list

    OBJECT_PTR result = NIL;

    OBJECT_PTR rest = form;

    while(rest != NIL)
    {
      OBJECT_PTR ret;
      OBJECT_PTR obj;

      if(IS_CONS_OBJECT(car(rest)) &&
	 IS_SYMBOL_OBJECT(CAAR(rest)))
      {
	char buf[SYMBOL_STRING_SIZE];
	print_symbol(CAAR(rest), buf);

	if(CAAR(rest) == COMMA_AT)
        {
          reg_next_expression = cons(FRAME, cons(cons(HALT, NIL),
                                                 cons(compile(CADAR(rest), NIL),
                                                      NIL)));

          reg_current_value_rib = NIL;

          while(reg_next_expression != NIL)
            eval();

          reg_next_expression = cons(RETURN, NIL);
          reg_current_value_rib = NIL;

	  obj = reg_accumulator;

	  if(result == NIL)
	    result = obj;
	  else
	    set_heap((last_cell(result) >> CONS_SHIFT) + 1, obj);
	}
	else
	{
	  obj = eval_backquote(car(rest));
	  
	  if(result == NIL)
	    result = cons(obj, NIL);
	  else
	    set_heap((last_cell(result) >> CONS_SHIFT) + 1, cons(obj, NIL));
	}
      }
      else
      {
	obj = eval_backquote(car(rest));

	if(result == NIL)
	  result = cons(obj, NIL);
	else
	  set_heap((last_cell(result) >> CONS_SHIFT) + 1, cons(obj, NIL));
      }
      rest = cdr(rest);
    }

    return result;
  }

  return cons(eval_backquote(car(form)),
	      eval_backquote(cdr(form)));

}

OBJECT_PTR eval_string(OBJECT_PTR literal)
{
  assert(IS_STRING_LITERAL_OBJECT(literal));

  char *str_val = strings[literal >> STRING_LITERAL_SHIFT];

  char *ptr = NULL;

  int len = strlen(str_val);

  RAW_PTR raw_ptr = object_alloc(len + 1);

  set_heap(raw_ptr, convert_int_to_object(len));

  int i=1;

  for(ptr=str_val;*ptr;ptr++) 
  { 
    set_heap(raw_ptr + i, (*ptr << CHAR_SHIFT) + CHAR_TAG);
    i++;
  }

  insert_node(&white, create_node((raw_ptr << ARRAY_SHIFT) + ARRAY_TAG));

  return (raw_ptr << ARRAY_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR eval_make_array(OBJECT_PTR size, OBJECT_PTR default_value)
{
  assert(IS_INTEGER_OBJECT(size));
  
  int sz = get_int_value(size);

  RAW_PTR ptr = object_alloc(sz+1);

  set_heap(ptr, size);

  int i;

  for(i=0; i< sz; i++)
    set_heap(ptr + i + 1, default_value);

  insert_node(&white, create_node((ptr << ARRAY_SHIFT) + ARRAY_TAG));

  return (ptr << ARRAY_SHIFT) + ARRAY_TAG;
}

OBJECT_PTR eval_sub_array(OBJECT_PTR array, OBJECT_PTR start, OBJECT_PTR length)
{

  OBJECT_PTR ret;
  int st = get_int_value(start);
  int len = get_int_value(length);

  RAW_PTR orig_ptr = array >> ARRAY_SHIFT;

  RAW_PTR ptr = object_alloc(len + 1);

  set_heap(ptr, convert_int_to_object(len));

  int i;

  for(i=1; i<=len; i++)
    set_heap(ptr+i, get_heap(orig_ptr + st + i));

  insert_node(&white, create_node((ptr << ARRAY_SHIFT) + ARRAY_TAG));

  ret = (ptr << ARRAY_SHIFT) + ARRAY_TAG;

  log_function_exit("eval_sub_array");

  return ret;
}

BOOLEAN is_permitted_in_debug_mode(OBJECT_PTR exp)
{
  if(IS_CONS_OBJECT(exp))
  {
    OBJECT_PTR car_obj = car(exp);

    if(IS_SYMBOL_OBJECT(car_obj))
    {
      return (car_obj == RESUME) || 
             (car_obj == PRINTENV) || 
             (car_obj == CURRENTENV) ||
             (car_obj == BACKTRACE);
    }

    return false;
  }
  else 
    return IS_SYMBOL_OBJECT(exp);
}

void print_backtrace()
{
  OBJECT_PTR rest = (debug_mode ? debug_execution_stack : cdr(execution_stack));

  while(rest != NIL)
  {
    print_object(car(rest));
    printf("\n");

    rest = cdr(rest);
  }
}