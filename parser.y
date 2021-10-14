%{
//#include "quad.h"
#include "avm.h"
int yyerror (char* yaccProvidedMessage);
int yylex(void);
int termflag=1;
int assignflag=0;
int scope=0;

char* leftvalue;
extern char * yyval;
extern int yylineno;
extern char* yytext;
extern FILE * yyin;
extern FILE * yyout;
int anonFuncCount=0;
int arrayFlag=0;
int functionFlag=0;
int IDfoundFlag=0;
node* currentFunction=NULL;
node* tempFunction=NULL;
char* tempName;
node* tempsymnode=NULL;
int loopcounter=0;
int returnPatch=-1;
node* tempNodeForFunctionLocals;
int currscope(){return scope;}
%}



%start program

%token 	IF
%token	ELSE
%token	WHILE
%token	FOR
%token	FUNCTION
%token	RETURN
%token	BREAK
%token	CONTINUE
%token	LOCAL
%token	TRUE
%token	FALSE
%token 	NIL
%token	AND
%token	NOT
%token	OR
%token	ASSIGNMENT
%token	PLUS
%token	MINUS
%token	MULT
%token	DIV
%token	MODULO
%token	EQUAL
%token	NOT_EQUAL
%token	INCREMENT
%token	DECREMENT
%token	GREATER
%token	LESS
%token	GREATER_EQUAL
%token	LESS_EQUAL
%token  NUMBER
%token  REAL_NUMBER
%token  ID
%token  STRING
%token	LEFT_PARENTHESIS
%token	RIGHT_PARENTHESIS
%token	LEFT_SQUARE_BRACKET
%token	RIGHT_SQUARE_BRACKET
%token	LEFT_CURLY_BRACKET
%token	RIGHT_CURLY_BRACKET
%token	SEMICOLON
%token	COMMA
%token	COLON
%token	DOUBLE_COLONS
%token	DOT
%token	DOUBLE_DOT
%token	ONE_LINE_COMMENT
%token 	MULTI_LINE_COMMENT


%token	OTHER
%right	ASSIGNMENT
%left	 OR
%left  AND

%nonassoc	EQUAL NOT_EQUAL
%nonassoc	GREATER GREATER_EQUAL LESS LESS_EQUAL

%left	PLUS MINUS
%left	MULT DIV MODULO

%right NOT INCREMENT DECREMENT UMINUS
%left	DOT DOTS
%left	LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET
%left	LEFT_PARENTHESIS RIGHT_PARENTHESIS
%left	LEFT_CURLY_BRACKET RIGHT_CURLY_BRACKET
%nonassoc IF
%nonassoc ELSE

%type <expression>      assignexpr
%type <expression>      expr
%type <expression>      term
%type <expression>      primary
%type <expression>      objectdef
%type <expression>      funcdef
%type <expression>      const
%type <expression>      lvalue
%type <expression>      member
%type <expression>      call
%type <call>            callsuffix
%type <call>            normcall
%type <call>            methodcall
%type <expression>      elist
%type <expression>      cexprs
%type <expression>      indexed
%type <expression>      clindexedelem
%type <expression>      indexedelem
%type <expression>		  block
%type <expression>		  ifstmt
%type <expression>		  whilestmt
%type <expression>		  loopstmt
%type <intValue>		    whilestart
%type <intValue>		    whilecond
%type <expression>		  forstmt
%type <expression>		  returnstmt
%type <expression> 		  stmt
%type <intValue>        M
%type <intValue>        ifprefix
%type <intValue>        elseprefix
%type <intValue>        N
%type<forStruct>       forprefix
%type <expression>     statements
%type<expression>      cidlist
%type<expression>      idlist
%type<expression>      functor


%union{
   char*  stringValue;
   int    intValue;
   double realValue;
   struct expr* expression;
   struct call* call;
   struct forStruct *forStruct;
}
%%
program : statements{printf(" program --> statements \n");}

		    ;

statements : stmt statements {
                              //  printf("%p %p %p\n",$$,$1,$2);
                                if($1&&$2){

                                  $1->breaklist=merge($1->breaklist,$2->breaklist);
                                  $1->continuelist=merge($1->continuelist,$2->continuelist);
                              //    printf("$1 %p \n",$1);
                                }
                                if($1){
                                  $$=$1;
                                }else if($2){$$=$2;}
                            //    printf("$$ %p \n",$$);

                                printf("statements --> stmt statements\n");}
           |{$$=NULL;printf("statements --> empty\n");}



stmt    : expr SEMICOLON  {

                            if($1->backpatch&&!assignflag){
                              expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                              t->type=constbool_e;
                              t->boolConst=1;
                              arg->type=var_e;
                              arg->sym=newtemp();
                              arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                              arg->strConst=strdup(arg->sym->name);
                              backpatch($1->truelist,nextquadlabel());
                              emit(assign,t,NULL,arg,-1,yylineno);
                              emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                              t1->boolConst=0;
                              t1->type=constbool_e;
                              backpatch($1->falselist,nextquadlabel());
                              emit(assign,t1,NULL,arg,-1,yylineno);
                              assignflag=0;
                            }


                            $$=$1;resetTempVar();printf("stmt-->expr;\n");}
        | ifstmt                    {$$=$1;printf("stmt-->ifstmt\n");}
        | whilestmt                 {
            $$=NULL;
            printf("stmt-->whilestmt\n");}
        | forstmt                   {
              $$=NULL;
              printf("stmt-->forstmt\n");}
        | returnstmt                {
              $$=NULL;
              printf("stmt-->return \n");}
        | BREAK{if (scope==0)printf("ERROR: use of break in scope 0 in line %d\n",yylineno);} SEMICOLON{
            if(loopcounter>0){

                  $$=malloc(sizeof(expr));
                //  printf("%p\n",$$);
                  $$->breaklist=makelist(nextquadlabel(),$$->breaklist);


                  emit(jump,NULL,NULL,NULL,-1,yylineno);

                  }
                  else
                  printf("ERROR: Use of break outside a loop in line %d\n",yylineno);
                   printf("stmt-->break;\n");
                   }
        | CONTINUE{if (scope==0)printf("ERROR: use of continue in scope 0 in line %d\n",yylineno);} SEMICOLON{
                if(loopcounter>0){

                    $$=malloc(sizeof(expr));
                //    printf("%p\n",$$);
                    $$->continuelist=makelist(nextquadlabel(),  $$->continuelist);
                    emit(jump,NULL,NULL,NULL,-1,yylineno);

                      }
                      else
                      printf("ERROR: Use of continue outside a loop in line %d\n",yylineno);
                printf("stmt-->continue;\n");
                }
        | block                     {$$=$1;printf("stmt-->block\n");}
        | funcdef                   {
              $$=NULL;
              printf("stmt-->funcdef\n");}
        | SEMICOLON                 {$$=NULL;resetTempVar();printf("stmt-->;\n");}
        ;




expr    : assignexpr                {$$=$1;printf("expr-->assignexpr\n");}

        | expr OR{
                      if(!$1->isBoolean&&$1->isEmitable){
                          if(!$1->passedByNot){
                            $1->truelist=makelist(nextquadlabel(),$1->truelist);
                            $1->falselist=makelist(nextquadlabel()+1,$1->falselist);
                            emit(if_eq,newexpr_constbool(1),$1,NULL,-1,yylineno);
                            emit(jump,NULL,NULL,NULL,-1,yylineno);
                            $1->isEmitable=0;
                          }else
                                $1->passedByNot=0;

                      }
                  } M expr    {
                                    $$->backpatch=1;

                                    backpatch($1->falselist,$4);



                                      if(!$5->isBoolean&&$5->isEmitable){
                                          if(!$5->passedByNot){
                                            $5->truelist=makelist(nextquadlabel(),$5->truelist);
                                            $5->falselist=makelist(nextquadlabel()+1,$5->falselist);
                                            emit(if_eq,newexpr_constbool(1),$5,NULL,-1,yylineno);
                                            emit(jump,NULL,NULL,NULL,-1,yylineno);
                                            $5->isEmitable=0;
                                          }else
                                                $5->passedByNot=0;

                                      }

                                      if(!$1->isEmitable&&!$5->isEmitable){
                                          $$->isEmitable=0;
                                      }

                                      $$->truelist=merge($1->truelist,$5->truelist);
                                      $$->falselist=$5->falselist;
                                      $$->passedByNot=0;

                                    printf("expr-->expr or expr\n");}

        | expr AND{

                    if(!$1->isBoolean&&$1->isEmitable){
                        if(!($1->passedByNot)){
                          $1->truelist=makelist(nextquadlabel(),$1->truelist);
                          $1->falselist=makelist(nextquadlabel()+1,$1->falselist);
                          emit(if_eq,newexpr_constbool(1),$1,NULL,-1,yylineno);
                          emit(jump,NULL,NULL,NULL,-1,yylineno);
                          $1->isEmitable=0;
                        }else
                              $1->passedByNot=0;

                    }

                    } M expr  {

                                    $$->backpatch=1;

                                    backpatch($1->truelist,$4);
                                     if(!$5->isBoolean&&$5->isEmitable){
                                         if(!$5->passedByNot){
                                           $5->truelist=makelist(nextquadlabel(),$5->truelist);
                                           $5->falselist=makelist(nextquadlabel()+1,$5->falselist);
                                           emit(if_eq,newexpr_constbool(1),$5,NULL,-1,yylineno);
                                           emit(jump,NULL,NULL,NULL,-1,yylineno);
                                           $5->isEmitable=0;
                                         }else
                                               $5->passedByNot=0;

                                     }
                                     if(!$1->isEmitable&&!$5->isEmitable){
                                         $$->isEmitable=0;
                                     }

                                    $$->truelist=$5->truelist;
                                    $$->falselist=merge($1->falselist,$5->falselist);
                                    printf("expr-->expr and expr\n");}

        | NOT expr              {

                                    if($2->isEmitable){
                                      $2->truelist=makelist(nextquadlabel(),$2->truelist);
                                      $2->falselist=makelist(nextquadlabel()+1,$2->falselist);
                                      emit(if_eq,newexpr_constbool(1),$2,NULL,-1,yylineno);
                                      emit(jump,NULL,NULL,NULL,-1,yylineno);
                                      $2->isEmitable=0;
                                    }

                                    swapLists($2->truelist,$2->falselist);
                                    $$=$2;

                                    $$->passedByNot=1;

                                    $$->backpatch=1;

                                    printf("expr--> !expr\n");}

        | expr PLUS expr            {
                                      $$=newexpr(arithexpr_e);
                                      $$->sym=newtemp();
                                      emit(add,$1,$3,$$,-1,yylineno);
                                      printf("expr-->expr + expr\n");}
        | expr MINUS expr           {
                                      $$=newexpr(arithexpr_e);
                                      $$->sym=newtemp();
                                      emit(sub,$1,$3,$$,-1,yylineno);
                                      printf("expr-->expr - expr\n");}
        | expr MULT expr	          {
                                      $$=newexpr(arithexpr_e);
                                      $$->sym=newtemp();
                                      emit(mul,$1,$3,$$,-1,yylineno);
                                      printf("expr-->expr * expr\n");}
        | expr DIV expr		          {
                                      $$=newexpr(arithexpr_e);
                                      $$->sym=newtemp();
                                      emit(divv,$1,$3,$$,-1,yylineno);
                                      printf("expr-->expr / expr\n");}
        | expr MODULO expr		          {
                                      $$=newexpr(arithexpr_e);
                                      $$->sym=newtemp();
                                      emit(mod,$1,$3,$$,-1,yylineno);
                                      printf("expr-->expr % expr\n");}
        | expr EQUAL {}expr	          {
                              expr * arg1=$1,*arg2=$4;
                              if($1->passedByNot/*&&$1->isEmitable*/){

                                expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                t->type=constbool_e;
                                t->boolConst=1;
                                arg->type=var_e;
                                arg->sym=newtemp();
                                arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                arg->strConst=strdup(arg->sym->name);
                                backpatch($1->truelist,nextquadlabel());
                                emit(assign,t,NULL,arg,-1,yylineno);
                                emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);

                                t1->boolConst=0;
                                t1->type=constbool_e;

                                backpatch($1->falselist,nextquadlabel());
                                emit(assign,t1,NULL,arg,-1,yylineno);

                                $1->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                $1->strConst=strdup(arg->sym->name);

                                assignflag=0;
                                arg1=arg;
                                $1->truelist=NULL;
                                $1->falselist=NULL;
                                $1->isEmitable=0;
                              }
                              else if($1->backpatch&&!assignflag){
                                    expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                    t->type=constbool_e;
                                    t->boolConst=1;
                                    arg->type=var_e;
                                    arg->sym=newtemp();
                                    arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                    arg->strConst=strdup(arg->sym->name);

                                    backpatch($1->truelist,nextquadlabel());
                                    emit(assign,t,NULL,arg,-1,yylineno);
                                    emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                                    t1->boolConst=0;
                                    t1->type=constbool_e;
                                    backpatch($1->falselist,nextquadlabel());
                                    emit(assign,t1,NULL,arg,-1,yylineno);
                                    assignflag=0;
                                    arg1=arg;
                                    $1->isEmitable=0;
                                    $1->truelist=NULL;
                                    $1->falselist=NULL;
                                    }

                                    if($4->passedByNot/*&&$4->isEmitable*/){
                                      expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                      t->type=constbool_e;
                                      t->boolConst=1;
                                      arg->type=var_e;
                                      arg->sym=newtemp();
                                      arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                      arg->strConst=strdup(arg->sym->name);

                                      backpatch($4->truelist,nextquadlabel());
                                      emit(assign,t,NULL,arg,-1,yylineno);
                                      emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);

                                      t1->boolConst=0;
                                      t1->type=constbool_e;

                                      backpatch($4->falselist,nextquadlabel());
                                      emit(assign,t1,NULL,arg,-1,yylineno);


                                      $4->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                      $4->strConst=strdup(arg->sym->name);

                                      assignflag=0;
                                      arg2=arg;
                                      $4->isEmitable=0;
                                      $4->truelist=NULL;
                                      $4->falselist=NULL;
                                    }
                                    else if($4->backpatch&&!assignflag){
                                          expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                          t->type=constbool_e;
                                          t->boolConst=1;
                                          arg->type=var_e;
                                          arg->sym=newtemp();
                                          arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                          arg->strConst=strdup(arg->sym->name);
                                          backpatch($4->truelist,nextquadlabel());
                                          emit(assign,t,NULL,arg,-1,yylineno);
                                          emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                                          t1->boolConst=0;
                                          t1->type=constbool_e;
                                          backpatch($4->falselist,nextquadlabel());
                                          emit(assign,t1,NULL,arg,-1,yylineno);
                                          assignflag=0;
                                          arg1=arg;
                                          $4->isEmitable=0;
                                    }
                                  //  if(!$1->isEmitable&&!$4->isEmitable)
                                        $$->isEmitable=0;


                                    $$->isBoolean=1;
                                    $$->backpatch=1;
                                    $$->truelist=makelist(nextquadlabel(),$$->truelist);
                                    $$->falselist=makelist(nextquadlabel()+1,$$->falselist);

                                    emit(if_eq,arg1,arg2,NULL,0,yylineno);
                                    emit(jump,NULL,NULL,NULL,-1,yylineno);

                                    printf("expr-->expr == expr\n");}
        | expr NOT_EQUAL {}expr{


                              expr * arg1=$1,*arg2=$4;
                              if($1->passedByNot){
                              //  if($1->isEmitable){
                                  expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                  t->type=constbool_e;
                                  t->boolConst=1;
                                  arg->type=var_e;
                                  arg->sym=newtemp();
                                  arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                  arg->strConst=strdup(arg->sym->name);
                                  backpatch($1->truelist,nextquadlabel());
                                  emit(assign,t,NULL,arg,-1,yylineno);
                                  emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);

                                  t1->boolConst=0;
                                  t1->type=constbool_e;

                                  backpatch($1->falselist,nextquadlabel());
                                  emit(assign,t1,NULL,arg,-1,yylineno);

                                  $1->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                  $1->strConst=strdup(arg->sym->name);

                                  assignflag=0;
                                  arg1=arg;
                                  $1->truelist=NULL;
                                  $1->falselist=NULL;
                                  $1->isEmitable=0;
                              //  }
                              }
                              else if($1->backpatch&&!assignflag){
                                    expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                    t->type=constbool_e;
                                    t->boolConst=1;
                                    arg->type=var_e;
                                    arg->sym=newtemp();
                                    arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                    arg->strConst=strdup(arg->sym->name);

                                    backpatch($1->truelist,nextquadlabel());
                                    emit(assign,t,NULL,arg,-1,yylineno);
                                    emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                                    t1->boolConst=0;
                                    t1->type=constbool_e;
                                    backpatch($1->falselist,nextquadlabel());
                                    emit(assign,t1,NULL,arg,-1,yylineno);
                                    assignflag=0;
                                    arg1=arg;
                                    $1->truelist=NULL;
                                    $1->falselist=NULL;
                                    $1->isEmitable=0;
                                    }

                                    if($4->passedByNot){

                                    //  if($4->isEmitable){
                                        expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                        t->type=constbool_e;
                                        t->boolConst=1;
                                        arg->type=var_e;
                                        arg->sym=newtemp();
                                        arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                        arg->strConst=strdup(arg->sym->name);

                                        backpatch($4->truelist,nextquadlabel());
                                        emit(assign,t,NULL,arg,-1,yylineno);
                                        emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);

                                        t1->boolConst=0;
                                        t1->type=constbool_e;

                                        backpatch($4->falselist,nextquadlabel());
                                        emit(assign,t1,NULL,arg,-1,yylineno);


                                        $4->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                        $4->strConst=strdup(arg->sym->name);

                                        assignflag=0;
                                        arg2=arg;
                                        $4->truelist=NULL;
                                        $4->falselist=NULL;
                                        $4->isEmitable=0;
                                    //  }
                                    }
                                    else if($4->backpatch&&!assignflag){
                                          expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                          t->type=constbool_e;
                                          t->boolConst=1;
                                          arg->type=var_e;
                                          arg->sym=newtemp();
                                          arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                          arg->strConst=strdup(arg->sym->name);
                                          backpatch($4->truelist,nextquadlabel());
                                          emit(assign,t,NULL,arg,-1,yylineno);
                                          emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                                          t1->boolConst=0;
                                          t1->type=constbool_e;
                                          backpatch($4->falselist,nextquadlabel());
                                          emit(assign,t1,NULL,arg,-1,yylineno);
                                          assignflag=0;
                                          arg1=arg;
                                          $4->isEmitable=0;
                                    }

                                //    if(!$1->isEmitable&&!$4->isEmitable)
                                        $$->isEmitable=0;

                                    $$->isBoolean=1;
                                    $$->backpatch=1;
                                    $$->truelist=makelist(nextquadlabel(),$$->truelist);
                                    $$->falselist=makelist(nextquadlabel()+1,$$->falselist);

                                    emit(if_noteq,arg1,arg2,NULL,0,yylineno);
                                    emit(jump,NULL,NULL,NULL,-1,yylineno);
                                    printf("expr-->expr != expr\n");}

        | expr GREATER  expr	        {
                                    $$->isBoolean=1;
                                    $$->backpatch=1;
                                    $$->truelist=makelist(nextquadlabel(),$$->truelist);
                                    $$->falselist=makelist(nextquadlabel()+1,$$->falselist);
                                    emit(if_greater,$1,$3,NULL,0,yylineno);
                                    emit(jump,NULL,NULL,NULL,-1,yylineno);
                                    printf("expr-->expr > expr\n");}
        |  expr  LESS  expr	       {
                                    $$->isBoolean=1;
                                    $$->backpatch=1;
                                    $$->truelist=makelist(nextquadlabel(),$$->truelist);
                                    $$->falselist=makelist(nextquadlabel()+1,$$->falselist);
                                    emit(if_less,$1,$3,NULL,-1,yylineno);
                                    emit(jump,NULL,NULL,NULL,-1,yylineno);


                                    printf("expr-->expr < expr\n");}
        | expr GREATER_EQUAL expr 	{
                                    $$->isBoolean=1;
                                    $$->backpatch=1;
                                    $$->truelist=makelist(nextquadlabel(),$$->truelist);
                                    $$->falselist=makelist(nextquadlabel()+1,$$->falselist);
                                    emit(if_geatereq,$1,$3,NULL,-1,yylineno);
                                    emit(jump,NULL,NULL,NULL,-1,yylineno);
                                    printf("expr-->expr >= expr\n");}

        | expr LESS_EQUAL expr		  {
                                    $$->isBoolean=1;
                                    $$->backpatch=1;
                                    $$->truelist=makelist(nextquadlabel(),$$->truelist);
                                    $$->falselist=makelist(nextquadlabel()+1,$$->falselist);
                                    emit(if_lesseq,$1,$3,NULL,-1,yylineno);
                                    emit(jump,NULL,NULL,NULL,-1,yylineno);
                                    printf("expr-->expr <= expr\n");}

    		| term				             {
                                    $$=$1;

                                    printf("expr-->term\n");}

        ;
M       : /*empty*/ {$$=nextquadlabel();}
        ;


assignexpr  : lvalue{
              if (!arrayFlag)
                  if (isFunction(yylval.stringValue))
                    printf("ERROR: In Line %d  Can not assign any value to %s cause its a function\n",yylineno, yylval.stringValue);

              arrayFlag=0;
}
              ASSIGNMENT expr   {
                                      expr* tmp_expr=$4;
                                      if($1->type==tableitem_e){
                                          emit(tablesetelem,$1->index,$4,$1,-1,yylineno);
                                          $$=emit_iftableitem($1,yylineno);
                                          $$->type=assignexpr_e;
                                          if($4->backpatch){
                                              expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                              t->type=constbool_e;
                                              t->boolConst=1;
                                              arg->type=var_e;
                                              arg->sym=newtemp();
                                              arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                              arg->strConst=strdup(arg->sym->name);

                                              backpatch($4->truelist,nextquadlabel());
                                              emit(assign,t,NULL,arg,-1,yylineno);
                                              emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);

                                              t1->boolConst=0;
                                              t1->type=constbool_e;

                                              backpatch($4->falselist,nextquadlabel());
                                              emit(assign,t1,NULL,arg,-1,yylineno);
                                              tmp_expr=arg;
                                              assignflag=0;

                                            }
                                      }else{
                                      if($4->backpatch){
                                          expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                          t->type=constbool_e;
                                          t->boolConst=1;
                                          arg->type=var_e;
                                          arg->sym=newtemp();
                                          arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                          arg->strConst=strdup(arg->sym->name);

                                          backpatch($4->truelist,nextquadlabel());
                                          emit(assign,t,NULL,arg,-1,yylineno);
                                          emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);

                                          t1->boolConst=0;
                                          t1->type=constbool_e;

                                          backpatch($4->falselist,nextquadlabel());
                                          emit(assign,t1,NULL,arg,-1,yylineno);
                                          tmp_expr=arg;
                                          assignflag=0;

                                        }

                                          $$=$4;

                                            emit(assign,tmp_expr,NULL,$1,-1,yylineno);//na check
                                            $$=newexpr(assignexpr_e);
                                            $$->sym=newtemp();
                                            $$->strConst=malloc(sizeof(strlen($$->sym->name))+1);
                                            $$->strConst=strdup($$->sym->name);
                                            $$->type=var_e;
                                            emit(assign,$1,NULL,$$,-1,yylineno);

                                      }

                                  printf("assignexpr-->lvalue = expr\n");}
              ;


term    : LEFT_PARENTHESIS expr RIGHT_PARENTHESIS {$$=$2;$$->passedByPar=1;printf("term-->(expr)\n");}
          | MINUS expr %prec UMINUS                 {

                  if(check_arithmetic($2)){
                      $$=newexpr(arithexpr_e);
                      $$->sym=newtemp();
                      emit(uminus,$2,NULL,$$,-1,yylineno);
                      }


                  printf("term--> -expr\n");}

          | INCREMENT lvalue                        {

               if(check_arithmetic($2)){
                    if($2->type==tableitem_e){
                      $$=emit_iftableitem($2,yylineno);
                      emit(add,$$,newexpr_constint(1),$$,-1,yylineno);
                      emit(tablesetelem,$2->index,$$,$2,-1,yylineno);
                    }else{
                      emit(add,newexpr_constint(1),$2,$2,-1,yylineno);
                      $$=newexpr(arithexpr_e);
                      $$->sym=newtemp();
                      emit(assign,$2,NULL,$$,-1,yylineno);
                    }
                }

                printf("term--> ++lvalue\n");
              if (isFunction(yylval.stringValue))
                printf("ERROR: In Line %d  Can not increment  %s,its a function\n",yylineno, yylval.stringValue); }
          | lvalue INCREMENT                        {

                if(check_arithmetic($1)){
                $$=newexpr(var_e);
                $$->sym=newtemp();
                if($1->type==tableitem_e){
                    expr* val=emit_iftableitem($1,yylineno);
                    emit(assign,val,NULL,$$,-1,yylineno);
                    emit(add,val,newexpr_constint(1),val,-1,yylineno);
                    emit(tablesetelem,$1->index,val,$1,-1,yylineno);
                }else{
                    emit(assign,$1,NULL,$$,-1,yylineno);
                    emit(add,$1,newexpr_constint(1),$1,-1,yylineno);
                }
                }

                printf("term--> lvalue++\n");
          if (isFunction(yylval.stringValue))
            printf("ERROR: In Line %d  Can not increment  %s,its a function\n",yylineno, yylval.stringValue); }
          | DECREMENT lvalue                        {

                  if(check_arithmetic($2)){
                      if($2->type==tableitem_e){
                        $$=emit_iftableitem($2,yylineno);
                        emit(sub,$$,newexpr_constint(1),$$,-1,yylineno);
                        emit(tablesetelem,$2->index,$$,$2,-1,yylineno);
                      }else{
                        emit(sub,$2,newexpr_constint(1),$2,-1,yylineno);
                        $$=newexpr(arithexpr_e);
                        $$->sym=newtemp();
                        emit(assign,$2,NULL,$$,-1,yylineno);
                      }
                  }

                    printf("term--> --lvalue\n");
          if (isFunction(yylval.stringValue))
            printf("ERROR: In Line %d  Can not decrement  %s,its a function\n",yylineno, yylval.stringValue); }
          | lvalue DECREMENT                        {

                  if(check_arithmetic($1)){
                  $$=newexpr(var_e);
                  $$->sym=newtemp();
                  if($1->type==tableitem_e){
                      expr* val=emit_iftableitem($1,yylineno);
                      emit(assign,val,NULL,$$,-1,yylineno);
                      emit(sub,val,newexpr_constint(1),val,-1,yylineno);
                      emit(tablesetelem,$1->index,val,$1,-1,yylineno);
                  }else{
                      emit(assign,$1,NULL,$$,-1,yylineno);
                      emit(sub,$1,newexpr_constint(1),$1,-1,yylineno);
                  }
                  }

                    printf("term--> lvalue--\n");
                  if (isFunction(yylval.stringValue))
                    printf("ERROR: In Line %d  Can not decrement  %s,its a function\n",yylineno, yylval.stringValue); }
          | primary                                 {
                $$=$1;
              //  printf("%p\n",$1);
                printf("term--> primary\n");}
          ;

primary   : lvalue {

                    $$=emit_iftableitem($1,yylineno);

                    printf("primary--> lvalue\n");}
          | call {$$=$1;printf("primary--> call\n");}
          | objectdef {$$=$1;printf("primary--> objectdef\n");}
          | LEFT_PARENTHESIS funcdef RIGHT_PARENTHESIS {
                $$=$2;
                printf("primary--> ( funcdef )\n");}
          | const{$$=$1;printf("primary--> const\n");}
          ;

const	    : NUMBER  		{$$=newexpr_constint(yylval.intValue);printf("const--> NUMBER\n");}
          | REAL_NUMBER	{$$=newexpr_constdouble(yylval.realValue);printf("const--> REAL_NUMBER\n");}
          | STRING		  {$$=newexpr_conststring(yylval.stringValue);printf("const--> STRING\n");}
          | TRUE			  {$$=newexpr_constbool(1);printf("const--> TRUE\n");}
          | FALSE			  {$$=newexpr_constbool(0);printf("const--> FALSE\n");}
          | NIL 		  	{$$=newexpr(nil_e);printf("const--> NIL\n");}
          ;

lvalue      : ID {

                    int tempScope=scope;
                    node* tempNode=NULL;
                    while (tempScope>-1){
                    //printf("%s inside while\n",yytext);
                      if((tempNode=scopeLookup(tempScope,yytext))!=NULL) {
                        //printf("%s inside if1\n",yytext);
                          $$=lvalue_expr(tempNode);

                          IDfoundFlag=1;
                          //printf("tempScope: %d tempNode->scope :%d\n",tempScope,tempNode->scope);
                          tempScope=tempNode->scope;
                          if((tempScope!=0)&&(!isFunction(tempNode->name))){
                          //  printf("%s inside if2\n",yytext);
                              /*if a function intervenes*/
                              //printf("scope: %d %d\n",tempScope,scope-1);
                              if(intervenesFunction(tempScope,scope-1))
                                  printf("ERROR: In Line %d Can not access %s\n",yylineno,yylval.stringValue);
                          /*    else{if(tempNode->space==formalarg){
                                        if (scope == 0){
                                          tempNode=insert(yytext, GLOBAL, yylineno, scope);
                                          tempNode->offset=currscopeoffset();
                                          tempNode->space=currscopespace();
                                          $$=lvalue_expr(tempNode);
                                        }
                                        else{
                                          tempNode=insert(yytext, LOCL, yylineno, scope);
                                          tempNode->offset=currscopeoffset();
                                          tempNode->space=currscopespace();
                                          $$=lvalue_expr(tempNode);
                                        }
                                      }
                                  }*/
                          }

                            break;
                        }
                      tempScope--;
                      }
                      /*if ID wasnt found*/
                      if (!IDfoundFlag){

                        if (scope == 0){

                            tempNode=insert(yytext, GLOBAL, yylineno, scope);
                            tempNode->offset=currscopeoffset();
                            tempNode->space=currscopespace();
                            $$=lvalue_expr(tempNode);
                        }
                        else{
                            tempNode=insert(yytext, LOCL, yylineno, scope);
                            tempNode->offset=currscopeoffset();
                            tempNode->space=currscopespace();
                            $$=lvalue_expr(tempNode);
                        }

                        inccurrescopeoffset();

                      }
                      IDfoundFlag=0;
                      printf("lvalue--> ID\n");
                    }


            | LOCAL ID {printf("lvalue--> local ID\n");
                      node* tempNode=scopeLookup(scope,yytext);
                      if (tempNode==NULL&&!checkIfLibFunc(yytext)){
                          if (scope==0){
                            tempNode=insert(strdup(yytext),GLOBAL, yylineno, scope);
                            tempNode->offset=currscopeoffset();
                            tempNode->space=currscopespace();
                          }
                          else{
                              tempNode= insert(strdup(yytext),LOCL, yylineno, scope);
                              tempNode->offset=currscopeoffset();
                              tempNode->space=currscopespace();
                              }
                          inccurrescopeoffset();
                      }else {
                        if (checkIfLibFunc(yytext)){
                        printf("ERROR: In Line %d  %s Shadows a Library Function!\n",yylineno, yytext); }
                      }

                      $$=lvalue_expr(tempNode);

          					}


            | DOUBLE_COLONS ID{printf("lvalue-->:: ID\n");
              node* tempNode=scopeLookup(0,yytext);
                if(tempNode== NULL){
                  printf("ERROR: In Line %d  There is no global var with the name %s\n",yylineno, yytext);
                }else $$=lvalue_expr(tempNode);

              }
            | member {$$=$1;printf("lvalue--> member\n");}
            ;

member       : lvalue DOT ID {
                    $$=member_item($1,yylval.stringValue,yylineno);//ID.name
                    printf("member--> lvalue . ID\n");}
            | lvalue LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET {
                    $1 =emit_iftableitem($1,yylineno);
                    $$=newexpr(tableitem_e);
                    $$->sym=$1->sym;
                    $$->index=$3;
                    arrayFlag = 1;printf("member--> lvalue [ expr ] ID\n");}
            | call DOT ID {$$=$1;
                          if ($$) {
                          $$->type=tableitem_e;
                          $$->index=newexpr_conststring(yylval.stringValue);
                          }
                          printf("member--> call . ID\n");}
            | call LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET {
            $$=$1 ;
            if ($$) {
            $$->type=tableitem_e;
            $$->index=$3;
            }
            arrayFlag = 1;
            printf("member--> call [ expr ]\n");}
            ;


call        : call LEFT_PARENTHESIS elist RIGHT_PARENTHESIS {
                  $$=make_call($1,$3,yylineno);//$1
                  printf("call--> call ( elist ) \n");}
            | lvalue callsuffix {

                  if($2->method){
                      expr* self=$1;
                      $1=emit_iftableitem(member_item(self,$2->name,yylineno),yylineno);

                      expr* temp=$2->elist;

                      while(temp&&temp->next) temp=temp->next;

                      if(temp)
                       temp->next=self;

                  }

                  $$=make_call($1,$2->elist,yylineno);
                  printf("call--> lvalue callsuffix\n");}
            | LEFT_PARENTHESIS funcdef RIGHT_PARENTHESIS LEFT_PARENTHESIS elist RIGHT_PARENTHESIS {

                  expr* func =$2;
                  func->sym=$2->sym;
                  $$=make_call(func,$5,yylineno);

                  printf("call--> ( funcdef ) ( elist )\n");}
            ;

callsuffix  : normcall {
                  $$=$1;
                  printf("callsuffix--> normcall \n");}
            | methodcall {
                  $$=$1;
                  printf("callsuffix--> methodcall \n");}
            ;

normcall    :  LEFT_PARENTHESIS elist RIGHT_PARENTHESIS {
                    $$=malloc(sizeof(struct call));
                    $$->elist=$2;
                    $$->method=0;
                    $$->name=NULL;

                    printf("normcall-->( elist )\n");}
            ;

methodcall  : DOUBLE_DOT ID{
                tempName=yylval.stringValue;
                }LEFT_PARENTHESIS elist RIGHT_PARENTHESIS {
                  $$=malloc(sizeof(struct call));
                  $$->elist=$5;//$4
                  $$->method=1;
                  $$->name=tempName;
                  printf("methodcall--> .. ID( elist )\n");}
            ;

elist       : expr cexprs	{
                  if($1->backpatch){
                     expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                     t->type=constbool_e;
                     t->boolConst=1;
                     arg->type=var_e;
                     arg->sym=newtemp();
                     arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                     arg->strConst=strdup(arg->sym->name);
                     backpatch($1->truelist,nextquadlabel());

                     emit(assign,t,NULL,arg,-1,yylineno);

                     emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                     t1->boolConst=0;
                     t1->type=constbool_e;

                     backpatch($1->falselist,nextquadlabel());
                     emit(assign,t1,NULL,arg,-1,yylineno);


                     assignflag=0;

                     $1=arg;}
                $$=$1;
                $$->next=$2;
                printf("elist-->expr cexprs\n");}
		        |/*empty*/	{
                $$=NULL;
                printf("elist-->empty\n");}
		        ;

cexprs	     :COMMA expr cexprs {
                if($2->backpatch){
                     expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                     t->type=constbool_e;
                     t->boolConst=1;
                     arg->type=var_e;
                     arg->sym=newtemp();
                     arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                     arg->strConst=strdup(arg->sym->name);
                     backpatch($2->truelist,nextquadlabel());

                     emit(assign,t,NULL,arg,-1,yylineno);

                     emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                     t1->boolConst=0;
                     t1->type=constbool_e;

                     backpatch($2->falselist,nextquadlabel());
                     emit(assign,t1,NULL,arg,-1,yylineno);


                     assignflag=0;

                     $2=arg;}
                  $$=$2;
                  $$->next=$3;
                  printf("cexprs--> , expr cexprs\n");}
		         | /*empty*/	   {
                 $$=NULL;
                  printf("cexprs-->empty\n");}
		         ;


objectdef	: LEFT_SQUARE_BRACKET elist RIGHT_SQUARE_BRACKET	{

                expr* t=newexpr(newtable_e);
                t->sym =newtemp();
                emit(tablecreate,NULL,NULL,t,-1,yylineno);
                int i=0;
                while($2){
                    emit(tablesetelem,newexpr_constint(i++),$2,t,-1,yylineno);
                    $2=$2->next;
                }
                $$=t;
                printf("objectdef-->[ elist ]\n");}
			    | LEFT_SQUARE_BRACKET indexed RIGHT_SQUARE_BRACKET   {

                expr* t=newexpr(newtable_e);
                t->sym =newtemp();
                emit(tablecreate,NULL,NULL,t,-1,yylineno);
                while($2){
                    emit(tablesetelem,$2->index,$2->value,t,-1,yylineno);
                    $2=$2->next;
                }
                $$=t;

                printf("objectdef-->[ indexed ]\n");}
			    ;

indexed		: indexedelem clindexedelem	{
                $$=$1;
                $$->next=$2;
                printf("indexed-->indexedelem clindexedelem\n");}
	        ;

clindexedelem	: COMMA indexedelem clindexedelem	{
                    $$=$2;
                    $$->next=$3;
                    printf("clindexedelem-->, indexedelem clindexedelem\n");}
				      | /*empty*/		{
                $$=NULL;
                printf("clindexedelem-->empty\n");}
				      ;

indexedelem	: LEFT_CURLY_BRACKET expr COLON functor RIGHT_CURLY_BRACKET		{
                $$=$4;
                $$->value=$4;
                $$->index=$2;
                printf("indexedelem-->{expr : expr }\n");}
            ;
functor     :funcdef{}
            | expr{}

block			: LEFT_CURLY_BRACKET RIGHT_CURLY_BRACKET	{$$=malloc(sizeof(expr));printf("block-->{}\n");}
			 	  | LEFT_CURLY_BRACKET {scope++;/*enterscopespace()*/;}  stmt statements RIGHT_CURLY_BRACKET{
            //    printf("$$ %p\n",$$);
            //    printf("$3 %p\n",$3);
            //    printf("$4 %p\n",$4);
                $$=malloc(sizeof(expr));
            if($3&&$4){
          //    printf("%p %p\n",$3->breaklist,$4->breaklist);
              $$->breaklist=merge($3->breaklist,$4->breaklist);
              $$->continuelist=merge($3->continuelist,$4->continuelist);
          //    printf("d %p \n",$$);
            }else {
                    if($4)$$=$4;
                    else if($3)$$=$3;
                  }

        //    printf("$$ %p\n",$$);
        //    printf("$3 %p\n",$3);
        //    printf("$4 %p\n",$4);
            printf("block-->{ stmt }\n");

            hide(scope);
          //  exitscopespace();
            scope--;}
				  ;

funcdef : FUNCTION {
              char *newName=malloc(sizeof(char)*15);
              strcpy(newName,"_f");
              char str[12];
              int i=0;
              for(i=0;i<12;i++)
              str[i]='\0';
              sprintf(str, "%d", anonFuncCount);
              strcat(newName,str);
              anonFuncCount++;

              currentFunction=insert(newName,USERFUNC,yylineno,scope);
              currentFunction->iaddress=nextquadlabel()+1;
              currentFunction->newType=programfunc_s;
              jump_push(nextquadlabel());
              emit(jump,NULL,NULL,NULL,-1,yylineno);
              emit(funcstart,NULL,NULL,lvalue_expr(currentFunction),-1,yylineno);
              retlist * ptr=NULL;
              retPush(ptr);
              func_push(currentFunction);
              offset_push(currscopeoffset());
              enterscopespace();
              resetformalargoffset();
              //tempsymnode=currentFunction;
              //tempNodeForFunctionLocals=currentFunction;
              //printf("\n\n%d\n\n",functionLocalOffset);
              //tempNodeForFunctionLocals->localSize=-1*functionLocalOffset;//new

              } LEFT_PARENTHESIS {scope++;}
              /*scope -- giati otan anoiksei to block tha kanei ++ pali*/
              idlist{} RIGHT_PARENTHESIS {scope--;enterscopespace();resetfunctionlocalsoffset();}
              funcblockstart block {
                      exitscopespace();
                      //if (returnPatch!=-1) patchlabel(returnPatch,nextquadlabel());
                      if(topS()!=NULL){
                        retlist * tmp=retPop();
                        while(tmp!=NULL){
                          patchlabel(tmp->label,nextquadlabel());
                           tmp=tmp->next;
                        }
                      }
                      tempNodeForFunctionLocals=func_pop();
                      emit(funcend,NULL,NULL,lvalue_expr(tempNodeForFunctionLocals),-1,yylineno);
                      patchlabel(jump_pop(),nextquadlabel());
                      printf("funcdef-->function( idlist ) block \n");
                      }   funcblockend
                      {

                      exitscopespace();
                      tempNodeForFunctionLocals->localSize=functionLocalOffset;
                      printf("\n\n%s %d\n\n",tempNodeForFunctionLocals->name,functionLocalOffset);
                      restorecurrscopeoffset(offset_pop());
                      $$=newexpr(programmfunc_e);
                      $$->sym=tempNodeForFunctionLocals;

                      }


    | FUNCTION ID{
                    if(scopeLookup(scope,yytext)!=NULL){
                        printf("ERROR: at line %d, %s collides with other function or variable\n",yylineno,yytext);
                    }
                    else if(checkIfLibFunc(yytext)){
                            printf("ERROR: at line %d, %s collides with library function\n",yylineno,yytext);
                    }else if(argLookup(yytext)){
                              printf("ERROR: at line %d,cant use %s as a function name\n",yylineno,yytext);
                    }
                  currentFunction=NULL;

                  currentFunction=insert(yytext,USERFUNC,yylineno,scope);

                  currentFunction->iaddress=nextquadlabel()+1;

                  currentFunction->newType=programfunc_s;
                  retlist * ptr=NULL;
                  retPush(ptr);
                  jump_push(nextquadlabel());

                  emit(jump,NULL,NULL,NULL,-1,yylineno);
                  emit(funcstart,NULL,NULL,lvalue_expr(currentFunction),-1,yylineno);

                  func_push(currentFunction);

                  offset_push(currscopeoffset());

                  enterscopespace();
                  resetformalargoffset();
                  //tempsymnode=currentFunction;
                  //tempNodeForFunctionLocals=currentFunction;
                  //tempNodeForFunctionLocals->localSize=-1*functionLocalOffset;//new
                  } LEFT_PARENTHESIS {scope++;}
                  idlist RIGHT_PARENTHESIS {scope--;enterscopespace();resetfunctionlocalsoffset();}
                   funcblockstart block	{
                         if(topS()!=NULL){
                           retlist * tmp=retPop();
                           while(tmp!=NULL){
                             patchlabel(tmp->label,nextquadlabel());
                              tmp=tmp->next;
                           }
                         }
                          tempNodeForFunctionLocals=func_pop();
                          emit(funcend,NULL,NULL,lvalue_expr(tempNodeForFunctionLocals),-1,yylineno);
                          patchlabel(jump_pop(),nextquadlabel());
                          exitscopespace();
                          printf("funcdef-->function ID( idlist ) block \n");
                          }funcblockend
                          {
                          tempNodeForFunctionLocals->localSize=functionLocalOffset;
                          printf("\n\n%s\t%d\n\n",tempNodeForFunctionLocals->name,tempNodeForFunctionLocals->localSize);
                          exitscopespace();
                          restorecurrscopeoffset(offset_pop());
                          $$=newexpr(programmfunc_e);
                          $$->sym=tempNodeForFunctionLocals;
                          }

    ;

funcblockstart:{loop_push(loopcounter);
                loopcounter=0;
               }
               ;

funcblockend:{loopcounter=loop_pop();
             }
             ;


idlist	: ID {if(scopeLookup(scope,yytext)!=NULL){
                printf("ERROR: invalid name in line %d\n",yylineno);
             }
             else if(checkIfLibFunc(yytext))
                printf("ERROR: collision with library function in line %d\n",yylineno);
             if(currentFunction!=NULL){ insertArguments(currentFunction,yytext)->offset=currscopeoffset();inccurrescopeoffset();}
             else {
                    insert(yytext,FORMAL,yylineno,scope);

                  }
             }
              cidlist	{printf("idlist--> ID cidlist\n");$$=malloc(sizeof(expr));}
		    | /*empty*/	{$$=NULL;currentFunction=NULL;printf("idlist-->empty\n");}
		    ;


cidlist	: COMMA ID{if(scopeLookup(scope,yytext)!=NULL){
                printf("ERROR: invalid name in line %d\n",yylineno);
                currentFunction=NULL;
             }
             else if(checkIfLibFunc(yytext))
                    printf("ERROR: collision with library function in line %d\n",yylineno);
             args *tmp=currentFunction->FuncArgs;
             while(tmp!=NULL){
               if(!strcmp(tmp->name,yytext))
                  printf("ERROR: argument redefinition in line %d\n",yylineno);
               tmp=tmp->next;
             }
             if(currentFunction!=NULL){ insertArguments(currentFunction,yytext)->offset=currscopeoffset();inccurrescopeoffset();}
             else insert(yytext,FORMAL,yylineno,scope);

            }
              cidlist {printf("cidlist-->, ID cidlist\n");$$=malloc(sizeof(expr));}
		    | /*empty*/	{$$=NULL;currentFunction=NULL;printf("cidlist-->empty\n");}
		    ;



ifprefix	: IF LEFT_PARENTHESIS expr{
                                    if($3->backpatch){
                                      expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                      t->type=constbool_e;
                                      t->boolConst=1;
                                      arg->type=var_e;
                                      arg->sym=newtemp();
                                      arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                      arg->strConst=strdup(arg->sym->name);
                                      backpatch($3->truelist,nextquadlabel());

                                      emit(assign,t,NULL,arg,-1,yylineno);

                                      emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                                      t1->boolConst=0;
                                      t1->type=constbool_e;

                                      backpatch($3->falselist,nextquadlabel());
                                      emit(assign,t1,NULL,arg,-1,yylineno);


                                      assignflag=0;

                                      $3=arg;
                                    }
                                        } RIGHT_PARENTHESIS{
        emit(if_eq,$3,newexpr_constbool(1),NULL,nextquadlabel()+2,yylineno);
        $$=nextquadlabel();
        emit(jump,NULL,NULL,NULL,0,yylineno);
        printf("ifstmt-->if(expr)stmt\n");
        }
	    ;

ifstmt : ifprefix stmt{patchlabel($1,nextquadlabel());$$=$2;/*it will break dikio exeis*/}
     | ifprefix stmt elseprefix stmt{
                                      if($2&&$4){
                                        $$=malloc(sizeof(expr));
                                        $2->breaklist=merge($2->breaklist,$4->breaklist);
                                        $2->continuelist=merge($2->continuelist,$4->continuelist);
                                        $$=$2;
                                      }
                                      else{
                                      if($2){$$=$2;}
                                      else if($4){$$=$4;}}
                                      patchlabel($1,$3+1);patchlabel($3,nextquadlabel());printf("ifstmt-->if(expr)stmt else stmt\n");}
     ;

elseprefix : ELSE{$$=nextquadlabel();
                emit(jump,NULL,NULL,NULL,0,yylineno);
                }
        ;



whilestmt	:whilestart whilecond loopstmt{

              emit(jump,NULL,NULL,NULL,$1,yylineno);
              patchlabel($2,nextquadlabel());
 //printf("%p\n",$3);
              if($3){
            //    printf("%p\n",$3);
                backpatch($3->breaklist,nextquadlabel());
              //  printf("a\n");
                backpatch($3->continuelist,$1);
              //  printf("a\n");
            }

          $$=$3;
              printf("whilestmt-->while(expr)stmt\n");}
          ;

whilestart :WHILE{
              $$=nextquadlabel();
              }
           ;

whilecond :LEFT_PARENTHESIS expr{if($2->backpatch){
                     expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                     t->type=constbool_e;
                     t->boolConst=1;
                     arg->type=var_e;
                     arg->sym=newtemp();
                     arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                     arg->strConst=strdup(arg->sym->name);
                     backpatch($2->truelist,nextquadlabel());

                     emit(assign,t,NULL,arg,-1,yylineno);

                     emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                     t1->boolConst=0;
                     t1->type=constbool_e;

                     backpatch($2->falselist,nextquadlabel());
                     emit(assign,t1,NULL,arg,-1,yylineno);


                     assignflag=0;

                     $2=arg;}} RIGHT_PARENTHESIS{

            emit(if_eq,newexpr_constbool(1),$2,NULL,nextquadlabel()+2,yylineno);
            $$=nextquadlabel();
            emit(jump,NULL,NULL,NULL,0,yylineno);

            }

            ;

loopstmt :loopstart stmt loopend{/*printf("l %p\n",$2);*/$$=$2;}
         ;

loopstart : {++loopcounter;}
          ;

loopend:{--loopcounter;}
        ;

forprefix: FOR LEFT_PARENTHESIS elist SEMICOLON M expr{if($6->backpatch){
                                  expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                                  t->type=constbool_e;
                                  t->boolConst=1;
                                  arg->type=var_e;
                                  arg->sym=newtemp();
                                  arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                                  arg->strConst=strdup(arg->sym->name);
                                  backpatch($6->truelist,nextquadlabel());

                                  emit(assign,t,NULL,arg,-1,yylineno);

                                  emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                                  t1->boolConst=0;
                                  t1->type=constbool_e;

                                  backpatch($6->falselist,nextquadlabel());
                                  emit(assign,t1,NULL,arg,-1,yylineno);

                                  $6=arg;
                                  assignflag=0;


                              }
                            //  printf("here\n");
                        } SEMICOLON {
                                      $$=malloc(sizeof(struct forStruct));
                                      $$->test=$5;
                                      $$->enter=nextquadlabel();




                                      emit(if_eq,newexpr_constbool(1),$6,NULL,0,yylineno);

                                    }
;


forstmt	: forprefix N elist RIGHT_PARENTHESIS N loopstmt N	{
                                        patchlabel($1->enter,$5+1);
                                      //  printf("A\n");
                                        patchlabel($2,nextquadlabel());
                                      //  printf("A\n");
                                        patchlabel($5,$1->test);
                                      //  printf("%p\n",$6);
                                        patchlabel($7,$2+1);
                                        if($6){
                                          backpatch($6->breaklist,nextquadlabel());
                                          backpatch($6->continuelist,$2+1);
                                        }
                                        printf("forstmt-->for(elist;expr;elist;) stmt\n");}
		    ;


N : {$$=nextquadlabel();emit(jump,NULL,NULL,NULL,0,yylineno);}
  ;


  returnstmt	: RETURN{if (scope==0)printf("Error use of return in scope 0 in line %d\n",yylineno);}
   expr {if($3->backpatch){
                        expr* t=newexpr(boolexpr_e),*t1=newexpr(boolexpr_e),*arg=newexpr(var_e);
                        t->type=constbool_e;
                        t->boolConst=1;
                        arg->type=var_e;
                        arg->sym=newtemp();
                        arg->strConst=malloc(sizeof(strlen(arg->sym->name))+1);
                        arg->strConst=strdup(arg->sym->name);
                        backpatch($3->truelist,nextquadlabel());

                        emit(assign,t,NULL,arg,-1,yylineno);

                        emit(jump,NULL,NULL,NULL,nextquadlabel()+2,yylineno);
                        t1->boolConst=0;
                        t1->type=constbool_e;

                        backpatch($3->falselist,nextquadlabel());
                        emit(assign,t1,NULL,arg,-1,yylineno);


                        assignflag=0;

                        $3=arg;}} SEMICOLON	{
                        if(currscopespace()==functionlocal){
                                                emit(ret,$3,NULL,NULL,-1,yylineno);
                                                insertRet(nextquadlabel());
                                                emit(jump,NULL,NULL,NULL,-1,yylineno);
                                             }
                                                printf("returnstmt-->return expr;\n");$$=malloc(sizeof(expr));}
  			      | RETURN{if (scope==0)
                        printf("Error use of return in scope 0 in line %d\n",yylineno);
                      else{
                            if(currscopespace()==functionlocal){
                              emit(ret,NULL,NULL,NULL,-1,yylineno);
                              //returnPatch=nextquadlabel();
                              insertRet(nextquadlabel());
                              emit(jump,NULL,NULL,NULL,-1,yylineno);

                              }
                        }

                        } SEMICOLON {$$=malloc(sizeof(expr));
                        printf("returnstmt-->return;\n");}
  			      ;


%%

int yyerror (char* yaccProvidedMessage)
{
	printf("%s: at line %d, before token: '%s'\n", yaccProvidedMessage, yylineno, yytext);
}

int main(int argc, char** argv){
initialize();
if (argc == 3){
  if( !(yyin = fopen(argv[1], "r")) ) {
    printf("Cannot Open File: %s\n", argv[1]);
    yyin = stdin;
  }
  if(!(yyout = fopen(argv[2], "w")) )
  {
    printf("Cannot Open File: %s\n", argv[2]);
    yyout = stdout;
  }
}
else if (argc == 2){
  if( !(yyin = fopen(argv[1], "r")) ) {
    printf("Cannot Open File: %s\n", argv[1]);
    yyin = stdin;
  }
}
else{
  printf("NO ARGUMENTS\n");
  return 0;
}

yyparse();

printf("SYMTABLE:\n");
printTable();

printf("\nSCOPES:\n");
printScopes();

PrintQuads();

generateAll();
print_instructions();

//filePrintQuads();
Write_Binary_Code();
avm_read_binary();
return 0;
}
