#include "quad.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
int totalInstr=0;
int currInstr=0;
int currProcessedQuad=0;
#define EXPAND_SIZE_I 1024
#define CURR_SIZE_I (totalInstr*sizeof(instruction))
#define NEW_SIZE_I (EXPAND_SIZE*sizeof(instruction)+CURR_SIZE_I)


typedef enum vmarg_t{
	label_a=0,
  global_a=1,
  formal_a=2,
	local_a=3,
  number_a=4,
  string_a=5,
  bool_a=6,
  nil_a=7,
	userfunc_a=8,
  libfunc_a=9,
  retval_a=10,
} vmarg_t;

typedef enum vmopcode{
	assign_v,	add_v,  	sub_v,
	mul_v, 		div_v, 		mod_v,
	uminus_v, 	and_v, 		or_v,
	not_v, 		jeq_v, 		jne_v,
	jgt_v, 		jlt_v, 		jge_v,
	jle_v, 		jump_v, 	call_v,
	funcenter_v, 	funcexit_v, 	newtable_v,
	tablegetelem_v,	tablesetelem_v,	pusharg_v,
	nop_v

} vmopcode;

typedef struct vmarg{
  vmarg_t type;
  int val;
}vmarg;

typedef struct instruction{
  vmopcode opcode;
  vmarg    *result;
  vmarg    *arg1;
  vmarg    *arg2;
  int srcLine;

}instruction;



typedef struct userFunc{
  int address;
  int localSize;
  char*	   id;
  int formalSize;
}userFunc;


typedef struct incomplete_function_locals{
	 int instrNo;
	 node* sym;
	struct incomplete_function_locals *next;
} incomplete_function_local;

typedef void (*generator_func_t) (quad*);

int nextInstructionLabel(){
  return currInstr;
}

vmarg* make_operand(expr* );
vmarg* make_booloperand(vmarg* ,  int);
vmarg* make_retvaloperand(vmarg* );
vmarg* make_numberoperand(vmarg *, double);

void generateAll(void);
void generate(vmopcode , quad *);
void generate_relational(vmopcode op, quad* q);


void generate_ADD(quad* q);
void generate_SUB(quad* q);
void generate_MUL(quad* q);
void generate_DIV(quad* q);
void generate_MOD(quad* q);
void generate_NEWTABLE(quad* q);
void generate_TABLEGETELEM(quad* q);
void generate_TABLESETELEM(quad* q);
void generate_ASSIGN(quad* q);
void generate_NOP(quad* q);
void generate_JUMP(quad* q);
void generate_UMINUS(quad *q);
void generate_IF_EQ(quad* q);
void generate_IF_NOTEQ(quad* q);
void generate_IF_GREATER(quad* q);
void generate_IF_GREATEREQ(quad* q);
void generate_IF_LESS(quad* q);
void generate_IF_LESSEQ(quad* q);
void generate_AND(quad* q);
void generate_NOT(quad* q);
void generate_OR(quad* q);
void generate_PARAM(quad* q);
void generate_CALL(quad* q);
void generate_GETRETVAL(quad* q);
void generate_FUNCSTART(quad* q);
void generate_RETURN(quad* q);
void generate_FUNCEND(quad* q);

generator_func_t generators[] = {

	generate_ASSIGN,
	generate_ADD,
	generate_SUB,
	generate_MUL,
	generate_DIV,
	generate_MOD,
	generate_UMINUS,
	generate_AND,
	generate_OR,
	generate_NOT,
	generate_IF_EQ,
	generate_IF_NOTEQ,
	generate_IF_LESSEQ,
	generate_IF_GREATEREQ,
	generate_IF_LESS,
	generate_IF_GREATER,
	generate_JUMP,
	generate_CALL,
	generate_PARAM,
	generate_RETURN,
	generate_GETRETVAL,
	generate_FUNCSTART,
	generate_FUNCEND,
	generate_NEWTABLE,
	generate_TABLEGETELEM,
	generate_TABLESETELEM,
	generate_NOP
};


void emitInstruction(instruction* t);
void expand_instruction();
void add_incomplete_function_local(int instrNo,node* sym);
void patch_incomplete_function_locals();

int consts_newstring(char *);
int consts_number(double );
int libfuncs_newfunc(const char *);
int userfuncs_newfunc(node *);


void print_Args(vmarg *arg);
void print_instructions();

void Write_Binary_Code();


 incomplete_function_local *ifl_head =(incomplete_function_local*) 0;
 int ifl_total = 0;

 instruction *instructions = NULL;

   char ** stringConsts;
  int  totalStringConsts=0;


  double* numConsts;
  int  totalNumConsts=0;

  char **libFuncs;
  int  totalLibFuncs=0;

  userFunc * userFuncs;
  int  totalUserFuncs=0;



int  consts_newstring(char *val){
    int  i;
    for(i=0;i<totalStringConsts;i++)
      if(!strcmp(stringConsts[i],val)) return i;

      /*an den to vrei*/
    if (totalStringConsts==0)
    stringConsts = malloc (sizeof(char*) );
    else
    stringConsts = realloc(stringConsts, ((totalStringConsts)+1)*sizeof(char*) );


    stringConsts[totalStringConsts] = strdup(val);

    return totalStringConsts++;
}


int consts_number(double val){
		printf("\nconsts_number %d\n",val);

  int  i;
  for(i=0;i<totalNumConsts;i++)
    if(numConsts[i]==val) return i;

    /*an den to vrei*/
    if (totalNumConsts==0)
    numConsts = malloc (sizeof(double));
    else
    numConsts = realloc(numConsts, ((totalNumConsts)+1)*sizeof(double) );


    numConsts[totalNumConsts] = val;
    return totalNumConsts++;
}

int libfuncs_newfunc(const char *id){
		printf("\nlibfuncs_newfunc\n\n\n\n\n");
	int i;
	for (i=0;i<totalLibFuncs; i++){
		if (!strcmp(libFuncs[i],id) )
			return i;
	}
    /*an den to vrei*/
    if (totalLibFuncs==0)
    libFuncs =  malloc (sizeof(char *) );
    else
    libFuncs = realloc(libFuncs, ((totalLibFuncs)+1)*sizeof(char*) );


    libFuncs[totalLibFuncs] = strdup(id);
    return totalLibFuncs++;
}

int userfuncs_newfunc(node *sym){
	int i;

	for (i=0;i<totalUserFuncs; i++){
		if (!strcmp(userFuncs[i].id,sym->name)&&userFuncs[i].address==sym->iaddress)//to be checked mallon thelei scope kai oxi address
			return i;
	}

    if (totalUserFuncs==0)
		userFuncs = malloc (sizeof(userFunc));
    else
		userFuncs = realloc(userFuncs, ((totalUserFuncs)+1)*sizeof(userFunc)) ;

		add_incomplete_function_local(nextInstructionLabel(),sym);
    userFuncs[totalUserFuncs].id = strdup(sym->name);
	args* tmp=sym->FuncArgs,*temp=NULL;
	while(tmp){
		temp=tmp;
		tmp=tmp->next;
	}
	if(temp){
		userFuncs[totalUserFuncs].formalSize=temp->offset+1;

	}else
		userFuncs[totalUserFuncs].formalSize=0;

	userFuncs[totalUserFuncs].address=sym->iaddress;
    return totalUserFuncs++;
}

vmarg * make_operand(expr* e){
	if(e == NULL)	return NULL;
	vmarg* arg = malloc(sizeof(vmarg));
	switch(e->type)	{
    /*All below use a variable for storage*/
		case var_e:
		case arithexpr_e:
		case boolexpr_e:
		case newtable_e:
    case assignexpr_e:
		case tableitem_e:
		{
        assert(e->sym);
        arg->val=e->sym->offset;
        switch(e->sym->space){
            case programvar:    arg->type=global_a;  break;
            case functionlocal: arg->type=local_a;   break;
            case formalarg:     arg->type=formal_a;  break;
            default:assert(0);
        }
			break;
		}
    /*Constants*/
		case constbool_e:
		{
			arg->val = e->boolConst;
			arg->type = bool_a;
			break;
		}
		case conststring_e:
		{
			arg->val = consts_newstring(e->strConst);
			arg->type = string_a;
			break;
		}
		case constdouble_e:
		{
			arg->val = consts_number(e->doubleConst);
			arg->type = number_a;
			break;
		}
		case constint_e:
		{
			arg->val = consts_number(e->intConst);
			arg->type = number_a;
			break;
		}
		case nil_e :
		{
			arg->type = nil_a;
			break;
		}

		case programmfunc_e:
		{
			arg->type = userfunc_a;
            arg->val=userfuncs_newfunc(e->sym);


			break;
		}
		case libraryfunc_e:
		{
			arg->type = libfunc_a;
			arg->val = libfuncs_newfunc(e->sym->name);
			break;
		}
		default:
			assert(0);
	}
	return arg;
}

void emitInstruction(instruction* t){
  assert(t);
  instruction *instr;
  if(currInstr == totalInstr)
    expand_instruction();


  instr = instructions + currInstr++;
  instr->opcode = t->opcode;
  instr->arg1 = t->arg1;
  instr->arg2 = t->arg2;
  instr->result = t->result;
  instr->srcLine = t->srcLine;

}

void generate_relational(vmopcode op, quad *q){
  instruction *t = (instruction *)malloc(sizeof(instruction));
   t->opcode = op;
   t->arg1 = make_operand(q->arg1);
   t->arg2 = make_operand(q->arg2);
   t->result=(vmarg*)malloc(sizeof(vmarg));
   t->result->type = label_a;

	 t->result->val=q->label;

  emitInstruction(t);
}

vmarg* make_booloperand(vmarg* arg,int val){
	arg = malloc(sizeof(vmarg));
  arg->val = val;
  arg->type = bool_a;
  return arg;
}

vmarg* make_retvaloperand(vmarg* arg){
	arg = malloc(sizeof(vmarg));
  arg->type = retval_a;
  return arg;
}

vmarg* make_numberoperand(vmarg* arg, double val){
	arg = malloc(sizeof(vmarg));
  arg->val = consts_number(val);
  arg->type = number_a;
  return arg;
}

void generateAll(void){
	for(currProcessedQuad=0;currProcessedQuad<nextquadlabel();currProcessedQuad++){
		(*generators[quads[currProcessedQuad].op]) (quads+currProcessedQuad);
	}
patch_incomplete_function_locals();
}


void add_incomplete_function_local (int instrNo, node* sym){
    ifl_total++;
    incomplete_function_local *newnode =malloc(sizeof(incomplete_function_local));
		newnode->sym=sym;
    newnode->instrNo = instrNo;
    newnode->next = NULL;
		if (ifl_head==NULL) ifl_head = newnode;
		else {
			newnode->next=ifl_head;
			ifl_head=newnode;
		}

}

void patch_incomplete_function_locals(){
  int i=totalUserFuncs-1;
  incomplete_function_local* temp=ifl_head;
  while(temp){
		  //printf("\n\n%s  %d\n\n",temp->sym->name,temp->sym->localSize);
			userFuncs[i--].localSize = temp->sym->localSize;//arithmos twn locals
    	temp=temp->next;
  }
}


void generate(vmopcode op, quad *q){
  instruction *t;

  t = malloc(sizeof(instruction));
  assert(t);
  t->opcode= op;
  t->arg1 = make_operand(q->arg1);
  t->arg2 = make_operand(q->arg2);
  t->result = make_operand(q->result);

  //q->taddress = nextInstructionLabel();

  emitInstruction(t);

}

void generate_ADD(quad* q){
  generate(add_v, q);
}
void generate_SUB(quad* q){
  generate(sub_v, q);
}
void generate_MUL(quad* q){
  generate(mul_v, q);
}
void generate_DIV(quad* q){
  generate(div_v, q);
}
void generate_MOD(quad* q){
  generate(mod_v, q);
}
void generate_NEWTABLE(quad* q){
  generate(newtable_v, q);
}
void generate_TABLEGETELEM(quad* q){
  generate(tablegetelem_v, q);
}
void generate_TABLESETELEM(quad* q){
  generate(tablesetelem_v, q);
}
void generate_ASSIGN(quad* q){
  generate(assign_v, q);
}
void generate_NOP(quad* q){

}
void generate_JUMP(quad* q){
  generate_relational(jump_v, q);
}
void generate_IF_EQ(quad* q){
  generate_relational(jeq_v, q);
}
void generate_UMINUS(quad *q){
	q->arg2=newexpr_constint(-1);
  generate(mul_v, q);
}
void generate_IF_NOTEQ(quad* q){
  generate_relational(jne_v, q);
}
void generate_IF_GREATER(quad* q){
  generate_relational(jgt_v, q);
}
void generate_IF_GREATEREQ(quad* q){
  generate_relational(jge_v, q);
}
void generate_IF_LESS(quad* q){
  generate_relational(jlt_v, q);
}
void generate_IF_LESSEQ(quad* q){
  generate_relational(jle_v, q);
}


void generate_AND(quad* q){

}
void generate_NOT(quad* q){

}
void generate_OR(quad* q){

}

void generate_PARAM(quad* q){
	//printf("\ngenerate_PARAM\n");
  instruction *t = malloc(sizeof(instruction));
  t->opcode = pusharg_v;
  t->arg1 = make_operand(q->arg1);
  t->arg2 = NULL;
  t->result = NULL;
  emitInstruction(t);
}

void generate_CALL(quad* q){
//printf("\ngenerate_CALL\n");
  instruction *t = malloc(sizeof(instruction));
  assert(t);

  t->opcode = call_v;
  t->arg1 = make_operand(q->arg1);
  t->arg2 = NULL;
  t->result = NULL;
  emitInstruction(t);

}
void generate_GETRETVAL(quad* q){
	//printf("\ngenerate_GETRETVAL\n");
  instruction *t = malloc(sizeof(instruction));
  assert(t);

  t->opcode = assign_v;
  t->result = make_operand(q->result);
  t->arg1 = make_retvaloperand(t->arg1);
  t->arg2 = NULL;

  emitInstruction(t);

}

void generate_FUNCSTART(quad* q){
	//printf("\ngenerate_FUNCSTART\n");
  instruction *t = malloc(sizeof(instruction));
  t->opcode = funcenter_v;
  t->result = make_operand(q->result);
  t->result->val = userfuncs_newfunc(q->result->sym);
  t->arg2 = NULL;
  t->arg1 = NULL;
  emitInstruction(t);

}

void generate_RETURN(quad* q){
//	printf("\ngenerate_RETURN\n");
  instruction *t = malloc(sizeof(instruction));


  //assign value to retval
  t->opcode = assign_v;
  t->result =  malloc (sizeof(vmarg) );
  t->result = make_retvaloperand(t->result);
  t->arg1 = make_operand(q->arg1);
  t->arg2 = NULL;
  emitInstruction(t);

}

void generate_FUNCEND(quad* q){
//	printf("\ngenerate_FUNCEND\n");

  instruction *t = (instruction *)malloc(sizeof(instruction));

  //funcexit
  t->opcode = funcexit_v;
  t->result = make_operand(q->result);
  t->result->val = userfuncs_newfunc(q->result->sym);
  t->arg2 = NULL;
  t->arg1 = NULL;
  emitInstruction(t);


  return;
}


void expand_instruction(){
	instruction *newInstr=malloc(NEW_SIZE_I);

	memcpy(newInstr,instructions,CURR_SIZE_I);
	free(instructions);

	instructions= newInstr;
	totalInstr=totalInstr+EXPAND_SIZE_I;
}


void print_instructions(){
	int j=0;
	instruction *tmp = instructions;

	printf("\n\nLine \tOpCode \t\tResult \t\tArg1 \t\tArg2\n\n");
	while(j < currInstr){

		printf("%d:\t",j+1);
		tmp= instructions+j++;
		switch(tmp->opcode)
		{
			case assign_v:
				printf("%s\t\t", "ASSIGN");
				break;
			case add_v:
				printf("%s\t\t", "ADD");
				break;
			case sub_v:
				printf("%s\t\t", "SUB");
				break;
			case mul_v:
				printf("%s\t\t", "MUL");
				break;
			case div_v:
				printf("%s\t\t", "DIV");
				break;
			case mod_v:
				printf("%s\t\t", "MOD");
				break;
			case and_v:
				printf("%s\t\t", "AND");
				break;
			case or_v:
				printf("%s\t\t", "OR");
				break;
			case not_v:
				printf("%s\t\t", "NOT");
				break;
			case jeq_v:
				printf("%s\t\t", "JEQ");
				break;
			case jne_v:
				printf("%s\t\t", "JNE");
				break;
			case jgt_v:
				printf("%s\t\t", "JQT");
				break;
			case jlt_v:
				printf("%s\t\t", "JLT");
				break;
			case jge_v:
				printf("%s\t\t", "JGE");
				break;
			case jle_v:
				printf("%s\t\t", "JLE");
				break;
			case call_v:
				printf("%s\t\t", "CALL");
				break;
			case funcenter_v:
				printf("%s\t", "FUNCENTER");
				break;
			case funcexit_v:
				printf("%s\t", "FUNCEXIT");
				break;
			case newtable_v:
				printf("%s\t", "NEWTABLE");
				break;
			case tablegetelem_v:
				printf("%s\t", "TABLEGETLEM");
				break;
			case tablesetelem_v:
				printf("%s\t", "TABLESETLEM");
				break;
			case pusharg_v:
				printf("%s\t\t", "PUSHARG");
				break;
			case nop_v:
				printf("%s\t\t", "NOP");
				break;
			case jump_v:
				printf("%s\t\t", "JUMP");
				break;
			default:
				break;
		}

		if(tmp->result)
		      print_Args(tmp->result);
		else	printf("\t\t");
		if(tmp->arg1)
		      print_Args(tmp->arg1);
		else	printf("\t\t");
		if(tmp->arg2)
		      print_Args(tmp->arg2);
		else	printf("\t\t");
		printf("\n");
	}
	printf("\n");
}


void print_Args(vmarg *arg)
{
  switch(arg->type)
			{
				case label_a:
					printf("0%d(label)%d\t",arg->type ,arg->val+1);
					break;
				case global_a:
					printf("0%d(global)%d\t",arg->type, arg->val);
					break;
				case formal_a:
					printf("0%d(formal)%d\t",arg->type, arg->val);
					break;
				case local_a:
					printf("0%d(local)%d\t",arg->type, arg->val);
					break;
				case number_a:
					printf("0%d(number)%d:%.1lf\t",arg->type, arg->val,numConsts[arg->val]);
					break;
				case string_a:
					printf("0%d(string)%d:%s\t",arg->type, arg->val,stringConsts[arg->val] );
					break;
				case bool_a:
					printf("0%d(boolean)%d:",arg->type, arg->val);
					if(arg->val)
					  printf("true\t");
					else
					  printf("false;\t");
					break;
				case nil_a:
					printf("0%d(nil)%d:nil\t",arg->type,arg->val);
					break;
				case userfunc_a:
					printf("0%d(userfunc)%d\t", arg->type, arg->val);
					break;
				case libfunc_a:
					printf("0%d(libfunc)%d:%s\t",arg->type, arg->val, libFuncs[arg->val]);
					break;
				case retval_a:
					printf("%d(retval)%d\t",arg->type, arg->val);
					break;
				default:
					break;
			}

}

void Write_Binary_Code(){

	FILE* bin = NULL;
	int MagicNum = 400060;
	int tmpCurrInst=0, i;
	instruction * p;

	bin = fopen("BinaryCode.abc", "wb");

	fwrite(&MagicNum, sizeof(int), 1, bin);


	fwrite(&totalStringConsts,sizeof(int),1,bin);

	for(i=0; i<totalStringConsts; i++)
		fwrite(&stringConsts[i],sizeof(char *),1,bin);

	fwrite(&totalNumConsts,sizeof(int),1,bin);

	for(i=0; i<totalNumConsts; i++)
		fwrite(&numConsts[i],sizeof(double),1,bin);



	fwrite(&totalLibFuncs,sizeof(int),1,bin);

	for(i=0; i<totalLibFuncs; i++)
		fwrite(&libFuncs[i],sizeof(char *),1,bin);


	fwrite(&totalUserFuncs,sizeof(int),1,bin);

	for(i=0; i<totalUserFuncs; i++){

		fwrite(&userFuncs[i].id,sizeof(char *),1,bin);
		fwrite(&userFuncs[i].address,sizeof(int),1,bin);
		fwrite(&userFuncs[i].localSize,sizeof(int),1,bin);
		fwrite(&userFuncs[i].formalSize,sizeof(int),1,bin);
	}

	fwrite(&currInstr, sizeof(unsigned int), 1, bin);
	p = instructions;
	while(tmpCurrInst < currInstr){


		fwrite(&(p->opcode),sizeof(int),1,bin);

		if (p->result)
		{
			fwrite(&(p->result->type),sizeof(int),1,bin);

			fwrite(&(p->result->val),sizeof(int),1,bin);
		}

		//arg1

		if(p->arg1)
		{
			fwrite(&(p->arg1->type),sizeof(int),1,bin);

			fwrite(&(p->arg1->val),sizeof(int),1,bin);
		}

		//arg2

		if(p->arg2)
		{
			fwrite(&(p->arg2->type),sizeof(int),1,bin);

			fwrite(&(p->arg2->val),sizeof(int),1,bin);
		}

		p+=1;
		tmpCurrInst++;
	}

	if(bin)	fclose(bin);
}
