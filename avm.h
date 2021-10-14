#include "final_code.h"
#include<ctype.h>
#include<math.h>


#define AVM_STACKENV_SIZE 4
#define AVM_NUMACTUALS_OFFSET 4
#define AVM_SAVEDPC_OFFSET 3
#define AVM_SAVEDTOP_OFFSET 2
#define AVM_SAVEDTOPSP_OFFSET 1
#define AVM_TABLE_HASHSIZE 211
#define AVM_STACKSIZE 4096


#define execute_add execute_arithmetic
#define execute_sub execute_arithmetic
#define execute_mul execute_arithmetic
#define execute_div execute_arithmetic
#define execute_mod execute_arithmetic
#define AVM_WIPEOUT(m) memset(&(m),0,sizeof(m))

typedef void (*execute_func_t)(instruction*);


void avm_initialize ();
void execute_cycle();



enum avm_memcell_t{
    number_m=0,
    string_m=1,
    bool_m=2,
    table_m=3,
    userfunc_m=4,
    libfunc_m=5,
    nil_m=6,
    undef_m=7
};

typedef struct avm_memcell{
    enum avm_memcell_t type;
    union
    {
        double numVal;
        char * strVal;
        char boolVal;
        struct avm_table* tableVal;
        int funcVal;
        char * libfuncVal;
    }data;

}avm_memcell;
avm_memcell* currentFunc;
typedef struct avm_table_bucket{
    avm_memcell* key;
    avm_memcell* value;
    struct avm_table_bucket* next;
}avm_table_bucket;

typedef struct avm_table{
    int refCounter;
    avm_table_bucket* strIndexed[AVM_TABLE_HASHSIZE];
    avm_table_bucket* numIndexed[AVM_TABLE_HASHSIZE];
    avm_table_bucket* boolIndexed[2];
    avm_table_bucket* tableIndexed[AVM_TABLE_HASHSIZE];
    avm_table_bucket* funcIndexed[AVM_TABLE_HASHSIZE];
    avm_table_bucket* libfuncIndexed[AVM_TABLE_HASHSIZE];
    int total;
}avm_table;

avm_table*    avm_tablenew();
void          avm_tabledestroy(avm_table* t);
avm_memcell*  avm_tablegetelem(avm_table* t,avm_memcell* key);
void          avm_tablesetelem(avm_table* t,avm_memcell* key,avm_memcell* value);
void          avm_memcellclear(avm_memcell* m);

void avm_error(char* format,...);
char* avm_tostring(avm_memcell*);
void avm_calllibfunc(char* funcName);
void avm_callsaveenvironment ();


void avm_tableincrefcounter (avm_table* t){
    ++t->refCounter;
}

void avm_tabledecrefcounter (avm_table* t){
    //printf("\ndecrease ref :%d--\n",t->refCounter);

    assert(t->refCounter>0);
    if(!--t->refCounter) avm_tabledestroy(t);
}

void avm_tablebucketsinit (avm_table_bucket** p,int hashsize){
  int i=0;
  for(i=0;i<hashsize;i++)
    p[i]=NULL;
}

avm_table* avm_tablenew(){
    avm_table* t=malloc(sizeof(avm_table));
    AVM_WIPEOUT(*t);
    t->refCounter=t->total=0;
    avm_tablebucketsinit(t->numIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketsinit(t->strIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketsinit(t->tableIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketsinit(t->libfuncIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketsinit(t->funcIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketsinit(t->boolIndexed,2);
    return t;
}
void avm_tablebucketdestroy(avm_table_bucket** p,int hashsize){
    int i=0;
    for(i=0;i<hashsize;i++){
      avm_table_bucket* b;
      for( b=*p;b;){
        avm_table_bucket* del=b;
        b=b->next;
        avm_memcellclear(del->key);
        avm_memcellclear(del->value);
        //free(del);
    }
    p[i]=NULL;
  }
}

void avm_tabledestroy (avm_table*t){
    avm_tablebucketdestroy(t->strIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketdestroy(t->numIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketdestroy(t->tableIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketdestroy(t->libfuncIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketdestroy(t->funcIndexed,AVM_TABLE_HASHSIZE);
    avm_tablebucketdestroy(t->boolIndexed,2);
    //free(t);
}
/*same hash for libfuncs,tables,userfuncs*/
int string_hash_function(char* str) {
    int i = 0,j=0;
    for (j=0; str[j]; j++)
        i += str[j];
    return i % AVM_TABLE_HASHSIZE;
}

int num_hash_function(double num) {
    return (int)num % AVM_TABLE_HASHSIZE;
}


avm_table_bucket* hash_lookup(avm_table* t,avm_memcell* key){
    assert(key);
    if(key->type==number_m){
      int index=num_hash_function(key->data.numVal);
        assert(t);
        avm_table_bucket* temp=t->numIndexed[index];
        while(temp){
        if(temp->key)
        if(temp->key->data.numVal==key->data.numVal)
            return temp;
        temp=temp->next;
      }
    }else if(key->type==string_m){
      int index=string_hash_function(key->data.strVal);
      avm_table_bucket* temp=t->strIndexed[index];
      while(temp){
        if(temp->key)
        if(!strcmp(temp->key->data.strVal,key->data.strVal))
            return temp;
         temp=temp->next;
        }
    }else if(key->type==table_m){
      int index=num_hash_function((int)key->data.tableVal);
      avm_table_bucket* temp=t->tableIndexed[index];
      while(temp){
        if(temp->key)
        if(temp->key->data.tableVal==key->data.tableVal)
            return temp;
         temp=temp->next;
        }
    }else if(key->type==userfunc_m){
      //char* funcName=readUserFuncs[key->data.funcVal]->id;
      int index=num_hash_function(key->data.funcVal);
      avm_table_bucket* temp=t->funcIndexed[index];
      while(temp){
        if(temp->key)//readUserFuncs[temp->key->data.funcVal]->id,readUserFuncs[key->data.funcVal]->id)
        if(temp->key->data.funcVal==key->data.funcVal)
            return temp;
         temp=temp->next;
        }
    }else if(key->type==libfunc_m){
      int index=string_hash_function(key->data.libfuncVal);
      avm_table_bucket* temp=t->libfuncIndexed[index];
      while(temp){
        if(temp->key)
        if(!strcmp(temp->key->data.libfuncVal,key->data.libfuncVal))
            return temp;
         temp=temp->next;
        }
    }else if(key->type==bool_m){
      avm_table_bucket* temp=t->boolIndexed[key->data.boolVal];
      while(temp){
        if(temp->key)
        if(temp->key->data.boolVal==key->data.boolVal)
            return temp;
         temp=temp->next;
        }
    }else{
      avm_error("table key type is undef or nil\n");
    }

    return NULL;
}


avm_memcell*  avm_tablegetelem(avm_table* t,avm_memcell* key){

    avm_table_bucket* temp=hash_lookup(t,key);
    if (temp) return temp->value;
    else return NULL;

}

void   avm_tablesetelem(avm_table* t,avm_memcell* key,avm_memcell* value){
    avm_table_bucket* temp=hash_lookup( t,key);
    int index;

    if(t==value->data.tableVal)
      avm_error(" table table self referencing is forbidden(will result in heap overflow in print)\n");
    //removal
    if(value->type==nil_m) {

      if(!temp){
        if(key)//ean vazeis stoixeio pou den uparxei to diagrafeis automata
        t->total++; //giati tha meiwthei meta
        //else avm_error("Removing a non existing table element is forbidden\n");
      }else if(t->total<=0){
        avm_error("Trying to delete something from an empty table\n");
      }else{
        int index;
        if(key->type==number_m){
          index=num_hash_function(key->data.numVal);
          if(temp==t->numIndexed[index]){//ean einai to prwto
            t->numIndexed[index]=t->numIndexed[index]->next;
          }else{
            avm_table_bucket* past_temp=t->numIndexed[index];
            while(past_temp&&past_temp->next){
              if(past_temp->next==temp){
                past_temp->next=past_temp->next->next;
                break;
              }
              past_temp=past_temp->next;
            }
          }
        }else if(key->type==string_m){
          index=string_hash_function(key->data.strVal);
          if(temp==t->strIndexed[index]){//ean einai to prwto
            t->strIndexed[index]=t->strIndexed[index]->next;
          }else{
            avm_table_bucket* past_temp=t->strIndexed[index];
            while(past_temp&&past_temp->next){
              if(past_temp->next==temp){
                past_temp->next=past_temp->next->next;
                break;
              }
              past_temp=past_temp->next;
            }
          }
            }else if(key->type==table_m){//string
              index=num_hash_function((int)key->data.tableVal);
              if(temp==t->tableIndexed[index]){//ean einai to prwto
                t->tableIndexed[index]=t->tableIndexed[index]->next;
              }else{
                avm_table_bucket* past_temp=t->tableIndexed[index];
                while(past_temp&&past_temp->next){
                  if(past_temp->next==temp){
                    past_temp->next=past_temp->next->next;
                    break;
                  }
                  past_temp=past_temp->next;
                }
              }
          }else if(key->type==userfunc_m){//string
            index=num_hash_function(key->data.funcVal);
            if(temp==t->funcIndexed[index]){//ean einai to prwto
              t->funcIndexed[index]=t->funcIndexed[index]->next;
            }else{
              avm_table_bucket* past_temp=t->funcIndexed[index];
              while(past_temp&&past_temp->next){
                if(past_temp->next==temp){
                  past_temp->next=past_temp->next->next;
                  break;
                }
                past_temp=past_temp->next;
              }
            }
        }else if(key->type==libfunc_m){//string
          index=string_hash_function(key->data.libfuncVal);
          if(temp==t->libfuncIndexed[index]){//ean einai to prwto
            t->libfuncIndexed[index]=t->libfuncIndexed[index]->next;
          }else{
            avm_table_bucket* past_temp=t->libfuncIndexed[index];
            while(past_temp&&past_temp->next){
              if(past_temp->next==temp){
                past_temp->next=past_temp->next->next;
                break;
              }
              past_temp=past_temp->next;
            }
          }
      }else if(key->type==bool_m){
        if(temp==t->boolIndexed[key->data.boolVal]){//ean einai to prwto
          t->boolIndexed[key->data.boolVal]=t->boolIndexed[key->data.boolVal]->next;
        }else{
          avm_table_bucket* past_temp=t->boolIndexed[key->data.boolVal];
          while(past_temp&&past_temp->next){
            if(past_temp->next==temp){
              past_temp->next=past_temp->next->next;
              break;
            }
            past_temp=past_temp->next;
          }
        }
    }

    }
    t->total--;
  }else{
    avm_table_bucket* new_bucket=malloc(sizeof(struct avm_table_bucket));
    new_bucket->key=malloc(sizeof(avm_memcell));
    new_bucket->value=malloc(sizeof(avm_memcell));
    new_bucket->next=NULL;
    new_bucket->value->type=value->type;

    if (value->type==table_m){
      avm_tableincrefcounter(value->data.tableVal);
      new_bucket->value->data.tableVal=value->data.tableVal;
    }else if(value->type==string_m){
      new_bucket->value->data.strVal=strdup(value->data.strVal);
    }else if(value->type==number_m){
      new_bucket->value->data.numVal=value->data.numVal;
    }else if(value->type==libfunc_m){
      new_bucket->value->data.libfuncVal=strdup(value->data.libfuncVal);
    }else if(value->type==userfunc_m){
      new_bucket->value->data.funcVal=value->data.funcVal;
    }else if(value->type==bool_m){
      new_bucket->value->data.boolVal=value->data.boolVal;
    }
    avm_table_bucket* replace;
    if(key->type==number_m){
      new_bucket->key->type=number_m;
      new_bucket->key->data.numVal=key->data.numVal;
      index=num_hash_function(key->data.numVal);
      replace=t->numIndexed[index] ;
      if(temp==NULL){//ean den vrhke hdh uparxwn item me to key
        if(t->numIndexed[index]==NULL)
            t->numIndexed[index]=new_bucket;//ean einai adeio to bucket
        else {

          while(replace->next){
            replace=replace->next;
        }
        replace->next=new_bucket;
      }
    } else {

      new_bucket->next=temp->next;

      while(replace!=temp&&replace->next!=temp)
            replace=replace->next;
        if(replace==temp) t->numIndexed[index]=new_bucket;
        else replace->next=new_bucket ;
      }

    }else if (key->type==string_m) {//string
      new_bucket->key->type=string_m;
      new_bucket->key->data.strVal=strdup(avm_tostring(key));
      index=string_hash_function(key->data.strVal);
      replace=t->strIndexed[index] ;
      if(temp==NULL){//ean den vrhke hdh uparxwn item me to key
        if(t->strIndexed[index]==NULL){
            t->strIndexed[index]=new_bucket;//ean einai adeio to bucket
        }else {

          while(replace->next){
            replace=replace->next;
        }
        replace->next=new_bucket;
      }
    } else {
      new_bucket->next=temp->next;

      while(replace!=temp&&replace->next!=temp)
            replace=replace->next;
        if(replace==temp) t->strIndexed[index]=new_bucket;
        else replace->next=new_bucket ;
      }


  }else if(key->type==table_m) {
      new_bucket->key->type=table_m;
      new_bucket->key->data.tableVal=key->data.tableVal;
      avm_tableincrefcounter(key->data.tableVal);
      index=num_hash_function((int)key->data.tableVal);
      replace=t->tableIndexed[index] ;
      if(temp==NULL){//ean den vrhke hdh uparxwn item me to key
        if(t->tableIndexed[index]==NULL){
            t->tableIndexed[index]=new_bucket;//ean einai adeio to bucket
        }else {

          while(replace->next){
            replace=replace->next;
        }
        replace->next=new_bucket;
      }
    } else {
      new_bucket->next=temp->next;

      while(replace!=temp&&replace->next!=temp)
            replace=replace->next;
        if(replace==temp) t->tableIndexed[index]=new_bucket;
        else replace->next=new_bucket ;
      }


    }else if(key->type==userfunc_m) {
      new_bucket->key->type=userfunc_m;
      new_bucket->key->data.funcVal=key->data.funcVal;
      index=num_hash_function(key->data.funcVal);
      replace=t->funcIndexed[index] ;
      if(temp==NULL){//ean den vrhke hdh uparxwn item me to key
        if(t->funcIndexed[index]==NULL){
            t->funcIndexed[index]=new_bucket;//ean einai adeio to bucket
        }else {

          while(replace->next){
            replace=replace->next;
        }
        replace->next=new_bucket;
      }
    } else {
      new_bucket->next=temp->next;

      while(replace!=temp&&replace->next!=temp)
            replace=replace->next;
        if(replace==temp) t->funcIndexed[index]=new_bucket;
        else replace->next=new_bucket ;
      }


    }else if(key->type==libfunc_m) {
      new_bucket->key->type=libfunc_m;
      new_bucket->key->data.libfuncVal=strdup(key->data.libfuncVal);
      index=string_hash_function(key->data.libfuncVal);
      replace=t->libfuncIndexed[index] ;
      if(temp==NULL){//ean den vrhke hdh uparxwn item me to key
        if(t->libfuncIndexed[index]==NULL){
            t->libfuncIndexed[index]=new_bucket;//ean einai adeio to bucket
        }else {

          while(replace->next){
            replace=replace->next;
        }
        replace->next=new_bucket;
      }
    } else {
      new_bucket->next=temp->next;

      while(replace!=temp&&replace->next!=temp)
            replace=replace->next;
        if(replace==temp) t->libfuncIndexed[index]=new_bucket;
        else replace->next=new_bucket ;
      }


    }else if(key->type==bool_m) {
      new_bucket->key->type=bool_m;
      new_bucket->key->data.boolVal=key->data.boolVal;
      replace=t->boolIndexed[key->data.boolVal] ;
      if(temp==NULL){//ean den vrhke hdh uparxwn item me to key
        if(t->boolIndexed[key->data.boolVal]==NULL){
            t->boolIndexed[key->data.boolVal]=new_bucket;//ean einai adeio to bucket
        }else {
          while(replace->next){
            replace=replace->next;
        }
        replace->next=new_bucket;
      }
    } else {
      new_bucket->next=temp->next;

      while(replace!=temp&&replace->next!=temp)
            replace=replace->next;
        if(replace==temp) t->boolIndexed[key->data.boolVal]=new_bucket;
        else replace->next=new_bucket ;
      }


    }
  t->total++;
}

}

struct avm_memcell stack[AVM_STACKSIZE];
static void avm_initstack(){
    int i=0;
    for(i=0;i<AVM_STACKSIZE;++i){
        AVM_WIPEOUT(stack[i]);
        stack[i].type=undef_m;
    }
}

struct avm_memcell ax,bx,cx,retval;
int top,topsp;


#define AVM_MAX_INSTRUCTIONS (unsigned) nop_v

double consts_getnumber(int index);
char* consts_getstring(int index);
char* libfuncs_getused(int index);


void avm_warning(char *format, ...);


void execute_assign(instruction*);
void execute_add(instruction*);
void execute_sub(instruction*);
void execute_mul(instruction*);
void execute_div(instruction*);
void execute_mod(instruction*);
void execute_jeq(instruction*);
void execute_jne(instruction*);
void execute_jle(instruction*);
void execute_jge(instruction*);
void execute_jlt(instruction*);
void execute_jgt(instruction*);
void execute_jump(instruction *);
void execute_call(instruction*);
void execute_pusharg(instruction*);
void execute_funcenter(instruction*);
void execute_funcexit(instruction*);
void execute_newtable(instruction*);
void execute_tablegetelem(instruction*);
void execute_tablesetelem(instruction*);
void execute_nop(instruction*);

typedef void (*execute_func_t)(instruction *);

execute_func_t executeFuncs[] = {

    execute_assign,
    execute_add,
    execute_sub,
    execute_mul,
    execute_div,
    execute_mod,
    0,  // uminus
    0,  // and
    0,  // or
    0,  // not
    execute_jeq,
    execute_jne,
    execute_jgt,
    execute_jlt,
    execute_jge,
    execute_jle,
    execute_jump,
    execute_call,
    execute_funcenter,
    execute_funcexit,
    execute_newtable,
    execute_tablegetelem,
    execute_tablesetelem,
    execute_pusharg

};

int executionFinished=0;
int pc=0;
int currLine=0,codeSize=0;
unsigned int 	glbnum = 0;
instruction * code =NULL;
#define AVM_ENDING_PC  codeSize

instruction* readInstructions = NULL;
int totalInstructionNum;
///TABLES declaration
  char ** readStringConsts;
 int  readTotalStringConsts;


 double* readNumConsts;
 int  readTotalNumConsts;

 char **readLibFuncs;
 int  readTotalLibFuncs;

 userFunc * readUserFuncs;
 int  readTotalUserFuncs;

void avm_read_binary() {
  // Read the binary and start running
  FILE *fp = NULL;
  int magNum;
  int i, tmp;
  instruction *newCodeItem;

  fp = fopen("BinaryCode.abc", "rb");
  if (!fp) printf("Error could not open source file \n");

  fread(&magNum, sizeof(int), 1, fp);

  printf("magic number is: %d\n",magNum);
  if (magNum != 400060) {
    printf("Wrong magic Number\n");
    exit(-1);
  }

  fread(&readTotalStringConsts, sizeof(int), 1, fp);
  readStringConsts = malloc(readTotalStringConsts*sizeof(char *));
  for (i = 0; i < readTotalStringConsts; i++)
    fread(&readStringConsts[i], sizeof(char *), 1, fp) ;

  fread(&readTotalNumConsts, sizeof(int), 1, fp);
  readNumConsts = malloc(readTotalNumConsts * sizeof(double));
  for (i = 0; i < readTotalNumConsts; i++)
    fread(&readNumConsts[i], sizeof(double), 1, fp);


  fread(&readTotalLibFuncs, sizeof(int), 1, fp);
  readLibFuncs =malloc(readTotalLibFuncs *sizeof(char *));
  for (i = 0; i < readTotalLibFuncs; i++)
    fread(&readLibFuncs[i], sizeof(char *), 1, fp);


  fread(&readTotalUserFuncs, sizeof(int), 1, fp);
  readUserFuncs = malloc(readTotalUserFuncs * sizeof(userFunc));
  for (i = 0; i < readTotalUserFuncs; i++) {
    fread(&readUserFuncs[i].id, sizeof(char *), 1, fp);
    fread(&readUserFuncs[i].address, sizeof(int), 1, fp);
    fread(&readUserFuncs[i].localSize, sizeof(int), 1, fp);
    fread(&readUserFuncs[i].formalSize, sizeof(int), 1, fp);
  }

  fread(&totalInstructionNum, sizeof(int), 1, fp);
  if (!totalInstructionNum) {
    printf("No instructions!\n");
    exit(1);
  }

  readInstructions =malloc(totalInstructionNum * sizeof(instruction));
  for (i = 0; i < totalInstructionNum; i++) {
    newCodeItem = readInstructions + i;
    fread(&(newCodeItem->opcode), sizeof(int), 1, fp);

    switch (newCodeItem->opcode) {
      case assign_v: {
        newCodeItem->result =malloc(sizeof(vmarg));
        fread(&(newCodeItem->result->type), sizeof(int), 1, fp);
        fread(&(newCodeItem->result->val), sizeof(int), 1, fp);

        newCodeItem->arg1 = malloc(sizeof(vmarg));
        fread(&(newCodeItem->arg1->type), sizeof(int), 1, fp);
        fread(&(newCodeItem->arg1->val), sizeof(int), 1, fp);
        break;
      }
      case add_v:
      case sub_v:
      case mul_v:
      case div_v:
      case mod_v:
      case jeq_v:
      case jne_v:
      case jgt_v:
      case jge_v:
      case jlt_v:
      case jle_v:
      case tablegetelem_v:
      case tablesetelem_v: {
        newCodeItem->result =malloc(sizeof(vmarg));
        fread(&(newCodeItem->result->type), sizeof(int), 1, fp);
        fread(&(newCodeItem->result->val), sizeof(int), 1, fp);

        newCodeItem->arg1 = malloc(sizeof(vmarg));
        fread(&(newCodeItem->arg1->type), sizeof(int), 1, fp);
        fread(&(newCodeItem->arg1->val), sizeof(int), 1, fp);

        newCodeItem->arg2 = malloc(sizeof(vmarg));
        fread(&(newCodeItem->arg2->type), sizeof(int), 1, fp);
        fread(&(newCodeItem->arg2->val), sizeof(int), 1, fp);
        break;
      }
      case funcenter_v:
      case funcexit_v:
      case jump_v:
      case newtable_v: {
        newCodeItem->result =malloc(sizeof(vmarg));
        fread(&(newCodeItem->result->type), sizeof(int), 1, fp);
        fread(&(newCodeItem->result->val), sizeof(int), 1, fp);
        break;
      }
      case call_v:
      case pusharg_v: {
        newCodeItem->arg1 =malloc(sizeof(vmarg));
        fread(&(newCodeItem->arg1->type), sizeof(int), 1, fp);
        fread(&(newCodeItem->arg1->val), sizeof(int), 1, fp);
        break;
      }
    }
  }

  codeSize = currInstr;


  avm_initialize();
  int ind=-1;
  glbnum = AVM_STACKSIZE - getTotalGlobals() - 1;
  printf("Glbnum is :%d  \n", glbnum);
  topsp = glbnum-1;
  top=topsp;

  printf("Top    is : %d \nTopsp  is :  %d\n", top, topsp);
  while (executionFinished == 0) execute_cycle();

  fclose(fp);
}


void execute_cycle() {
  unsigned int oldPc;
  if (executionFinished)
    return;
  else
      if (pc == AVM_ENDING_PC) {
      //printf("%d\n",pc);
      printf("\nExecution of Virtual Machine completed.\n");
      executionFinished = 1;
      return;
    } else {
      assert(pc < AVM_ENDING_PC);
      instruction *instr = instructions + pc;
     //  printf("AVM_ENDING_PC %d",AVM_ENDING_PC);

      assert(instr->opcode >= 0 && instr->opcode <= AVM_MAX_INSTRUCTIONS);

      if (instr->srcLine) currLine = instr->srcLine;

      oldPc = pc;

      //printf("%d\n",totalUserFuncs);
      (*executeFuncs[instr->opcode])(instr);

      if (pc == oldPc) pc++;

  }

}


double consts_getnumber(int index){
  return readNumConsts[index];
}
char* consts_getstring(int index){
  return readStringConsts[index];
}
char* libfuncs_getused(int index){
  return readLibFuncs[index];
}

avm_memcell* avm_translate_operand( vmarg* arg,avm_memcell* reg){
    //printf("type in translate operand is %d\n",arg->type);


    switch(arg->type){
        case global_a: return &stack[AVM_STACKSIZE-1-arg->val];
        case local_a: return &stack[topsp-arg->val];
        case formal_a:  return &stack[topsp+AVM_STACKENV_SIZE+1+arg->val];
        case retval_a:  return &retval;
        case number_a:
        {
            reg->type=number_m;
            reg->data.numVal=consts_getnumber(arg->val);
            return reg;
        }

        case string_a:{

            reg->type=string_m;

            reg->data.strVal=consts_getstring(arg->val);
            //printf("%s\n", reg->data.strVal);
            return reg;
        }

        case bool_a:{

            reg->type=bool_m;

            reg->data.boolVal=arg->val;

            return reg;
        }
        case nil_a: reg->type=nil_m; return reg;

        case userfunc_a:{
            reg->type=userfunc_m;
            reg->data.funcVal=arg->val;
            return reg;
        }
        case libfunc_a:{
            reg->type=libfunc_m;
            reg->data.libfuncVal=libfuncs_getused(arg->val);
            return reg;
        }
        default: assert(0);
    }
}



typedef void (*memclear_func_t)(avm_memcell*);

void memclear_string(avm_memcell* m){
    assert(m->data.strVal);
    //free(m->data.strVal);
}
void memclear_table(avm_memcell *m){
    assert(m->data.tableVal);
    //printf("\n--SEIRA 669\n");
    avm_tabledecrefcounter(m->data.tableVal);
}

memclear_func_t memclearFuncs[]={
  0,//memclear_number,
  memclear_string,
  0,//  memclear_bool,
  memclear_table,
  0,//  memclear_userfunc,
  0,//  memclear_libfunc,
  0,//  memclear_nil,
  0,//  memclear_undef
};

void avm_memcellclear (avm_memcell* m){
    if(m->type!=undef_m){
        memclear_func_t f=memclearFuncs[m->type];
        if(f)
            (*f)(m);
        m->type=undef_m;
    }
}


void avm_assign(avm_memcell* lv,avm_memcell* rv){
  if(lv==rv)
      return;
  if(lv->type==table_m&&rv->type==table_m&&lv->data.tableVal==rv->data.tableVal)
      return;

  if(rv->type==undef_m)
      avm_warning("assigning from 'undef' content!");

  avm_memcellclear(lv);

  memcpy(lv,rv,sizeof(avm_memcell));
 // printf("assign value is %f\n",rv->data.numVal);
  if(lv->type==string_m){
      //lv->data.strVal=malloc(strlen(strdup(rv->data.strVal))+1);
      lv->data.strVal=strdup(rv->data.strVal);
  }else if(lv->type==table_m){
          //printf("\n++SEIRA 709\n");
         avm_tableincrefcounter(lv->data.tableVal);
         }

}
void execute_assign(instruction * instr){
  avm_memcell* lv=avm_translate_operand((instr->result),NULL);
  avm_memcell* rv= avm_translate_operand((instr->arg1),&ax);


  assert(lv);
  assert(&stack[top] <lv ||lv == &retval);
  assert(&stack[AVM_STACKSIZE-1] >=lv||lv == &retval);
  assert(rv);

  avm_assign(lv,rv);

}


void avm_warning(char *format, ...)
{
  printf("\n Warning : %s \n", format);
  return;
}


void avm_error(char *format, ...)
{
  printf("\nRuntime Error: %s ",format);
  executionFinished=1;
}


int totalActuals=0;

void avm_dec_top(){
    if(!top){
        avm_error("stack overflow\n");
    }
    else
        --top;
}

void avm_push_envvalue(int val){
    stack[top].type=number_m;
    stack[top].data.numVal=val;
    avm_dec_top();
}

void avm_callsaveenvironment(){
    avm_push_envvalue(totalActuals);
    avm_push_envvalue(pc+1);
    avm_push_envvalue(top+totalActuals+2);
    avm_push_envvalue(topsp);
}

void execute_jump(instruction *instr)
{
   // printf("jump is %d\n",instr->result->val);
    if(!executionFinished && instr->result )
	    pc = instr->result->val;
}

int getFunctorVal(avm_table *arr){
  int i=0,index=string_hash_function("()");
  avm_table_bucket* tmp=arr->strIndexed[index];
  while(tmp){
    if(!strcmp(tmp->key->data.strVal,"()"))
        return tmp->value->data.funcVal;
    tmp=tmp->next;
  }
  return -1;
}
int getFuncFromArray(avm_table *arr,char* name){

}


void execute_call(instruction* instr){

  avm_memcell* func = avm_translate_operand(instr->arg1,&ax);
  assert(func&&(&stack[AVM_STACKSIZE-1]>=func&&func>&stack[top]||func==&ax ));
  if(func->type==table_m&&getFunctorVal(func->data.tableVal)!=-1){
    avm_assign(&stack[top],func);
   ++totalActuals;
   avm_dec_top();
  }

  avm_callsaveenvironment();

  switch(func->type){
      case userfunc_m:{
       // printf("%s\n",readUserFuncs[func->data.funcVal].id);
          pc=readUserFuncs[func->data.funcVal].address;
          if(readUserFuncs[func->data.funcVal].formalSize>totalActuals)
            avm_error("too few arguments for function");
          assert(pc<AVM_ENDING_PC);
          assert((instructions+pc)->opcode==funcenter_v);
          break;
      }
      case string_m:avm_calllibfunc(func->data.strVal);break;
      case libfunc_m:avm_calllibfunc(func->data.libfuncVal); break;
      case table_m:   if(getFunctorVal(func->data.tableVal)!=-1){ pc=readUserFuncs[getFunctorVal(func->data.tableVal)].address; break;}

      default:{
          char* s=avm_tostring(func);
          printf("call: cannot bind %s to function!",s);
          executionFinished=1;
          //free(s);
      }

  }

}


//userfunc* avm_getfuncinfo(int address);


void execute_funcenter(instruction * instr){
  //printf("%p\n",instr->result);
  avm_memcell* func=avm_translate_operand(instr->result,&ax);
  assert(func&&(&stack[AVM_STACKSIZE-1]>=func&&func>&stack[top]||func==&ax ));
  //assert(pc == readUserFuncs[func->data.funcVal].address);
  //printf("%d %d\n",pc,readUserFuncs[func->data.funcVal].address);
  assert(pc==readUserFuncs[func->data.funcVal].address);

  totalActuals=0;
  //userFunc funcInfo =avm_getfuncinfo(pc);
  topsp=top;
 // printf("retrun %d\n",funcInfo->localSize);
  top=top-readUserFuncs[func->data.funcVal].localSize;

}


int avm_get_envvalue(int i){
    assert(stack[i].type==number_m);//stis dialekseis exei =
    int val=stack[i].data.numVal;
    assert(stack[i].data.numVal==val);
    return val;
}

void execute_funcexit(instruction* unused){

  int oldTop=top;
  top=avm_get_envvalue(topsp+AVM_SAVEDTOP_OFFSET);
  pc = avm_get_envvalue(topsp+AVM_SAVEDPC_OFFSET);
  topsp= avm_get_envvalue(topsp + AVM_SAVEDTOPSP_OFFSET);
  //printf("%d %d\n",oldTop,top);
  while(oldTop++<=top){
    avm_memcellclear(&stack[oldTop]);
  }
}

void execute_pusharg(instruction* instr){
  avm_memcell* arg=avm_translate_operand(instr->arg1,&ax);
  assert(arg&&(&stack[AVM_STACKSIZE-1]>=arg&&arg>&stack[top]||arg==&ax ));
  //printf("aa %d\n",arg->type);
 // printf("aa%s\n",avm_tostring(arg));
  avm_assign(&stack[top],arg);
  ++totalActuals;
  avm_dec_top();
}

typedef char* (*tostring_func_t)(avm_memcell*);


 char* number_tostring(avm_memcell* cell){

      assert(cell &&  cell->type == number_m);
      int length = snprintf(NULL,0,"%0.3f",cell->data.numVal);
      char* str=malloc(length+1);
      snprintf(str,length+1,"%0.3f",cell->data.numVal);
      /*
      char* new=malloc(sizeof(double)*sizeof(char)+1);
      sprintf(new, "%0.3f", cell->data.numVal);
      */
      return strdup(str);
}

char* string_tostring(avm_memcell* cell){
      assert(cell &&  cell->type == string_m);
      char* target = strdup(cell->data.strVal);
      return target;
}

char* bool_tostring(avm_memcell* cell){
    assert(cell &&  cell->type == bool_m);
    char* alithia=strdup("true");
    char* psema=strdup("false");
    if(cell->data.boolVal) return alithia;
    else return psema;
}


char* table_tostring(avm_memcell* cell){
    assert(cell &&  cell->type == table_m);

    char* tsitsis=strdup("");
    avm_table* temp=cell->data.tableVal;
    avm_table_bucket* temp_hash;

  //  printf("\nTable's ref counter:%d\nTable's total counter:%d\n",temp->refCounter,temp->total);

    int i=0;
    tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("[")+1);
    strcat(tsitsis,strdup("["));
    for(i=0;i<AVM_TABLE_HASHSIZE;i++){
      assert(temp);

      temp_hash=temp->strIndexed[i];

      while(temp_hash){
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("{")+1);

        strcat(tsitsis,strdup("{"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->key))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->key)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(":")+1);
        strcat(tsitsis,strdup(":"));

        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->value))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->value)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("} ")+1);
        strcat(tsitsis,strdup("} "));

        temp_hash=temp_hash->next;

      }
    }

    for(i=0;i<AVM_TABLE_HASHSIZE;i++){
      assert(temp);
      temp_hash=temp->numIndexed[i];

      while(temp_hash){
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("{")+1);
        strcat(tsitsis,strdup("{"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->key))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->key)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(":")+1);
        strcat(tsitsis,strdup(":"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->value))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->value)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("} ")+1);
        strcat(tsitsis,strdup("} "));
        temp_hash=temp_hash->next;
      }

    }
    for(i=0;i<AVM_TABLE_HASHSIZE;i++){
      assert(temp);
      temp_hash=temp->funcIndexed[i];

      while(temp_hash){
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("{")+1);
        strcat(tsitsis,strdup("{"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->key))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->key)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(":")+1);
        strcat(tsitsis,strdup(":"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->value))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->value)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("} ")+1);
        strcat(tsitsis,strdup("} "));
        temp_hash=temp_hash->next;
      }

    }
    for(i=0;i<AVM_TABLE_HASHSIZE;i++){
      assert(temp);
      temp_hash=temp->libfuncIndexed[i];

      while(temp_hash){
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("{")+1);
        strcat(tsitsis,strdup("{"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->key))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->key)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(":")+1);
        strcat(tsitsis,strdup(":"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->value))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->value)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("} ")+1);
        strcat(tsitsis,strdup("} "));
        temp_hash=temp_hash->next;
      }

    }
    for(i=0;i<AVM_TABLE_HASHSIZE;i++){
      assert(temp);
      temp_hash=temp->tableIndexed[i];

      while(temp_hash){
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("{")+1);
        strcat(tsitsis,strdup("{"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->key))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->key)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(":")+1);
        strcat(tsitsis,strdup(":"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->value))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->value)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("} ")+1);
        strcat(tsitsis,strdup("} "));
        temp_hash=temp_hash->next;
      }

    }
    for(i=0;i<2;i++){
      assert(temp);
      temp_hash=temp->boolIndexed[i];

      while(temp_hash){
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("{")+1);
        strcat(tsitsis,strdup("{"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->key))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->key)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(":")+1);
        strcat(tsitsis,strdup(":"));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen(avm_tostring(temp_hash->value))+1);
        strcat(tsitsis,strdup(avm_tostring(temp_hash->value)));
        tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("} ")+1);
        strcat(tsitsis,strdup("} "));
        temp_hash=temp_hash->next;
      }

    }
    tsitsis=realloc(tsitsis,strlen(tsitsis)+sizeof(char)*strlen("] ")+1);
    strcat(tsitsis,strdup("] "));
  char* new=strdup(tsitsis);

  return new;
}

char* userfunc_tostring(avm_memcell* cell){
    assert(cell &&  cell->type == userfunc_m);
    char* new=strdup(readUserFuncs[cell->data.funcVal].id);
    return strdup(new);
}

char* libfunc_tostring(avm_memcell* cell){
    assert(cell &&  cell->type == libfunc_m);
    char* new=strdup(cell->data.libfuncVal);
    return new;
}

char* nil_tostring(avm_memcell* cell){
    assert(cell &&  cell->type == nil_m);
    char* new=strdup("nil");
    return new;
}

char* undef_tostring(avm_memcell* cell){
    assert(cell &&  cell->type == undef_m);
    char* new=strdup("undef");
    return new;
}

tostring_func_t tostringFuncs[]={
    number_tostring,
    string_tostring,
    bool_tostring,
    table_tostring,
    userfunc_tostring,
    libfunc_tostring,
    nil_tostring,
    undef_tostring
};


char* avm_tostring(avm_memcell* cell){
    assert(cell->type>=0 && cell->type<=7);
   return (*tostringFuncs[cell->type])(cell);
}

typedef double(*arithmetic_func_t)(double x,double y);

double add_impl (double x,double y){return x+y;}
double sub_impl (double x,double y){return x-y;}
double mul_impl (double x,double y){return x*y;}
double div_impl (double x,double y){if (!(int)y) {
      avm_error("Cant divide with 0\n");
}else
    return x/y;
}
double mod_impl (double x,double y){if (!(int)y) {
      avm_error("Cant divide with 0\n");
}else
    return ((unsigned)x)%((unsigned)y);
}

arithmetic_func_t arithmenticFuncs[]={
    add_impl,
    sub_impl,
    mul_impl,
    div_impl,
    mod_impl
};

void execute_arithmetic(instruction* instr){
    avm_memcell* lv=avm_translate_operand(instr->result,NULL);
    avm_memcell* rv1=avm_translate_operand(instr->arg1,&ax);
    avm_memcell* rv2=avm_translate_operand(instr->arg2,&bx);

    assert(lv && (&stack[top] <= lv && &stack[AVM_STACKSIZE] > lv || lv == &retval));
    assert(rv1&&(&stack[AVM_STACKSIZE-1]>=rv1 && rv1>&stack[top]||rv1==&retval||rv1==&ax));
    assert(rv2&&(&stack[AVM_STACKSIZE-1]>=rv2 && rv2>&stack[top]||rv2==&retval||rv2==&bx));
    if(rv1->type!=number_m || rv2->type!=number_m){
        avm_error("not a number in arithmetic!\n");
  }else{
        arithmetic_func_t op=arithmenticFuncs[instr->opcode-add_v];
        avm_memcellclear(lv);
        lv->type =number_m;
        lv->data.numVal=(*op)(rv1->data.numVal,rv2->data.numVal);
      }
}

char* typeStrings[]={
  "number",
  "string",
  "bool",
  "table",
  "userfunc",
  "libfunc",
  "nil",
  "undef"
};

void execute_newtable (instruction* instr){
    avm_memcell* lv=avm_translate_operand(instr->result,NULL);
    assert(lv&&(&stack[AVM_STACKSIZE-1]>=lv&&lv>&stack[top]|| lv==&retval));

    avm_memcellclear(lv);

    lv->type=table_m;
    lv->data.tableVal=avm_tablenew();
    //printf("\n++SEIRA 1063\n");
    avm_tableincrefcounter(lv->data.tableVal);
}

void execute_tablegetelem (instruction * instr){

  avm_memcell* lv=avm_translate_operand(instr->result,NULL);
  avm_memcell* t=avm_translate_operand(instr->arg1,NULL);
  avm_memcell* i=avm_translate_operand(instr->arg2,&ax);
  assert(lv&&(&stack[AVM_STACKSIZE-1]>=lv&&lv>&stack[top] || lv==&retval));
  assert(t&&(&stack[AVM_STACKSIZE-1]>=t&&t>&stack[top] ));
  assert(i&&(&stack[AVM_STACKSIZE-1]>=i&&i>&stack[top] || i==&ax));

  avm_memcellclear(lv);
  lv->type=nil_m;



  if(t->type !=table_m){
      printf("illegal use of type %s as table!!\n",typeStrings[t->type]);
      executionFinished=1;
  }else{

     avm_memcell*content =avm_tablegetelem(t->data.tableVal,i);

     if(content&&content->type==userfunc_m)
        currentFunc=content;

     if(content)
          avm_assign(lv,content);
     else{//optional
       //char* ts=avm_tostring(t);
       //char* is=avm_tostring(i);
       //printf("\n");
    // avm_warning("%s[%s] not found!",ts,is);
       //free(ts);
       //free(is);
     }

   }
}

void execute_tablesetelem(instruction * instr) {
  avm_memcell* t=avm_translate_operand(instr->result,NULL);
  avm_memcell* i=avm_translate_operand(instr->arg1,&ax);
  avm_memcell* c=avm_translate_operand(instr->arg2,&bx);

  assert(t&&(&stack[AVM_STACKSIZE-1]>=t&&t>&stack[top] ));
  assert(i&&(&stack[AVM_STACKSIZE-1]>=i&&i>&stack[top]||i==&ax ));
  assert(c&&(&stack[AVM_STACKSIZE-1]>=c&&c>&stack[top]||c==&bx ));



  if(t->type !=table_m){
      printf("illegal use of type %s as table!\n",typeStrings[t->type]);
      executionFinished=1;
}else
     avm_tablesetelem(t->data.tableVal,i,c);
}
typedef unsigned char (*tobool_func_t)(avm_memcell*);

unsigned char number_tobool (avm_memcell* m){return m->data.numVal!=0;}
unsigned char string_tobool (avm_memcell* m){return m->data.strVal[0]!='\0';}
unsigned char bool_tobool (avm_memcell* m){return m->data.boolVal;}
unsigned char table_tobool (avm_memcell* m){return 1;}
unsigned char userfunc_tobool (avm_memcell* m){return 1;}
unsigned char libfunc_tobool (avm_memcell* m){return 1;}
unsigned char nil_tobool (avm_memcell* m){return 0;}
unsigned char undef_tobool (avm_memcell* m){assert(0);return 0;}


tobool_func_t toboolFuncs[]={
  number_tobool,
  string_tobool,
  bool_tobool,
  table_tobool,
  userfunc_tobool,
  libfunc_tobool,
  nil_tobool,
  undef_tobool
};



unsigned char avm_tobool (avm_memcell* m){
  assert(m->type>=0&&m->type<undef_m);
  return (*toboolFuncs[m->type])(m);
}


void execute_jeq(instruction* instr){
  assert(instr->result->type==label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1,&ax);
  //printf("%p\n",instr->arg2);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2,&bx);
//printf("Aa\n");
  assert(rv1&&(&stack[AVM_STACKSIZE-1]>=rv1&&rv1>&stack[top]||rv1==&ax ));
  assert(rv2&&(&stack[AVM_STACKSIZE-1]>=rv2&&rv2>&stack[top]||rv2==&bx ));
 // printf("%d\n",rv2->type);
  unsigned char result =0;
  //printf("type %s %d\n",,rv2->data.numVal);
  if(rv1->type==undef_m||rv2->type==undef_m)
    avm_error("'undef' involved in equality!");
  else if(rv1->type==nil_m||rv2->type==nil_m)
          result= (rv1->type == nil_m)&&(rv2->type == nil_m);
  else if(rv1->type==bool_m||rv2->type==bool_m)
          result=((avm_tobool(rv1))==avm_tobool(rv2));
  else if (rv1->type!=rv2->type)
          avm_error("Jeq operands of different type");
  else{
        switch(rv1->type){

          case number_m:result=!strcmp(number_tostring(rv1),number_tostring(rv2));break;
          case string_m:result=!strcmp(rv1->data.strVal,rv2->data.strVal);break;
          case table_m :result=(rv1->data.tableVal==rv2->data.tableVal);break;//edw niths
          case userfunc_m:result=(rv1->data.funcVal==rv2->data.funcVal);break;
          case libfunc_m:result=(rv1->data.libfuncVal==rv2->data.libfuncVal);break;
          default:assert(0);
        }
  }

  if(!executionFinished&&result)
    pc=instr->result->val;

 // printf("%d %d\n",pc,instr->result->val);
}



void execute_jne(instruction* instr){
  assert(instr->result->type==label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1,&ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2,&bx);

  assert(rv1&&(&stack[AVM_STACKSIZE-1]>=rv1&&rv1>&stack[top]||rv1==&ax ));
  assert(rv2&&(&stack[AVM_STACKSIZE-1]>=rv2&&rv2>&stack[top]||rv2==&bx ));

  unsigned char result =0;
  //printf("types are %d %d\n\n",rv1->type,rv2->type);
  if(rv1->type==undef_m||rv2->type==undef_m)
    avm_error("'undef' involved in equality!");
  else if(rv1->type==nil_m||rv2->type==nil_m)
        result= (rv1->type != nil_m)||(rv2->type != nil_m);
  else if(rv1->type==bool_m||rv2->type==bool_m)
        result=((avm_tobool(rv1))!=avm_tobool(rv2));
  else if (rv1->type!=rv2->type)
      avm_error("Jeq operands of different type");
  else{
        switch(rv1->type){
          case number_m:result=strcmp(number_tostring(rv1),number_tostring(rv2));break;
          case string_m:result=strcmp(rv1->data.strVal,rv2->data.strVal);break;
          case table_m : result=(rv1->data.tableVal!=rv2->data.tableVal);break;//edw niths
          case userfunc_m:result=(rv1->data.funcVal!=rv2->data.funcVal);break;
          case libfunc_m:result=(rv1->data.libfuncVal!=rv2->data.libfuncVal);break;
          default:assert(0);
        }
  }


 if(!executionFinished&&result)
    pc=instr->result->val;


}

void execute_jle(instruction* instr){
  assert(instr->result->type==label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1,&ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2,&bx);

  assert(rv1&&(&stack[AVM_STACKSIZE-1]>=rv1&&rv1>&stack[top]||rv1==&ax ));
  assert(rv2&&(&stack[AVM_STACKSIZE-1]>=rv2&&rv2>&stack[top]||rv2==&bx ));

  unsigned char result =0;
  if(rv1->type==undef_m||rv2->type==undef_m)
    avm_error("'undef' involved in equality!");
  else if(rv1->type==nil_m||rv2->type==nil_m)
    avm_error("invalid use of comparison operator on nil");
  else if(rv1->type==bool_m||rv2->type==bool_m)
    avm_error("invalid use of comparison operator on bool");
  else if(rv1->type==table_m||rv2->type==table_m)
   avm_error("invalid use of comparison operator on table");
  else if (rv1->type!=rv2->type)
  avm_error("Jeq operands of different type");
  else{
       switch(rv1->type){
          case number_m:result=(rv1->data.numVal<=rv2->data.numVal);break;
          case string_m:result=strcmp(rv1->data.strVal,rv2->data.strVal);break; //to be checked
          case userfunc_m:result=(rv1->data.funcVal<=rv2->data.funcVal);break;
          case libfunc_m:result=(rv1->data.libfuncVal<=rv2->data.libfuncVal);break;
          default:assert(0);
        }
  }

  if(!executionFinished&&result)
    pc=instr->result->val;

}
void execute_jge(instruction* instr){
   assert(instr->result->type==label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1,&ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2,&bx);

  assert(rv1&&(&stack[AVM_STACKSIZE-1]>=rv1&&rv1>&stack[top]||rv1==&ax ));
  assert(rv2&&(&stack[AVM_STACKSIZE-1]>=rv2&&rv2>&stack[top]||rv2==&bx ));

  unsigned char result =0;
  if(rv1->type==undef_m||rv2->type==undef_m)
    avm_error("'undef' involved in equality!");
  else if(rv1->type==nil_m||rv2->type==nil_m)
    avm_error("invalid use of comparison operator on nil");
  else if(rv1->type==bool_m||rv2->type==bool_m)
    avm_error("invalid use of comparison operator on bool");
  else if(rv1->type==table_m||rv2->type==table_m)
   avm_error("invalid use of comparison operator on table");
  else if (rv1->type!=rv2->type)
  avm_error("Jeq operands of different type");
  else{
       switch(rv1->type){
          case number_m:result=(rv1->data.numVal>=rv2->data.numVal);break;
          case string_m:result=strcmp(rv1->data.strVal,rv2->data.strVal);break; //to be checked
          case userfunc_m:result=(rv1->data.funcVal>=rv2->data.funcVal);break;
          case libfunc_m:result=(rv1->data.libfuncVal>=rv2->data.libfuncVal);break;
          default:assert(0);
        }
  }

  if(!executionFinished&&result)
    pc=instr->result->val;
}


void execute_jlt(instruction* instr){
  assert(instr->result->type==label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1,&ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2,&bx);

  assert(rv1&&(&stack[AVM_STACKSIZE-1]>=rv1&&rv1>&stack[top]||rv1==&ax ));
  assert(rv2&&(&stack[AVM_STACKSIZE-1]>=rv2&&rv2>&stack[top]||rv2==&bx ));

  unsigned char result =0;
  if(rv1->type==undef_m||rv2->type==undef_m)
    avm_error("'undef' involved in equality!");
  else if(rv1->type==nil_m||rv2->type==nil_m)
    avm_error("invalid use of comparison operator on nil");
  else if(rv1->type==bool_m||rv2->type==bool_m)
    avm_error("invalid use of comparison operator on bool");
  else if(rv1->type==table_m||rv2->type==table_m)
   avm_error("invalid use of comparison operator on table");
  else if (rv1->type!=rv2->type)
  avm_error("Jeq operands of different type");
  else{
       switch(rv1->type){
          case number_m:result=(rv1->data.numVal<rv2->data.numVal);break;
          case string_m:result=strcmp(rv1->data.strVal,rv2->data.strVal);break; //to be checked
          case userfunc_m:result=(rv1->data.funcVal<rv2->data.funcVal);break;
          case libfunc_m:result=(rv1->data.libfuncVal<rv2->data.libfuncVal);break;
          default:assert(0);
        }
  }

  if(!executionFinished&&result)
    pc=instr->result->val;


}
void execute_jgt(instruction* instr){
  assert(instr->result->type==label_a);
  avm_memcell* rv1 = avm_translate_operand(instr->arg1,&ax);
  avm_memcell* rv2 = avm_translate_operand(instr->arg2,&bx);

  assert(rv1&&(&stack[AVM_STACKSIZE-1]>=rv1&&rv1>&stack[top]||rv1==&ax ));
  assert(rv2&&(&stack[AVM_STACKSIZE-1]>=rv2&&rv2>&stack[top]||rv2==&bx ));

  unsigned char result =0;
  if(rv1->type==undef_m||rv2->type==undef_m)
    avm_error("'undef' involved in equality!");
  else if(rv1->type==nil_m||rv2->type==nil_m)
    avm_error("invalid use of comparison operator on nil");
  else if(rv1->type==bool_m||rv2->type==bool_m)
    avm_error("invalid use of comparison operator on bool");
  else if(rv1->type==table_m||rv2->type==table_m)
   avm_error("invalid use of comparison operator on table");
  else if (rv1->type!=rv2->type)
  avm_error("Jeq operands of different type");
  else{
       switch(rv1->type){
          case number_m:result=(rv1->data.numVal>rv2->data.numVal);break;
          case string_m:result=strcmp(rv1->data.strVal,rv2->data.strVal);break; //to be checked
          case userfunc_m:result=(rv1->data.funcVal>rv2->data.funcVal);break;
          case libfunc_m:result=(rv1->data.libfuncVal>rv2->data.libfuncVal);break;
          default:assert(0);
        }
  }

  if(!executionFinished&&result)
    pc=instr->result->val;


}



typedef void (*library_func_t)();


void libfunc_print ();
void libfunc_input ();
void libfunc_objectmemberkeys ();
void libfunc_objecttotalmembers();
void libfunc_objectcopy ();
void libfunc_totalarguments ();
void libfunc_argument ();
void libfunc_typeof ();
void libfunc_strtonum ();
void libfunc_sqrt ();
void libfunc_cos ();
void libfunc_sin ();

library_func_t libraryFuncs[]={
  libfunc_print,
  libfunc_input,
  libfunc_objectmemberkeys,
  libfunc_objecttotalmembers,
  libfunc_objectcopy,
  libfunc_totalarguments,
  libfunc_argument,
  libfunc_typeof,
  libfunc_strtonum,
  libfunc_sqrt,
  libfunc_cos,
  libfunc_sin
};
library_func_t avm_getlibraryfunc (char* id){
    if(!strcmp(id,"print")) return libraryFuncs[0];
    else if(!strcmp(id,"input"))return libraryFuncs[1];
    else if(!strcmp(id,"objectmemberkeys"))return libraryFuncs[2];
    else if(!strcmp(id,"objecttotalmembers"))return libraryFuncs[3];
    else if(!strcmp(id,"objectcopy"))return libraryFuncs[4];
    else if(!strcmp(id,"totalarguments"))return libraryFuncs[5];
    else if(!strcmp(id,"argument"))return libraryFuncs[6];
    else if(!strcmp(id,"typeof"))return libraryFuncs[7];
    else if(!strcmp(id,"strtonum"))return libraryFuncs[8];
    else if(!strcmp(id,"sqrt"))return libraryFuncs[9];
    else if(!strcmp(id,"cos"))return libraryFuncs[10];
    else if(!strcmp(id,"sin"))return libraryFuncs[11];
    else return NULL;
}

int avm_totalactuals(){
    return avm_get_envvalue(topsp+AVM_NUMACTUALS_OFFSET);
}

avm_memcell* avm_getactual(int i){
    assert(i<avm_totalactuals());
    return &stack[topsp+AVM_STACKENV_SIZE+1+i];
}

void avm_calllibfunc(char* id){
    library_func_t f=avm_getlibraryfunc(id);
    if(!f){
        printf("unsupported lib func '%s' called!\n",id);
        executionFinished=1;
    }else{
        topsp=top;
        totalActuals=0;

        (*f)();
        if(!executionFinished){
            execute_funcexit(NULL);}

    }

}

void libfunc_typeof (){
    int n=avm_totalactuals();

    if(n!=1){
      printf("one argument (not %d) expected in -typeof-!\n",n);
      executionFinished=1;
    }else{
          avm_memcellclear(&retval);
          retval.type=string_m;
          retval.data.strVal=strdup(typeStrings[avm_getactual(0)->type]);
    }
}

void libfunc_totalarguments(){

  int p_topsp=stack[topsp+AVM_SAVEDTOPSP_OFFSET].data.numVal;
  avm_memcellclear(&retval);
  if(stack[p_topsp+1].type==undef_m){
      printf("-totalarguments- called outside a function!\n");

      retval.type=nil_m;
  }else{
        int p_topsp=avm_get_envvalue(topsp+1);
        avm_memcellclear(&retval);
        retval.type=number_m;
        retval.data.numVal=avm_get_envvalue(p_topsp+AVM_NUMACTUALS_OFFSET);
    }

}


void libfunc_print(){

    int n =avm_totalactuals(),i=0;
    for(i=0;i<n;++i){

        avm_memcell* lol=avm_getactual(i);

        char* tsi=avm_tostring(lol);

        char* s=strdup(tsi);


        printf("%s",s);

        //free(s);

    }
}
/*
void libfunc_input (){

  int length = 100; //initial size
  char * name = malloc(length * sizeof(char)+1); //allocate mem for 100 chars
  int count = 0; //to keep track of how many chars have been used
  char c; // to store the current char

 while((c = getchar()) != '\n'){ //keep reading until a newline
     if(count >= length)
        name = realloc(name, (length += 10) * sizeof(char)); //add room for 10 more chars
     name[count++] = c;
  }
  name[count]='\0';

  avm_memcellclear(&retval);

  char* dublicate=strdup(name);
  double dublicate_num =atof(name);

  if(!strcmp(dublicate,"true")){
    retval.type=bool_m;
    retval.data.boolVal=1;
  }else if(!strcmp(dublicate,"false")){
    retval.type=bool_m;
    retval.data.boolVal=0;
  }else if(!strcmp(dublicate,"nil")){
    retval.type=nil_m;
  }else if(dublicate_num!=(double)0){
    retval.type=number_m;

    retval.data.numVal=dublicate_num;
  }else {
      retval.type=string_m;
      retval.data.strVal=strdup(name);
  }

}

*/
void libfunc_input (){

  int length = 100; //initial size
  char * name = malloc(length * sizeof(char)+1); //allocate mem for 100 chars
  int count = 0; //to keep track of how many chars have been used
  char c; // to store the current char
  int i;
 while((c = getchar()) != '\n'){ //keep reading until a newline
     if(count >= length)
        name = realloc(name, (length += 10) * sizeof(char)); //add room for 10 more chars
     name[count++] = c;
  }
  name[count]='\0';

  avm_memcellclear(&retval);

  char* dublicate=strdup(name);
  double dublicate_num;
  int flag=0;
  sscanf(dublicate, "%lf", &dublicate_num);

  for(i=0;i<strlen(dublicate);i++)
      if(!isdigit(dublicate[i])&&dublicate[i]!='.') flag=1;




  if(!strcmp(dublicate,"true")){
    retval.type=bool_m;
    retval.data.boolVal=1;
  }else if(!strcmp(dublicate,"false")){
    retval.type=bool_m;
    retval.data.boolVal=0;
  }else if(!strcmp(dublicate,"nil")){
    retval.type=nil_m;
  }else if(dublicate_num!=(double)0&&!flag){
    retval.type=number_m;
    retval.data.numVal=dublicate_num;
  }else {
      retval.type=string_m;
      retval.data.strVal=strdup(name);
  }

}
void libfunc_objectmemberkeys (){
      int n =avm_totalactuals();
      if(!n){
        avm_error("objectmemberkeys called with more than one argument");
        return;
      }
      avm_memcell* temp=avm_getactual(0);
      if(temp->type!=table_m) {
        avm_error("objectmemberkeys called with argument other than table");
        return;
      }

      avm_memcellclear(&retval);
      //avm_memcell* retval=malloc(sizeof(struct avm_memcell));
      retval.type=table_m;
      retval.data.tableVal=avm_tablenew();
      avm_tableincrefcounter(retval.data.tableVal);

      int counter=0,i=0;
      for( i=0;i<AVM_TABLE_HASHSIZE;i++){

        avm_table_bucket* temp_bucket=temp->data.tableVal->strIndexed[i];
        while(temp_bucket) {
          avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
          new_cell_key->type=number_m;
          new_cell_key->data.numVal=(double)counter++;

          avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
          new_cell_value->type=string_m;
          new_cell_value->data.strVal=strdup(temp_bucket->key->data.strVal);

          avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

          temp_bucket=temp_bucket->next;
        }

      }
      for( i=0;i<AVM_TABLE_HASHSIZE;i++){

        avm_table_bucket* temp_bucket=temp->data.tableVal->numIndexed[i];
        while(temp_bucket) {
          avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
          new_cell_key->type=number_m;
          new_cell_key->data.numVal=(double)counter++;

          avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
          new_cell_value->type=number_m;
          new_cell_value->data.numVal=temp_bucket->key->data.numVal;

          avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

          temp_bucket=temp_bucket->next;
        }

      }
      for( i=0;i<AVM_TABLE_HASHSIZE;i++){

        avm_table_bucket* temp_bucket=temp->data.tableVal->tableIndexed[i];
        while(temp_bucket) {
          avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
          new_cell_key->type=number_m;
          new_cell_key->data.numVal=(double)counter++;

          avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
          new_cell_value->type=table_m;
          new_cell_value->data.tableVal=temp_bucket->key->data.tableVal;
          avm_tableincrefcounter(temp_bucket->key->data.tableVal);
          avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

          temp_bucket=temp_bucket->next;
        }

      }
      for( i=0;i<AVM_TABLE_HASHSIZE;i++){

        avm_table_bucket* temp_bucket=temp->data.tableVal->funcIndexed[i];
        while(temp_bucket) {
          avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
          new_cell_key->type=number_m;
          new_cell_key->data.numVal=(double)counter++;

          avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
          new_cell_value->type=userfunc_m;
          new_cell_value->data.funcVal=temp_bucket->key->data.funcVal;

          avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

          temp_bucket=temp_bucket->next;
        }

      }
      for( i=0;i<AVM_TABLE_HASHSIZE;i++){

        avm_table_bucket* temp_bucket=temp->data.tableVal->libfuncIndexed[i];
        while(temp_bucket) {
          avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
          new_cell_key->type=number_m;
          new_cell_key->data.numVal=(double)counter++;

          avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
          new_cell_value->type=libfunc_m;
          new_cell_value->data.libfuncVal=strdup(temp_bucket->key->data.libfuncVal);

          avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

          temp_bucket=temp_bucket->next;
        }

      }
      for( i=0;i<2;i++){

        avm_table_bucket* temp_bucket=temp->data.tableVal->boolIndexed[i];
        while(temp_bucket) {
          avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
          new_cell_key->type=number_m;
          new_cell_key->data.numVal=(double)counter++;

          avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
          new_cell_value->type=bool_m;
          new_cell_value->data.boolVal=temp_bucket->key->data.boolVal;

          avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

          temp_bucket=temp_bucket->next;
        }

      }

      retval.data.tableVal->total=counter;
}
void libfunc_objecttotalmembers(){
  int n =avm_totalactuals();
  if(!n){
    avm_error("objecttotalmembers called with more than one argument");
    return;
  }
  avm_memcell* temp=avm_getactual(0);
  if(temp->type!=table_m) {
    avm_error("objecttotalmembers called with argument other than table");
    return;
  }

  avm_memcellclear(&retval);
  //avm_memcell* retval=malloc(sizeof(struct avm_memcell));
  retval.type=number_m;
  retval.data.numVal=temp->data.tableVal->total;

}
void libfunc_objectcopy (){
  int n =avm_totalactuals();
  if(!n){
    avm_error("objectcopy called with more than one argument");
    return;
  }
  avm_memcell* temp=avm_getactual(0);
  if(temp->type!=table_m) {
    avm_error("objectcopy called with argument other than table");
    return;
  }

  avm_memcellclear(&retval);
  retval.type=table_m;
  retval.data.tableVal=avm_tablenew();
  avm_tableincrefcounter(retval.data.tableVal);

  int i=0;
  for(i=0;i<AVM_TABLE_HASHSIZE;i++){

    avm_table_bucket* temp_bucket=temp->data.tableVal->strIndexed[i];
    while(temp_bucket) {
      avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
      new_cell_key->type=string_m;
      new_cell_key->data.strVal=strdup(temp_bucket->key->data.strVal);

      avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
      new_cell_value->type=temp_bucket->value->type;
      switch (new_cell_value->type) {
        case  number_m:
          new_cell_value->data.numVal=temp_bucket->value->data.numVal;
          break;
        case  string_m:
          new_cell_value->data.strVal=strdup(temp_bucket->value->data.strVal);
          break;
        case  bool_m:
            new_cell_value->data.boolVal=temp_bucket->value->data.boolVal;
            break;
        case  table_m:
        //edw kaluvetai kai to shallow copy
            avm_tableincrefcounter(temp_bucket->value->data.tableVal);
            new_cell_value->data.tableVal=temp_bucket->value->data.tableVal;
            break;
        case  userfunc_m:
            new_cell_value->data.funcVal=temp_bucket->value->data.funcVal;
            break;
        case  libfunc_m:
            new_cell_value->data.libfuncVal=strdup(temp_bucket->value->data.libfuncVal);
            break;
        case  nil_m:
            break;
        case  undef_m:
            break;
        default:assert(0);
      }
      //new_cell_value->data.strVal=strdup(temp_bucket->key->data.strVal);

      avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

      temp_bucket=temp_bucket->next;

    }
  }


  for(i=0;i<AVM_TABLE_HASHSIZE;i++){

    avm_table_bucket* temp_bucket=temp->data.tableVal->numIndexed[i];
    while(temp_bucket) {
      avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
      new_cell_key->type=number_m;
      new_cell_key->data.numVal=temp_bucket->key->data.numVal;

      avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
      new_cell_value->type=temp_bucket->value->type;
      switch (new_cell_value->type) {
        case  number_m:
          new_cell_value->data.numVal=temp_bucket->value->data.numVal;
          break;
        case  string_m:
          new_cell_value->data.strVal=strdup(temp_bucket->value->data.strVal);
          break;
        case  bool_m:
            new_cell_value->data.boolVal=temp_bucket->value->data.boolVal;
            break;
        case  table_m:
        //edw kaluvetai kai to shallow copy
            avm_tableincrefcounter(temp_bucket->value->data.tableVal);
            new_cell_value->data.tableVal=temp_bucket->value->data.tableVal;
            break;
        case  userfunc_m:
            new_cell_value->data.funcVal=temp_bucket->value->data.funcVal;
            break;
        case  libfunc_m:
            new_cell_value->data.libfuncVal=strdup(temp_bucket->value->data.libfuncVal);
            break;
        case  nil_m:
            break;
        case  undef_m:
            break;
        default:assert(0);
      }
      //new_cell_value->data.strVal=strdup(temp_bucket->key->data.strVal);

      avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

      temp_bucket=temp_bucket->next;

    }
  }
  for(i=0;i<AVM_TABLE_HASHSIZE;i++){

    avm_table_bucket* temp_bucket=temp->data.tableVal->tableIndexed[i];
    while(temp_bucket) {
      avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
      new_cell_key->type=table_m;
      new_cell_key->data.tableVal=temp_bucket->key->data.tableVal;
      avm_tableincrefcounter(temp_bucket->key->data.tableVal);
      avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
      new_cell_value->type=temp_bucket->value->type;
      switch (new_cell_value->type) {
        case  number_m:
          new_cell_value->data.numVal=temp_bucket->value->data.numVal;
          break;
        case  string_m:
          new_cell_value->data.strVal=strdup(temp_bucket->value->data.strVal);
          break;
        case  bool_m:
            new_cell_value->data.boolVal=temp_bucket->value->data.boolVal;
            break;
        case  table_m:
        //edw kaluvetai kai to shallow copy
            avm_tableincrefcounter(temp_bucket->value->data.tableVal);
            new_cell_value->data.tableVal=temp_bucket->value->data.tableVal;
            break;
        case  userfunc_m:
            new_cell_value->data.funcVal=temp_bucket->value->data.funcVal;
            break;
        case  libfunc_m:
            new_cell_value->data.libfuncVal=strdup(temp_bucket->value->data.libfuncVal);
            break;
        case  nil_m:
            break;
        case  undef_m:
            break;
        default:assert(0);
      }
      //new_cell_value->data.strVal=strdup(temp_bucket->key->data.strVal);

      avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

      temp_bucket=temp_bucket->next;

    }
  }
  for(i=0;i<AVM_TABLE_HASHSIZE;i++){

    avm_table_bucket* temp_bucket=temp->data.tableVal->funcIndexed[i];
    while(temp_bucket) {
      avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
      new_cell_key->type=userfunc_m;
      new_cell_key->data.funcVal=temp_bucket->key->data.funcVal;

      avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
      new_cell_value->type=temp_bucket->value->type;
      switch (new_cell_value->type) {
        case  number_m:
          new_cell_value->data.numVal=temp_bucket->value->data.numVal;
          break;
        case  string_m:
          new_cell_value->data.strVal=strdup(temp_bucket->value->data.strVal);
          break;
        case  bool_m:
            new_cell_value->data.boolVal=temp_bucket->value->data.boolVal;
            break;
        case  table_m:
        //edw kaluvetai kai to shallow copy
            avm_tableincrefcounter(temp_bucket->value->data.tableVal);
            new_cell_value->data.tableVal=temp_bucket->value->data.tableVal;
            break;
        case  userfunc_m:
            new_cell_value->data.funcVal=temp_bucket->value->data.funcVal;
            break;
        case  libfunc_m:
            new_cell_value->data.libfuncVal=strdup(temp_bucket->value->data.libfuncVal);
            break;
        case  nil_m:
            break;
        case  undef_m:
            break;
        default:assert(0);
      }
      //new_cell_value->data.strVal=strdup(temp_bucket->key->data.strVal);

      avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

      temp_bucket=temp_bucket->next;

    }
  }
  for(i=0;i<AVM_TABLE_HASHSIZE;i++){

    avm_table_bucket* temp_bucket=temp->data.tableVal->libfuncIndexed[i];
    while(temp_bucket) {
      avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
      new_cell_key->type=libfunc_m;
      new_cell_key->data.libfuncVal=strdup(temp_bucket->key->data.libfuncVal);

      avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
      new_cell_value->type=temp_bucket->value->type;
      switch (new_cell_value->type) {
        case  number_m:
          new_cell_value->data.numVal=temp_bucket->value->data.numVal;
          break;
        case  string_m:
          new_cell_value->data.strVal=strdup(temp_bucket->value->data.strVal);
          break;
        case  bool_m:
            new_cell_value->data.boolVal=temp_bucket->value->data.boolVal;
            break;
        case  table_m:
        //edw kaluvetai kai to shallow copy
            avm_tableincrefcounter(temp_bucket->value->data.tableVal);
            new_cell_value->data.tableVal=temp_bucket->value->data.tableVal;
            break;
        case  userfunc_m:
            new_cell_value->data.funcVal=temp_bucket->value->data.funcVal;
            break;
        case  libfunc_m:
            new_cell_value->data.libfuncVal=strdup(temp_bucket->value->data.libfuncVal);
            break;
        case  nil_m:
            break;
        case  undef_m:
            break;
        default:assert(0);
      }
      //new_cell_value->data.strVal=strdup(temp_bucket->key->data.strVal);

      avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

      temp_bucket=temp_bucket->next;

    }
  }
  for(i=0;i<2;i++){

    avm_table_bucket* temp_bucket=temp->data.tableVal->boolIndexed[i];
    while(temp_bucket) {
      avm_memcell* new_cell_key=malloc(sizeof(struct avm_memcell));
      new_cell_key->type=bool_m;
      new_cell_key->data.boolVal=temp_bucket->key->data.boolVal;

      avm_memcell* new_cell_value=malloc(sizeof(struct avm_memcell));
      new_cell_value->type=temp_bucket->value->type;
      switch (new_cell_value->type) {
        case  number_m:
          new_cell_value->data.numVal=temp_bucket->value->data.numVal;
          break;
        case  string_m:
          new_cell_value->data.strVal=strdup(temp_bucket->value->data.strVal);
          break;
        case  bool_m:
            new_cell_value->data.boolVal=temp_bucket->value->data.boolVal;
            break;
        case  table_m:
        //edw kaluvetai kai to shallow copy
            avm_tableincrefcounter(temp_bucket->value->data.tableVal);
            new_cell_value->data.tableVal=temp_bucket->value->data.tableVal;
            break;
        case  userfunc_m:
            new_cell_value->data.funcVal=temp_bucket->value->data.funcVal;
            break;
        case  libfunc_m:
            new_cell_value->data.libfuncVal=strdup(temp_bucket->value->data.libfuncVal);
            break;
        case  nil_m:
            break;
        case  undef_m:
            break;
        default:assert(0);
      }
      //new_cell_value->data.strVal=strdup(temp_bucket->key->data.strVal);

      avm_tablesetelem(retval.data.tableVal,new_cell_key,new_cell_value);

      temp_bucket=temp_bucket->next;

    }
  }

  //retval.data.tableVal->total=temp->data.tableVal->total;

}
void libfunc_argument (){
 // int p_topsp=avm_get_envvalue(topsp+AVM_SAVEDTOPSP_OFFSET);
  avm_memcellclear(&retval);
  int p_topsp=stack[topsp+AVM_SAVEDTOPSP_OFFSET].data.numVal;
  if(stack[p_topsp+1].type==undef_m){
    printf("totalarguments called outside of a function\n");
    retval.type=nil_m;
  }else{
    int oldtopsp=avm_get_envvalue(topsp+1);
    int currentActual=avm_get_envvalue(topsp+AVM_STACKENV_SIZE+1);
    int actuals=avm_get_envvalue(oldtopsp+AVM_STACKENV_SIZE);
    if(currentActual+1>actuals) avm_error("argument called with more than available arguments");
    avm_memcell* t=&stack[oldtopsp+AVM_STACKENV_SIZE+currentActual+1];
    switch(t->type){
      case   number_m:{retval.type=number_m;retval.data.numVal=t->data.numVal;break;}
      case   string_m:{retval.type=string_m;retval.data.strVal=strdup(t->data.strVal);break;}
      case   bool_m:{retval.type=bool_m;retval.data.boolVal=t->data.boolVal;break;}
      case   table_m:{retval.type=table_m;retval.data.tableVal=t->data.tableVal;break;}
      case   userfunc_m:{retval.type=userfunc_m;retval.data.funcVal=t->data.funcVal;break;}
      case   libfunc_m:{retval.type=libfunc_m;retval.data.libfuncVal=strdup(t->data.libfuncVal);break;}
      case   nil_m:{retval.type=nil_m;break;}
      case   undef_m:{assert(0);break;}
    }


  }
}

void libfunc_strtonum (char* str){

  int n =avm_totalactuals();
  if(!n){
    avm_error("strtonum called with more than one argument");
    return;
  }
  avm_memcell* temp=avm_getactual(0);
  if(temp->type!=string_m) {
    avm_error("strtonum called with argument other than string");
    return;
  }
  avm_memcellclear(&retval);
  int i;
  retval.type=nil_m;
  double temp_num;
  char* temp_string=strdup(temp->data.strVal);
  sscanf(temp_string, "%lf", &temp_num);
  for(i=0;i<strlen(temp_string);i++){
      if(!isdigit(temp_string[i])&&temp_string[i]!='.') return;
  }


  if(temp_num!=(double)0){
    retval.type=number_m;
    retval.data.numVal=temp_num;
  }



}


void libfunc_sqrt (){

  int n =avm_totalactuals();
  if(!n){
    avm_error("sqrt called with more than one argument");
    return;
  }
  avm_memcell* temp=avm_getactual(0);
  if(temp->type!=number_m) {
    avm_error("sqrt called with argument other than number");
    return;
  }
  avm_memcellclear(&retval);
  /*default value*/
  retval.type=nil_m;
  if(temp->data.numVal>=(double)0){
    double temp_num=sqrt(temp->data.numVal);
    retval.type=number_m;
    retval.data.numVal=temp_num;
  }


}
void libfunc_cos (){

  int n =avm_totalactuals();
  if(!n){
    avm_error("cos called with more than one argument");
    return;
  }
  avm_memcell* temp=avm_getactual(0);
  if(temp->type!=number_m) {
    avm_error("cos called with argument other than number");
    return;
  }
  avm_memcellclear(&retval);

  double temp_num=cos(temp->data.numVal);
  retval.type=number_m;
  retval.data.numVal=temp_num;


}
void libfunc_sin (){
  int n =avm_totalactuals();
  if(!n){
    avm_error("sin called with more than one argument");
    return;
  }
  avm_memcell* temp=avm_getactual(0);
  if(temp->type!=number_m) {
    avm_error("sin called with argument other than number");
    return;
  }
  avm_memcellclear(&retval);

  double temp_num=sin(temp->data.numVal);
  retval.type=number_m;
  retval.data.numVal=temp_num;

}

void printmemcell(avm_memcell* tmp);
void printbuckets(avm_table_bucket* tmp)
{
  avm_table_bucket* head=tmp;
  printf("\n");
  while(head!=NULL){

    printmemcell(head->key);

    printmemcell(head->value);

    head=head->next;
  }
}



void printmemcell(avm_memcell* tmp){
  int i;
  if(tmp->type==table_m){
    printf("Table:\t");
    //printf("Table RefCounter: %d\t",tmp->data.tableVal->refCounter);
    //printf("Table Total: %d",tmp->data.tableVal->total);
    for(i=0;i<AVM_TABLE_HASHSIZE;i++){
	if(tmp->data.tableVal->strIndexed[i]!=NULL)
	    printbuckets(tmp->data.tableVal->strIndexed[i]);
	if(tmp->data.tableVal->numIndexed[i]!=NULL)
	    printbuckets(tmp->data.tableVal->numIndexed[i]);
  if(tmp->data.tableVal->tableIndexed[i]!=NULL)
	    printbuckets(tmp->data.tableVal->tableIndexed[i]);
  if(tmp->data.tableVal->funcIndexed[i]!=NULL)
      printbuckets(tmp->data.tableVal->funcIndexed[i]);
  if(tmp->data.tableVal->libfuncIndexed[i]!=NULL)
      printbuckets(tmp->data.tableVal->libfuncIndexed[i]);

    }
    if(tmp->data.tableVal->boolIndexed[0]!=NULL)
        printbuckets(tmp->data.tableVal->boolIndexed[0]);
    if(tmp->data.tableVal->boolIndexed[1]!=NULL)
        printbuckets(tmp->data.tableVal->boolIndexed[1]);
    }
  else if(tmp->type==string_m){
    printf("String:%s \n",tmp->data.strVal);
  }
  else if(tmp->type==bool_m)
  {
    printf("Bool:%d \n",tmp->data.boolVal);
  }
  else if(tmp->type==userfunc_m)
  {
    printf("Userfunc %d",readUserFuncs[tmp->data.funcVal].id);
  }
  else if(tmp->type==libfunc_m)
  {
    printf("Libfunc %s \n",tmp->data.libfuncVal);
  }
  else if(tmp->type==number_m)
  {
    printf("NumVal %d \n", (int)tmp->data.numVal);
  }
}








void avm_initialize ()
{
  avm_initstack();
}
