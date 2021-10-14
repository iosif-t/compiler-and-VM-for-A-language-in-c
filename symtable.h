#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<assert.h>
#define size 25
#define max_scope 8




typedef enum SymbolType{
    GLOBAL,LOCL,FORMAL,USERFUNC,LIBFUNC
}Type;

typedef enum scopespace_type{programvar,functionlocal,formalarg}scopespace_t;
typedef enum symbol_type{var_s,programfunc_s,libraryfunc_s}symbol_t;

char* scopespaceToStr(scopespace_t space){
    switch(space){
        case  programvar    : return "programvar";
        case  functionlocal : return "functionlocal";
        case  formalarg     : return "formalarg";
        default             : return " ";
    }
}

typedef struct Function{
    const char *name;
    struct Function *next;
     int offset;
    scopespace_t  type;
}args;

typedef struct SymbolNode{
    int localSize;
    int iaddress;
    symbol_t   newType;
    scopespace_t space;
     int offset;
    const char *name;
     int scope;
     int line;
    struct SymbolNode *next;
    struct SymbolNode *next_scope;
    Type type;
    int isActive;
    args *FuncArgs;
}node;


struct jumpStack{

    int data;
    struct jumpStack *link;
};
struct jumpStack* jumpHead=NULL;
int jump_pop(){
    int value=-1;

    if(jumpHead!=NULL){
        value=jumpHead->data;
        jumpHead = jumpHead->link;
    }
    return value;
}


void jump_push(int val){

    struct jumpStack *temp;
    temp = malloc(sizeof(struct jumpStack));
    temp->data = val;
    temp->link = jumpHead;
    jumpHead = temp;
}


struct loopStack{

    int data;
    struct loopStack *link;
};
struct loopStack * loopHead=NULL;
int loop_pop(){
    int value=-1;

    if(loopHead!=NULL){
        value=loopHead->data;
        loopHead = loopHead->link;
    }
    return value;
}


void loop_push(int val){

    struct loopStack *temp;
    temp = malloc(sizeof(struct loopStack));
    temp->data = val;
    temp->link = loopHead;
    loopHead = temp;
}




struct stackNode{

    node *data;
    struct stackNode *link;
};
struct stackNode *nodeHead=NULL;
node *func_pop(){
    node *value=NULL;

    if(nodeHead!=NULL){

        value=nodeHead->data;
        nodeHead = nodeHead->link;
    }
    return value;
}



void func_push(node* val){

    struct stackNode *temp;
    temp = malloc(sizeof(struct stackNode));
   // printf("\n\n\n%s\n\n\n",temp);
   // printf("\n\n\n%s\n\n\n",temp);
  //  printf("\n\n\n%s\n\n\n",temp);
    temp->data = val;
    temp->link = nodeHead;
    nodeHead = temp;
}


struct offsetStack{

     int data;
    struct offsetStack *link;
};
struct offsetStack* offsetHead=NULL;

 int offset_pop(){
     int value=-1;

    if(offsetHead!=NULL){

        value=offsetHead->data;
        offsetHead = offsetHead->link;
    }
    return value;
}


void offset_push( int val){

    struct offsetStack *temp;
    temp = malloc(sizeof(struct offsetStack));
    temp->data = val;
    temp->link = offsetHead;
    offsetHead = temp;
}
 int programVarOffset=0;
 int functionLocalOffset=0;
 int formalArgOffset=0;
 int scopeSpaceCounter =1;

node *heads[max_scope]; // heads gia tis upolistes twn scopes
node *hash_array[size]; //pointers se listes tou hastable

scopespace_t currscopespace(){
    if(scopeSpaceCounter==1)
        return programvar;
    else if(scopeSpaceCounter%2==0)
        return formalarg;
    else
        return functionlocal;

}

 int currscopeoffset(){
    switch(currscopespace()){
        case programvar      : return programVarOffset;
        case functionlocal   : return functionLocalOffset;
        case formalarg       : return formalArgOffset;
        default              : break;
    }
}
void inccurrescopeoffset(){
    switch(currscopespace()){
        case programvar      :  ++programVarOffset; break;
        case functionlocal   :  ++functionLocalOffset; break;
        case formalarg       :  ++formalArgOffset; break;
        default              : assert(0);
    }
}

void enterscopespace(){++scopeSpaceCounter;}

void exitscopespace(){
    assert(scopeSpaceCounter>1);
    --scopeSpaceCounter;
}

/*Return the index where the symbol is going */
/*to be inserted based on it's name*/
int hash_function(const char *a){
    int i=0;
    int mul=3;
    int return_value=0;
    for(i=0;a[i]!='\0';i++)
        return_value=mul*return_value+a[i];

    return return_value%size;
}


node *create_node(const char*name,Type type,int line ,int scope){
    node *new_node=malloc(sizeof(node));
    new_node->localSize=0;
    new_node->isActive=1;
    new_node->line=line;
    new_node->name=strdup(name);
    new_node->scope=scope;
    new_node->type=type;
    new_node->next_scope=NULL;
    new_node->next=NULL;
    new_node->FuncArgs=NULL;
    return new_node;
}

node* insert(const char *name,Type type,int line,int scope);

args* insertArguments(node * function,const char *arg){
    args *newArg=malloc(sizeof(args));
    newArg->name=strdup(arg);
    newArg->next=NULL;
    newArg->type=formalarg;
    if(function->FuncArgs==NULL)
        function->FuncArgs=newArg;
    else{
        args *tmp=function->FuncArgs;
        while(tmp->next!=NULL)
            tmp=tmp->next;
        tmp->next=newArg;
    }
    node* tmp=insert(arg,FORMAL,function->line,function->scope+1);
    tmp->space=formalarg;
    tmp->isActive=1;
    tmp->offset=currscopeoffset();
    return newArg;
}

node* insert(const char *name,Type type,int line,int scope){
    int index=hash_function(name);
    node *new_node=create_node(name,type,line,scope);
    if(heads[scope]==NULL){
        heads[scope]=new_node;
    }else{
        new_node->next_scope=heads[scope];
        heads[scope]=new_node;
    }

    if(hash_array[index]==NULL)
        hash_array[index]=new_node;
    else{
       new_node->next=hash_array[index];
       hash_array[index]=new_node;
    }
    return new_node;
}

void hide(int scope){
    if(scope>max_scope)
        return;
    node *tmp=heads[scope];
    while(tmp!=NULL){
        tmp->isActive=0;
        tmp=tmp->next_scope;
    }
}

node* scopeLookup(int scope,const char * name){
    node* tmp=heads[scope];
    while(tmp!=NULL&&strcmp(tmp->name,name)){
        if(tmp->isActive){
            args* temp=tmp->FuncArgs;
            while(temp!=NULL){
                if(!strcmp(temp->name,name)){
                    node * newNode=malloc(sizeof(node));
                    newNode->FuncArgs=NULL;
                    newNode->isActive=1;
                    newNode->line=tmp->line;
                    newNode->name=strdup(name);
                    newNode->next=NULL;
                    newNode->next_scope=NULL;
                    newNode->scope=++scope;
                    newNode->type=FORMAL;
                    return newNode;
                }
                temp=temp->next;
            }
        }
        tmp=tmp->next_scope;
    }
   if(tmp==NULL||!tmp->isActive)
        return NULL;
    else if(tmp->isActive)
        return tmp;
    else
        return NULL;
}


/*converts Type to string */
char *enum_to_str(Type type){
  switch(type){
    case GLOBAL:return "global";
    case LOCL:return "local";
    case FORMAL:return "formal";
    case USERFUNC: return "userfunc";
    case LIBFUNC:return "libfunc";
    default:return "";
  }
}
int argLookup(const char *name){
    int i;
    for(i=0;i<max_scope;i++){
        node* tmp=heads[i];
        while(tmp!=NULL){
            args *tempArgs=tmp->FuncArgs;
            while(tempArgs!=NULL){
                if(!strcmp(tempArgs->name,name))
                    return 1;
                tempArgs=tempArgs->next;
            }

          tmp=tmp->next_scope;
      }
  }
  return 0;
}
node* tableLookup(const char *name){
    int index=hash_function(name);
    node * tmp=hash_array[index];
    while(tmp!=NULL&&strcmp(tmp->name,name)){
        if(tmp->isActive){
            args* temp=tmp->FuncArgs;
            while(temp!=NULL){
                if(!strcmp(tmp->name,name)){
                    node * newNode=malloc(sizeof(node));
                    newNode->FuncArgs=NULL;
                    newNode->isActive=1;
                    newNode->line=tmp->line;
                    newNode->name=strdup(name);
                    newNode->next=NULL;
                    newNode->next_scope=NULL;
                    newNode->scope=++(tmp->scope);
                    newNode->type=FORMAL;
                    return newNode;
                }
                temp=temp->next;
                }
            }
        tmp=tmp->next;
    }

    if(tmp==NULL||!tmp->isActive)
        return NULL;
    else if(tmp->isActive)
        return tmp;
    else
        return NULL;
}
int isFunction(const char * name){
  node* temp=tableLookup(name);
  if(temp==NULL) return 0;
  if(temp->type == USERFUNC || temp->type == LIBFUNC){
    return 1;
  }
return 0;
}

int intervenesFunction(int startScope, int endScope) {
    int i=0;
    for (i = endScope; i >= startScope; i--) {
        node* temp = heads[i];
        while (temp != NULL) {
            if (isFunction(temp->name)) return 1;
            temp = temp->next_scope;
        }
    }
    return 0;
}
int checkIfLibFunc(const char * name){
  if(!strcmp(name,"print")||
     !strcmp(name,"input")||
     !strcmp(name,"objectmemberkeys")||
     !strcmp(name,"objecttotalmembers")||
     !strcmp(name,"objectcopy")||
     !strcmp(name,"totalarguments")||
     !strcmp(name,"argument")||
     !strcmp(name,"typeof")||
     !strcmp(name,"stronum")||
     !strcmp(name,"sqrt")||
     !strcmp(name,"cos")||
     !strcmp(name,"sin"))return 1;
  return 0;
}
void initialize(){
    int i=0;
    for(i=0;i<size;i++)
        hash_array[i]=NULL;
    for(i=0;i<max_scope;i++)
        heads[i]=NULL;
    node * t;
    t=insert("print", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("input", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("objectmemberkeys", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("objecttotalmembers", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("objectcopy", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("totalarguments", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("argument", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("typeof", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("strtonum", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("sqrt", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("cos", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
	t=insert("sin", LIBFUNC , 0, 0);
    t->newType=libraryfunc_s;
    t->space=programvar;
    t->offset=-1;
    t->iaddress=-1;
}


int getTotalGlobals(){
    int i=0,result=0;
    for(i=0;i<max_scope;i++){
        node* tmp=heads[i];
        while(tmp){
            if(!strcmp(scopespaceToStr(tmp->space),"programvar")&&tmp->newType==var_s)
              result++;
            tmp=tmp->next_scope;
        }
        tmp=heads[i];
    }
    return result;
}


void printTable(){

  int i;
  node *past_tmp=NULL;
  args *tempArgs=NULL;
  for(i=0;i<size;i++){
      printf("Bucket :%d ",i);
      node *tmp=hash_array[i];
      if(i>0)past_tmp=hash_array[i-1];
      while(past_tmp){
       tempArgs=past_tmp->FuncArgs;
        while(tempArgs){
          printf("\tName: %s, Type: Function Argument of Function:%s Scope: %d, Line: %d\n",tempArgs->name,past_tmp->name, past_tmp->scope+1 ,past_tmp->line);
          tempArgs=tempArgs->next;
        }
        past_tmp=past_tmp->next;
      }
      while(tmp!=NULL){

          printf("\tName: %s, Type: %s, Scope: %d, Line: %d",tmp->name,enum_to_str(tmp->type), tmp->scope ,tmp->line);
          if(tmp->type==USERFUNC||tmp->type==LIBFUNC){
            printf(" with arguments: ");
            tempArgs=tmp->FuncArgs;
            while(tempArgs!=NULL){
              printf("%s ",tempArgs->name);
              tempArgs=tempArgs->next;
            }

          }
          printf("\n");
          tmp=tmp->next;
      }
      printf("\n");
  }
}

void printScopes(){
  int i;
  node *past_tmp=NULL;
  args *tempArgs=NULL;
  for(i=0;i<max_scope;i++){
    printf("Entries with scope %d :\n",i);
      node* tmp=heads[i];
      if(i>0)past_tmp=heads[i-1];
      while(past_tmp){
       tempArgs=past_tmp->FuncArgs;
        while(tempArgs){
          printf("\tName: %s, Type: Function Argument of Function:%s Scope: %d, Line: %d, ScopeSpace: %s ,Offset: %d\n",tempArgs->name,past_tmp->name, past_tmp->scope+1 ,past_tmp->line,scopespaceToStr(tempArgs->type),tempArgs->offset);
          tempArgs=tempArgs->next;
        }
        past_tmp=past_tmp->next_scope;
      }
      while(tmp){
        if(tmp->type!=FORMAL)
          printf("\tName: %s, Type: %s, Line: %d,ScopeSpace: %s,Offset: %d ",tmp->name,enum_to_str(tmp->type) ,tmp->line,scopespaceToStr(tmp->space),tmp->offset);
          if(tmp->type==USERFUNC||tmp->type==LIBFUNC){
            printf("Address: %d with arguments: ",tmp->iaddress);
            tempArgs=tmp->FuncArgs;
            while(tempArgs){
              printf("%s ",tempArgs->name);
              tempArgs=tempArgs->next;
            }

          }
          printf("\n");
          tmp=tmp->next_scope;
      }
      printf("\n");
  }
}
