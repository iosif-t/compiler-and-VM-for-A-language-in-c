#include "symtable.h"
#define EXPAND_SIZE 1024
#define CURR_SIZE (totalQuads*sizeof(quad))
#define NEW_SIZE (EXPAND_SIZE*sizeof(quad)+CURR_SIZE)

extern int currscope();
typedef struct relist{
	int label;
	struct relist * next;
}retlist;


struct retStack{
    retlist *data;
    struct retStack *link;
};
struct retStack* retHead=NULL;

retlist* retPop(){
    retlist *value=NULL;

    if(retHead!=NULL){
        value=retHead->data;
        retHead = retHead->link;
    }
    return value;
}

struct retStack * topS(){

	return retHead;
}

void retPush(retlist* val){

    struct retStack *temp;
    temp = malloc(sizeof(struct retStack));
	temp->data=malloc(sizeof(retlist));
	temp->data->next=NULL;
    temp->data = val;
    temp->link = retHead;
    retHead = temp;
}

void insertRet(int label){
		retlist * newNode=malloc(sizeof(retlist));
		struct retStack *tmp=NULL;
		newNode->next=NULL;
		newNode->label=label;
		tmp=topS();
		if(tmp==NULL){
			tmp->data=newNode;
		}
		else{
			newNode->next=tmp->data;
			tmp->data=newNode;
		}


}
typedef struct evList{
	 int quad;
	struct evList* next;
}list;

typedef struct forStruct{
	int test;
	int enter;
	char * name;
}forStruct;


typedef enum expr_t {
    var_e,
    tableitem_e,

    programmfunc_e,
    libraryfunc_e,

    arithexpr_e,
    boolexpr_e,
    assignexpr_e,
    newtable_e,

    constint_e,
    constdouble_e,
    constbool_e,
    conststring_e,

    nil_e
}expr_t;

typedef enum iopcode{
	assign, add, sub,
	mul, divv, mod,
	uminus, and, or,
	not, if_eq, if_noteq,
	if_lesseq, if_geatereq, if_less,
	if_greater, jump, call,
	param, ret, getretval,
	funcstart, funcend, tablecreate,
	tablegetelem, tablesetelem,nop
 }iopcode;


 typedef struct expr{
	expr_t type;
    node * sym;
    struct expr* index;
    int intConst;
    double doubleConst;
    char* strConst;
     int boolConst;
    struct expr* next;
	struct expr* value;
	list* truelist;
	list* falselist;
	list* breaklist;
	list* continuelist;
	int passedByNot;
	int passedByPar;
	int isEmitable;
	int isBoolean;
	int backpatch;


} expr;

typedef struct quad{
		 iopcode op;
	     expr* result;
	     expr* arg1;
	     expr* arg2;
	     int label;
	     int line;
			 //int taddress;
}quad ;

struct call{
	expr* elist;
	int method;
	char* name;
};

quad* quads;
		int totalQuads=0;
 		int currQuad=0;
int tempcounter=0;

void expand(){
	quad *newquads=malloc(NEW_SIZE);
    assert(totalQuads==currQuad);
	memcpy(newquads,quads,CURR_SIZE);
	free(quads);
	quads= newquads;
	totalQuads=totalQuads+EXPAND_SIZE;
}


char* copy(char * a,char * b){
	a=malloc(sizeof(strlen(b))+1);
	a=strdup(b);
	return a;
}

void setQuad(int n){
	currQuad=n;
}
void emit(iopcode op,expr* arg1,expr* arg2,expr* result, int label, int line){
    if(currQuad==totalQuads)
       expand();



  	quad *newquad = quads+currQuad++;

	newquad->op=op;
	newquad->result=result;
	newquad->arg1=arg1;
	newquad->arg2=arg2;
	newquad->line=currQuad;
	newquad->label=label;
   // return result;
}
expr* lvalue_expr (node * sym){

	assert(sym);
	expr* e = malloc(sizeof(expr));
	memset(e,0,sizeof(expr));
	e->next=NULL;
	e->sym=sym;
	e->strConst=malloc(sizeof(strlen(sym->name))+1);
	e->truelist=NULL;
	e->falselist=NULL;
	e->breaklist=NULL;
	e->continuelist=NULL;
	e->passedByNot=0;
	e->isEmitable=1;
	e->passedByPar=0;
	e->isBoolean=0;
	e->backpatch=0;
	e->value=NULL;
	e->strConst=strdup(sym->name);

	switch(sym->newType){
		case var_s		   : e->type=var_e; break;
		case programfunc_s : e->type=programmfunc_e; break;
		case libraryfunc_s : e->type=libraryfunc_e; break;
		default			   : break;
	}
	return e;
}





void increasetemp(){tempcounter++;}
void resetformalargoffset(){
	formalArgOffset=0;
}
void resetfunctionlocalsoffset(){
	functionLocalOffset=0;
}
void restorecurrscopeoffset( int n){
	switch(currscopespace()){
		case programvar      :  programVarOffset=n; break;
        case functionlocal   :  functionLocalOffset=n; break;
        case formalarg       :  formalArgOffset=n; break;
        default              : assert(0);
	}
}
 int nextquadlabel(){
	return currQuad;
}

void patchlabel( int quadNo, int label){
	assert(quadNo<currQuad);
	//assert(!quads[quadNo].label);
	quads[quadNo].label=label;
}

expr* newexpr(expr_t t){
	expr* e = malloc(sizeof(expr));
	e->falselist=NULL;
	e->truelist=NULL;
	e->breaklist=NULL;
	e->continuelist=NULL;
	memset(e,0,sizeof(expr));
	e->passedByNot=0;
	e->type=t;
	e->passedByPar=0;
	e->isEmitable=1;
	e->isBoolean=0;
	e->backpatch=0;
	e->value=NULL;
	return e;
}
expr* newexpr_conststring(char * s){
	expr* e = newexpr(conststring_e);
	e->strConst=malloc(sizeof(strlen(s))+1);
	e->strConst=strdup(s);
	return e;
}
expr* newexpr_constdouble(float  a){
	expr* e = newexpr(constdouble_e);
	e->doubleConst=a;
	return e;
}
expr* newexpr_constint(int n){
	expr* e = newexpr(constint_e);
	e->intConst=n;
	return e;
}
expr* newexpr_constbool( int b){
	expr * e=newexpr(constbool_e);
	e->boolConst=b;
	return e;
}

char* newtempvar(){
	int length = snprintf(NULL,0,"%d",tempcounter);
    char* str=malloc(length+1);
    snprintf(str,length+1,"%d",tempcounter);
    char * newtemp=malloc(sizeof(strlen("_t")+strlen(str))+1);
    newtemp=strdup("_t");
	
    strcat(newtemp,str);
	tempcounter++;
    return newtemp;
}
void resetTempVar(){
	tempcounter=0;
}
node *newtemp(){
	node* result;
	node * t;
	char* name=newtempvar();
	result=scopeLookup(currscope(),name);

	if(result==NULL){

		result=malloc(sizeof(node));
		result->isActive=1;
		result->name=name;
		result->next=NULL;
		result->next_scope=NULL;
		result->newType=var_s;
		result->space=currscopespace();
		result->offset=currscopeoffset();
		
		if(!strcmp(scopespaceToStr(currscopespace()),"programvar")){
			t=insert(name,GLOBAL,-1,currscope());
			t->space=programvar;
		}else if(!strcmp(scopespaceToStr(currscopespace()),"formalarg")){
			t=insert(name,FORMAL,-1,currscope());
			t->space=formalarg;
		}else{
			t=insert(name,LOCL,-1,currscope());
			t->space=functionlocal;
		}
		t->offset=currscopeoffset();
		inccurrescopeoffset();

	}

	return result;
}
expr* emit_iftableitem(expr* e,int yylineno){

	if(e==NULL) return e;
	if(e->type!=tableitem_e){
		return e;
	}else{
		expr* result = newexpr(var_e);
		result->sym=newtemp();
		emit(tablegetelem,e,e->index,result,-1,yylineno);
		return result;
	}
}

expr* member_item(expr* lvalue,char* name,int yylineno){
	lvalue=emit_iftableitem(lvalue,yylineno);
	expr* item= newexpr(tableitem_e);
	item->sym=lvalue->sym;
	item->index = newexpr_conststring(name);
	return item;
}
expr* reverseList(expr* head)
{
    struct expr *prevNode, *curNode;

    if(head != NULL)
    {
        prevNode = head;
        curNode = head->next;
        head = head->next;

        prevNode->next = NULL; // Make first node as last node

        while(head != NULL)
        {
            head = head->next;
            curNode->next = prevNode;

            prevNode = curNode;
            curNode = head;
        }

        head = prevNode; // Make last node as head
    }
		return head;
}

expr* make_call(expr* lv,expr* elist,int yylineno){
	expr* temp=elist;
	temp=reverseList(temp);
	expr* func =emit_iftableitem(lv,yylineno);
	while (temp){
			emit(param,temp,NULL,NULL,-1,yylineno);
			temp=temp->next;
	}
	emit(call,func,NULL,NULL,-1,yylineno);
	expr* result=newexpr(var_e);
	result->sym=newtemp();
	emit(getretval,NULL,NULL,result,-1,yylineno);
	return result;

}


list* makelist( int quad,list* head){
	list* tmp=malloc(sizeof(list));
	tmp->quad=quad;
	if(head==NULL){
		tmp->next=NULL;
		head=tmp;
		return head;
	}

	tmp->next=head;
	return tmp;
}
list* merge(list* l1,list* l2){
	if(l1==NULL)
		return l2;
	if(l2==NULL)
		return l1;

	list* tmp=l1,*t=l1;
	while(tmp->next!=NULL){

		tmp=tmp->next;
	}
	tmp->next=l2;


	return l1;
}
void backpatch(list* head,int label){
	list* tmp=head;
//	printf("%p\n",head);
	assert(label<totalQuads);
	while(tmp!=NULL){
	//	printf("%d\n",head->quad);
		quads[tmp->quad].label=label;
		tmp=tmp->next;
	}

}





int check_arithmetic(expr* e){
		if(e->type==constbool_e ||
			 e->type==conststring_e ||
		 	 e->type==nil_e ||
	 	 	 e->type==newtable_e ||
       e->type==programmfunc_e ||
       e->type==libraryfunc_e ||
       e->type==boolexpr_e){
	printf("ERROR:Illegal expr used!");
	return 0;
	}
return 1;
}


void swapLists(list *a,list *b){
	//printf("%p %p\n",a,b);
	
	if(a){
	  list tmp=*a;
	 *a=*b;
	 *b=tmp;
	}
	 
}


FILE *fp = NULL;



void printExpr(expr *e)
{
	if(e){
	switch(e->type){
	  case var_e:
		if(e->sym)
		printf("%s\t", e->sym->name);
		break;
	  case tableitem_e:
		if(e->sym)
		printf("%s\t", e->sym->name);
		break;
	  case programmfunc_e:
		if(e->sym)
		printf("%s\t", e->sym->name);
		break;
	  case libraryfunc_e:
		if(e->sym)
		printf("%s\t", e->sym->name);
		break;
	  case arithexpr_e:
		if(e->sym)
		printf("%s\t", e->sym->name);
		break;
	  case boolexpr_e:
		printf("%s\t", e->boolConst);
		break;
	  case assignexpr_e:
		if(e->sym)
		printf("%s\t", e->sym->name);
		break;
	  case newtable_e:
		if(e->sym)
		printf("%s\t", e->sym->name);
		break;
	  case constint_e:
		printf("%d\t",e->intConst);
		break;
	  case constdouble_e:
		printf("%.2lf\t",e->doubleConst);
		break;
	  case constbool_e:
		if(e->boolConst)
		      printf("TRUE\t");
		else
		      printf("FALSE\t");
		break;
	  case conststring_e:
		printf("%s\t",e->strConst);
		break;
	  case nil_e:
		printf("null\t");
		break;
	  default:
		printf("***\t");
	}
}}
void PrintQuads()
{


	int j=0;
	quad *tmp = quads;

	while(j < currQuad){

		printf("%d:  ",j+1);
		tmp= quads+j++;
		if(tmp){
		switch(tmp->op)
		{
			case assign:
				printf("ASSIGN\t");
				break;
			case add:
				printf("ADD\t");
				break;
			case sub:
				printf("SUB\t");
				break;
			case mul:
				printf("MUL\t");
				break;
			case divv:
				printf("DIV\t");
				break;
			case mod:
				printf("MOD\t");
				break;
			case and:
				printf("AND\t");
				break;
			case or:
				printf("OR\t");
				break;
			case not:
				printf("NOT\t");
				break;
			case jump:
				printf("JUMP\t");
				break;
			case if_eq:
				printf("IF_EQ\t");
				break;
			case if_noteq:
				printf("IF_NOTEQ\t");
				break;
			case if_greater:
				printf("IF_GREATER\t");
				break;
			case if_lesseq:
				printf("IF_LESSEQ\t");
				break;
			case if_geatereq:
				printf("IF_GREATEREQ\t");
				break;
			case if_less:
				printf("IF_LESS\t");
				break;
			case call:
				printf("CALL\t");
				break;
			case getretval:
				printf("GETRETVAL\t");
				break;
			case funcstart:
				printf("FUNCSTART\t");
				break;
			case funcend:
				printf("FUNCEND\t");
				break;
			case tablecreate:
				printf("TABLECREATE\t");
				break;
			case tablegetelem:
				printf("TABLEGETELEM\t");
				break;
			case tablesetelem:
				printf("TABLESETELEM\t");
				break;
			case ret:
				printf("RETURN\t");
				break;
			case uminus:
				printf("UMINUS\t");
				break;
			case param:
				printf("PARAM\t");
				break;
			default:
				printf("***\t");
		}}
		if(tmp->result)
		      printExpr(tmp->result);
		if(tmp->arg1)
		      printExpr(tmp->arg1);
		if(tmp->arg2)
		      printExpr(tmp->arg2);

		if(tmp->label != -1)
		      printf("%d",tmp->label+1);
		printf("\n");
	}
}
void filePrintExpr(FILE*fp,expr *e)
{
	if(e){
	switch(e->type){
	  case var_e:
		if(e->sym)
		fprintf(fp,"%s\t", e->sym->name);
		break;
	  case tableitem_e:
		if(e->sym)
		fprintf(fp,"%s\t", e->sym->name);
		break;
	  case programmfunc_e:
		if(e->sym)
		fprintf(fp,"%s\t", e->sym->name);
		break;
	  case libraryfunc_e:
		if(e->sym)
		fprintf(fp,"%s\t", e->sym->name);
		break;
	  case arithexpr_e:
		if(e->sym)
		fprintf(fp,"%s\t", e->sym->name);
		break;
	  case boolexpr_e:
		fprintf(fp,"%s\t", e->boolConst);
		break;
	  case assignexpr_e:
		if(e->sym)
		fprintf(fp,"%s\t", e->sym->name);
		break;
	  case newtable_e:
		if(e->sym)
		fprintf(fp,"%s\t", e->sym->name);
		break;
	  case constint_e:
		fprintf(fp,"%d\t",e->intConst);
		break;
	  case constdouble_e:
		fprintf(fp,"%.2lf\t",e->doubleConst);
		break;
	  case constbool_e:
		if(e->boolConst)
		      fprintf(fp,"TRUE\t");
		else
		      fprintf(fp,"FALSE\t");
		break;
	  case conststring_e:
		fprintf(fp,"%s\t",e->strConst);
		break;
	  case nil_e:
		fprintf(fp,"null\t");
		break;
	  default:
		fprintf(fp,"***\t");
	}
}}
void filePrintQuads()
{
	FILE *fp = fopen("Quads.txt" ,"w");
	int j=0;
	quad *tmp = quads;

	while(j < currQuad){

		fprintf(fp,"%d:  ",j+1);
		tmp= quads+j++;
		if(tmp){
		switch(tmp->op)
		{
			case assign:
				fprintf(fp,"ASSIGN\t");
				break;
			case add:
				fprintf(fp,"ADD\t");
				break;
			case sub:
				fprintf(fp,"SUB\t");
				break;
			case mul:
				fprintf(fp,"MUL\t");
				break;
			case divv:
				fprintf(fp,"DIV\t");
				break;
			case mod:
				fprintf(fp,"MOD\t");
				break;
			case and:
				fprintf(fp,"AND\t");
				break;
			case or:
				fprintf(fp,"OR\t");
				break;
			case not:
				fprintf(fp,"NOT\t");
				break;
			case jump:
				fprintf(fp,"JUMP\t");
				break;
			case if_eq:
				fprintf(fp,"IF_EQ\t");
				break;
			case if_noteq:
				fprintf(fp,"IF_NOTEQ\t");
				break;
			case if_greater:
				fprintf(fp,"IF_GREATER\t");
				break;
			case if_lesseq:
				fprintf(fp,"IF_LESSEQ\t");
				break;
			case if_geatereq:
				fprintf(fp,"IF_GREATEREQ\t");
				break;
			case if_less:
				fprintf(fp,"IF_LESS\t");
				break;

			case call:
				fprintf(fp,"CALL\t");
				break;
			case getretval:
				fprintf(fp,"GETRETVAL\t");
				break;
			case funcstart:
				fprintf(fp,"FUNCSTART\t");
				break;
			case funcend:
				fprintf(fp,"FUNCEND\t");
				break;
			case tablecreate:
				fprintf(fp,"TABLECREATE\t");
				break;
			case tablegetelem:
				fprintf(fp,"TABLEGETELEM\t");
				break;
			case tablesetelem:
				fprintf(fp,"TABLESETELEM\t");
				break;
			case ret:
				fprintf(fp,"RETURN\t");
				break;
			case uminus:
				fprintf(fp,"UMINUS\t");
				break;
			case param:
				fprintf(fp,"PARAM\t");
				break;
			default:
				fprintf(fp,"***\t");
		}}
		if(tmp->result)
		      filePrintExpr(fp,tmp->result);
		if(tmp->arg1)
		      filePrintExpr(fp,tmp->arg1);
		if(tmp->arg2)
		      filePrintExpr(fp,tmp->arg2);

		if(tmp->label != -1)
		      fprintf(fp,"%d",tmp->label+1);
		fprintf(fp,"\n");
	}
	fclose(fp);
}
