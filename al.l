%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "parser.h"

/*#define YY_DECL int alpha_yylex (void* yylval)*/
int lines=0;
int tokens=0;
int string_list_size=0;


struct node1{

    char data;
    struct node1 *link;
}*top = NULL;

char pop(){
    char value='1';
    struct node1 *temp=NULL;
    if(top!=NULL){
        temp = top;
        value=temp->data;
        top = top->link;
    }
    return value;
}

int isEmpty(){

    if (top == NULL)
        return 1;
    else
        return 0;
}

void push(char val){

    struct node1 *temp;
    temp = (struct node1*)malloc(sizeof(struct node1));
    temp->data = val;
    temp->link = top;
    top = temp;
}

struct stringList{
    char stringChar;
    struct stringList *next;
};
struct stringList* string_head=NULL;

struct stringList* newStringNode(char stringChar){
    string_list_size++;
    struct stringList *newNode=malloc(sizeof(struct stringList));
    newNode->stringChar=stringChar;
    newNode->next=NULL;
    return newNode;
}

void insert_char(char stringChar){
    if(string_head==NULL){
       string_head=newStringNode(stringChar);
    }
    else{
        struct stringList* tmp_head=string_head;
        while(tmp_head->next!=NULL)
            tmp_head=tmp_head->next;
            tmp_head->next=newStringNode(stringChar);
    }
}

struct tokenList{
    int line_number;
    int token_number;
    char* token_string;
    char* tokenType;
    char* tokenSubType;
    struct tokenList *next;
};
struct tokenList* tokens_head=NULL;

struct tokenList* newNode(int lineNumber,char* tokenString,char* tokenType,char* tokenSubType){
    struct tokenList *newNode=malloc(sizeof(struct tokenList));
    newNode->line_number=lineNumber;
    newNode->token_number=++tokens;

    newNode->token_string=malloc(sizeof(strlen(tokenString))+1);
    newNode->token_string=strdup(tokenString);
    newNode->tokenType=malloc(sizeof(strlen(tokenType))+1);
    newNode->tokenType=strdup(tokenType);
    newNode->tokenSubType=malloc(sizeof(strlen(tokenSubType))+1);
    newNode->tokenSubType=strdup(tokenSubType);
    newNode->next=NULL;
    return newNode;
}

void insert_token(int lineNumber,char *tokenType,char* tokenString,char* tokenSubType){
    if(tokens_head==NULL){
       tokens_head=newNode(lineNumber,tokenString,tokenType,tokenSubType);
    }
    else{
        struct tokenList* tmp_head=tokens_head;
        while(tmp_head->next!=NULL)
            tmp_head=tmp_head->next;
        tmp_head->next=newNode(lineNumber,tokenString,tokenType,tokenSubType);
    }
    return;
}

void print(){
    struct tokenList* tmp_head=tokens_head;
    int i=0;
    while(tmp_head!=NULL){
        printf("%d: %d# \"%s\" %s\t%s\n",tmp_head->line_number,tmp_head->token_number,tmp_head->token_string,tmp_head->tokenType,tmp_head->tokenSubType);
        tmp_head=tmp_head->next;
    }
}


%}

              /* ---tokens--- */
              /* keywords */
IF 		       "if"
ELSE		     "else"
WHILE 		   "while"
FOR 	       "for"
FUNCTION	   "function"
RETURN 		   "return"
BREAK		     "break"
CONTINUE	   "continue"

LOCAL		     "local"
TRUE		     "true"
FALSE		     "false"
NIL		       "nil"

NOT		       "not"
AND		       "and"
OR		       "or"




/* operators */
ASSIGNMENT		"="
PLUS		      "+"
MINUS	 	      "-"
MULT		      "*"
DIV		        "/"
MODULO	      "%"
EQUAL		      "=="
NOT_EQUAL		  "!="
INCREMENT		  "++"
DECREMENT		"--"
GREATER		    ">"
LESS		      "<"
GREATER_EQUAL	">="
LESS_EQUAL	  "<="


/* arithmetic constants */
NUMBER		   [0-9]+
REAL_NUMBER  [0-9]+"."[0-9]+

/* string */
STRING \"

/* punctuation */
LEFT_PARENTHESIS \(
RIGHT_PARENTHESIS \)
LEFT_SQUARE_BRACKET \[
RIGHT_SQUARE_BRACKET \]
LEFT_CURLY_BRACKET \{
RIGHT_CURLY_BRACKET \}
DOT \.
DOUBLE_DOT \.\.
COMMA \,
SEMICOLON \;
COLON \:
DOUBLE_COLONS \:\:



/* identifier */
ID		       [a-zA-Z][a-zA-Z_0-9]*



/* comments */
ONE_LINE_COMMENT "//".*
MULTI_LINE_COMMENT "/*"
MULTI_LINE_COMMENT_END "*/"



/* others*/
SPACES [\t' '\r]*
NEW_LINE "\n"
OTHER		.




%option noyywrap
%option yylineno
%%



{IF} 		       {return IF;}
{ELSE}		     {return ELSE;}
{WHILE} 		   {return WHILE;}
{FOR}		       {return FOR;}
{FUNCTION}	   {return FUNCTION;}
{RETURN} 		   {return RETURN;}
{BREAK}		     {return BREAK;}
{CONTINUE}	   {return CONTINUE;}
{AND}		       {return AND;}
{NOT}		       {return NOT;}
{OR}		       {return OR;}
{LOCAL}		     {return LOCAL;}
{TRUE}		     {return TRUE;}
{FALSE}		     {return FALSE;}
{NIL}		       {return NIL;}



{ASSIGNMENT}		{return ASSIGNMENT;}
{PLUS}		      {return PLUS;}
{MINUS}	 	      {return MINUS;}
{MULT}		      {return MULT;}
{DIV}		        {return DIV;}
{MODULO}	      {return MODULO;}
{EQUAL}		      {return EQUAL;}
{NOT_EQUAL}		  {return NOT_EQUAL;}
{INCREMENT}		  {return INCREMENT;}
{DECREMENT}		  {return DECREMENT;}
{GREATER}		    {return GREATER;}
{LESS}		      {return LESS;}
{GREATER_EQUAL}	{return GREATER_EQUAL;}
{LESS_EQUAL}	  {return LESS_EQUAL;}





{LEFT_PARENTHESIS}      {return LEFT_PARENTHESIS;}
{RIGHT_PARENTHESIS}     {return RIGHT_PARENTHESIS;}
{LEFT_SQUARE_BRACKET}   {return LEFT_SQUARE_BRACKET;}
{RIGHT_SQUARE_BRACKET}  {return RIGHT_SQUARE_BRACKET;}
{LEFT_CURLY_BRACKET}    {return LEFT_CURLY_BRACKET;}
{RIGHT_CURLY_BRACKET}   {return RIGHT_CURLY_BRACKET;}
{DOT}                   {return DOT;}
{DOUBLE_DOT}            {return DOUBLE_DOT;}
{COMMA}                 {return COMMA;}
{SEMICOLON}             {return SEMICOLON;}
{COLON}                 {return COLON;}
{DOUBLE_COLONS}         {return DOUBLE_COLONS;}


{ID}                    {yylval.stringValue=malloc(sizeof(strlen(yytext))+1);yylval.stringValue=strdup(yytext);return ID;}
{ONE_LINE_COMMENT}		  {}
{MULTI_LINE_COMMENT}    {
                 push('*');
                 int i=0;
                  char token;
                  while((token=input())!=EOF){
                    if(token=='/'){
                      token=input();
                      if(token=='*'){
                        push(token);
                      }else {
                      unput(token);
                      }

                      }else if(token=='*'){
                        token=input();
                        if(token=='/'){
                          if(pop()=='*')
                            //if(!isEmpty())
                            //else
                          i=-1;
                        }else{
                            unput(token);
                              }
                      }
                      if(isEmpty()) break;
                  }
                  while(!isEmpty()){
                   if(pop()=='*'){
                      //if(!isEmpty())
                    //  else
                   }
                }
}



{STRING}      {
            char c;
            int flag = 1;
            //insert_char('\0');
                  while((c=input())!=EOF)
            {

              while (c == '\\'){/*auth h while voleuei gia ta sunexomena slash
                          vlepei pote uparxei slash kai analoga vazei sto string
                  ton katallhlo escape character h' petaei warning */

                    c = input();
                    if ( c == 'n')  insert_char('\n');
                    else if ( c == 't' )insert_char('\t');
                    else if ( c == '\\' )insert_char('\\');
                    else if ( c == '\"' )insert_char('"');
                    /*otan vrei kati pou den to anagnwrizei dinei keno xarakthra sto string*/
                    else{
                        insert_char(' ');
                        //printf("WARNING, Unknown character found! a space character was added.\n");
                    }

            // insert_char('\0');
              c = input();
            }

            if (c == '\"'){			/* to string eftase sto telos tou*/
              flag = 0;				/*mhdemismos ths error flag gia na mhn petaksei la8os*/
              char* newString=malloc(string_list_size*sizeof(char)+1);
              struct stringList* temp_string_head=string_head;
              int i=0;
              for(i=0;i<string_list_size;i++){

                  newString[i]=temp_string_head->stringChar;
                  temp_string_head=temp_string_head->next;
              }
              insert_char('\0');



              yylval.stringValue =malloc(sizeof(strlen(newString))+1);yylval.stringValue=strdup(newString);
              string_list_size=0;
              string_head=NULL;
              return STRING;
            }
            else 	/*alliws sunexizri na vazei xarakthres*/
            {
              insert_char(c);
              //insert_char('\0');
            }
            }
            /*an h error flag den mhdenistei pote tote exoume ftasei sto EOF
                              xwris na kleisei to string opote exoume error			*/

}

{NUMBER}        {yylval.intValue = atoi(yytext);return NUMBER;}
{REAL_NUMBER}   {yylval.realValue= atof(yytext);return REAL_NUMBER;}
{SPACES}        {}
{NEW_LINE}      {lines++;}
{OTHER}			    {return OTHER;}




%%
