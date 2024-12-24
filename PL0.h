#include <stdio.h>

#define NRW        20     // number of reserved words
#define TXMAX      500    // length of identifier table
#define MAXNUMLEN  14     // maximum number of digits in numbers
#define NSYM       17     // maximum number of symbols in array ssym and csym
#define MAXIDLEN   10     // length of identifiers

#define MAXADDRESS 32767  // maximum address
#define MAXLEVEL   32     // maximum depth of nesting block
#define CXMAX      500    // size of code array

#define MAXSYM     46     // maximum number of symbols  

#define STACKSIZE  1000   // maximum storage

#define MAXDIM 10	// maximum dimension of array

enum symtype
{
	SYM_NULL,
	SYM_IDENTIFIER,
	SYM_NUMBER,
	SYM_PLUS,
	SYM_MINUS,
	SYM_TIMES,
	SYM_SLASH,
	SYM_ODD,
	SYM_EQU,
	SYM_NEQ,
	SYM_LES,
	SYM_LEQ,
	SYM_GTR,
	SYM_GEQ,
	SYM_LPAREN,
	SYM_RPAREN,
	SYM_COMMA,
	SYM_SEMICOLON,
	SYM_PERIOD,
	SYM_BECOMES,
    SYM_BEGIN,
	SYM_END,
	SYM_IF,
	SYM_THEN,
	SYM_WHILE,
	SYM_DO,
	SYM_CALL,
	SYM_CONST,
	SYM_VAR,
	SYM_PROCEDURE,
	SYM_AND,
	SYM_OR,
	SYM_NOT,
	SYM_SWITCH,
	SYM_CASE,
	SYM_DEFAULT,
	SYM_ENDSWITCH,
	SYM_LBRACKET,
	SYM_RBRACKET,
	SYM_LBRACE,
	SYM_RBRACE,
	SYM_RETURN,
	SYM_ELSE,
	SYM_ELIF,
	SYM_FOR,
	SYM_EXIT
};

enum idtype
{
	ID_CONSTANT, ID_VARIABLE, ID_PROCEDURE, ID_ARRAY
};

enum opcode
{
	LIT, OPR, LOD, STO, CAL, INT, JMP, JPC, JPC1, JPC2,
	CPY, LEA, STA, LDA, EXIT, PAS
};

enum oprcode
{
	OPR_RET, OPR_NEG, OPR_ADD, OPR_MIN,
	OPR_MUL, OPR_DIV, OPR_ODD, OPR_EQU,
	OPR_NEQ, OPR_LES, OPR_LEQ, OPR_GTR,
	OPR_GEQ, OPR_AND, OPR_OR, OPR_NOT,
	OPR_RET_VAL
};


typedef struct
{
	int f; // function code
	int l; // level
	int a; // displacement address
} instruction;

//////////////////////////////////////////////////////////////////////
char* err_msg[] =
{
/*  0 */    "",
/*  1 */    "Found ':=' when expecting '='.",
/*  2 */    "There must be a number to follow '='.",
/*  3 */    "There must be an '=' to follow the identifier.",
/*  4 */    "There must be an identifier to follow 'const', 'var', or 'procedure'.",
/*  5 */    "Missing ',' or ';'.",
/*  6 */    "Incorrect procedure name.",
/*  7 */    "Statement expected.",
/*  8 */    "Follow the statement is an incorrect symbol.",
/*  9 */    "'.' expected.",
/* 10 */    "';' expected.",
/* 11 */    "Undeclared identifier.",
/* 12 */    "Illegal assignment.",
/* 13 */    "':=' expected.",
/* 14 */    "There must be an identifier to follow the 'call'.",
/* 15 */    "A constant or variable can not be called.",
/* 16 */    "'then' expected.",
/* 17 */    "';' or 'end' expected.",
/* 18 */    "'do' expected.",
/* 19 */    "Incorrect symbol.",
/* 20 */    "Relative operators expected.",
/* 21 */    "Procedure identifier can not be in an expression.",
/* 22 */    "Missing ')'.",
/* 23 */    "The symbol can not be followed by a factor.",
/* 24 */    "The symbol can not be as the beginning of an expression.",
/* 25 */    "The number is too great.",
/* 26 */    "'while' execpted",
/* 27 */    "Parameter count mismatch.",
/* 28 */    "",
/* 29 */    "",
/* 30 */    "",
/* 31 */    "Missing 'endswitch'",
/* 32 */    "There are too many levels.",
/* 33 */    "There are too many dimensions in the array.",
/* 34 */    "Array dimension must be greater than zero.",
/* 35 */	"Missing ']'.",
/* 36 */	"Missing array size.",
/* 37 */	"Missing dimension in array element assignment."
};

//////////////////////////////////////////////////////////////////////
char ch;         // last character read
int  sym;        // last symbol read
char id[MAXIDLEN + 1]; // last identifier read
int  num;        // last number read
int  cc;         // character count
int  ll;         // line length
int  kk;
int  err;
int  cx;         // index of current instruction to be generated.
int  level = 0;
int  tx = 0;
struct array_info* pa; // pointer to the last array read
int  arr_tx = 0; // current number of symbols in the array information table

char line[80];

instruction code[CXMAX];

char* word[NRW + 1] =
{
	"", /* place holder */
	"begin", "call", "const", "do", "end","if",
	"odd", "procedure", "then", "var", "while", 
	"switch", "case", "default", "endswitch",
	"elif", "for", "else", "return", "exit"
};

int wsym[NRW + 1] =
{
	SYM_NULL, SYM_BEGIN, SYM_CALL, SYM_CONST, SYM_DO, SYM_END,
	SYM_IF, SYM_ODD, SYM_PROCEDURE, SYM_THEN, SYM_VAR, SYM_WHILE,
	SYM_SWITCH, SYM_CASE, SYM_DEFAULT, SYM_ENDSWITCH,SYM_ELIF, 
	SYM_FOR, SYM_ELSE,SYM_RETURN, SYM_EXIT
};

int ssym[NSYM + 1] =
{
	SYM_NULL, SYM_PLUS, SYM_MINUS, SYM_TIMES, SYM_SLASH,
	SYM_LPAREN, SYM_RPAREN, SYM_EQU, SYM_COMMA, SYM_PERIOD, SYM_SEMICOLON,
	SYM_AND, SYM_OR, SYM_NOT, SYM_LBRACKET, SYM_RBRACKET, SYM_LBRACE, SYM_RBRACE
};

char csym[NSYM + 1] =
{
	' ', '+', '-', '*', '/', '(', ')', '=', ',', '.', ';',
	'&', '|', '!', '[', ']', '{', '}'
};

#define MAXINS   16
char* mnemonic[MAXINS] =
{
	"LIT", "OPR", "LOD", "STO", "CAL", "INT", "JMP", "JPC" , "JPC1", "JPC2",
	"CPY", "LEA", "STA", "LDA", "EXIT", "PAS"
};

typedef struct
{
	char name[MAXIDLEN + 1];
	int  kind;
	int  value;
	int paramCount; // 新增：参数个数
    int paramTypes[MAXIDLEN]; // 新增：参数类型
} comtab;

comtab table[TXMAX];

typedef struct
{
	char  name[MAXIDLEN + 1];
	int   kind;
	short level;
	short address;
	int paramCount; // 新增：参数个数
    int paramTypes[MAXIDLEN]; // 新增：参数类型
} mask;

typedef struct array_info	// array information
{
	int address;
	int size;	//total size?
	int dim;	//total dimension
	int dim_size[MAXDIM+1];
} array_info;

array_info array_table[TXMAX];	// array information table

FILE* infile;

