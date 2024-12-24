// pl0 compiler source code

#pragma warning(disable:4996)


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "PL0.h"
#include "set.c"

//////////////////////////////////////////////////////////////////////
// print error message.
void error(int n)
{
	int i;

	printf("      ");
	for (i = 1; i <= cc - 1; i++)
		printf(" ");
	printf("^\n");
	printf("Error %3d: %s\n", n, err_msg[n]);
	err++;
} // error

//////////////////////////////////////////////////////////////////////
void getch(void)
{
	if (cc == ll)
	{
		if (feof(infile))
		{
			printf("\nPROGRAM INCOMPLETE\n");
			exit(1);
		}
		ll = cc = 0;
		printf("%5d  ", cx);
		while ( (!feof(infile)) // added & modified by alex 01-02-09
			    && ((ch = getc(infile)) != '\n'))
		{
			printf("%c", ch);
			line[++ll] = ch;
		} // while
		printf("\n");
		line[++ll] = ' ';
	}
	ch = line[++cc];
} // getch

//////////////////////////////////////////////////////////////////////
// gets a symbol from input stream.
void getsym(void)
{
	int i, k;
	char a[MAXIDLEN + 1];

	// while (ch == ' '||ch == '\t')
	// 	getch();

	while (ch == ' '||ch == '\t'||ch == '/'){
		if(ch == '/'){
            char tch=ch;
            getch();
            if(ch == '*'){
                getch();
                while(1){
                    getch();
                    if(ch=='*'){
                        getch();
                        if(ch=='/')
                            break;
                    }
                }
                getch();
            }
            else if(ch == '/'){
                while(cc!=ll)
                    getch();
            }
            else
			{
				ch=tch;
				cc --;
				break;
			}

        }
        else
            getch();
	}
	if (isalpha(ch))
	{ // symbol is a reserved word or an identifier.
		k = 0;
		do
		{
			if (k < MAXIDLEN)
				a[k++] = ch;
			getch();
		}
		while (isalpha(ch) || isdigit(ch));
		a[k] = 0;
		strcpy(id, a);
		word[0] = id;
		i = NRW;
		while (strcmp(id, word[i--]));
		if (++i)
			sym = wsym[i]; // symbol is a reserved word
		else
			sym = SYM_IDENTIFIER;   // symbol is an identifier
	}
	else if (isdigit(ch))
	{ // symbol is a number.
		k = num = 0;
		sym = SYM_NUMBER;
		do
		{
			num = num * 10 + ch - '0';
			k++;
			getch();
		}
		while (isdigit(ch));
		if (k > MAXNUMLEN)
			error(25);     // The number is too great.
	}
	else if (ch == ':')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_BECOMES; // :=
			getch();
		}
		else
		{
			sym = SYM_NULL;       // illegal?
		}
	}
	else if (ch == '>')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_GEQ;     // >=
			getch();
		}
		else
		{
			sym = SYM_GTR;     // >
		}
	}
	else if (ch == '<')
	{
		getch();
		if (ch == '=')
		{
			sym = SYM_LEQ;     // <=
			getch();
		}
		else if (ch == '>')
		{
			sym = SYM_NEQ;     // <>
			getch();
		}
		else
		{
			sym = SYM_LES;     // <
		}
	}
	else if (ch == '&')
	{
		getch();
		if(ch == '&')
		{
			sym = SYM_AND;
			getch();
		}
	}
	else if (ch == '|')
	{
		getch();
		if(ch == '|')
		{
			sym = SYM_OR;
			getch();
		}
	}
	else if (ch == '!')
	{
		sym = SYM_NOT;
		getch();
	}
	else
	{ // other tokens
		i = NSYM;
		csym[0] = ch;
		while (csym[i--] != ch);
		if (++i)
		{
			sym = ssym[i];
			getch();
		}
		else
		{
			printf("Fatal Error: Unknown character.\n");
			exit(1);
		}
	}
} // getsym

//////////////////////////////////////////////////////////////////////
// generates (assembles) an instruction.
void gen(int x, int y, int z)
{
	if (cx > CXMAX)
	{
		printf("Fatal Error: Program too long.\n");
		exit(1);
	}
	code[cx].f = x;
	code[cx].l = y;
	code[cx++].a = z;
} // gen

//////////////////////////////////////////////////////////////////////
// tests if error occurs and skips all symbols that do not belongs to s1 or s2.
void test(symset s1, symset s2, int n)
{
	symset s;

	if (! inset(sym, s1))
	{
		error(n);
		s = uniteset(s1, s2);
		while(! inset(sym, s))
			getsym();
		destroyset(s);
	}
} // test

//////////////////////////////////////////////////////////////////////
int dx;  // data allocation index

// enter object(constant, variable or procedre) into table.
void enter(int kind)
{
	mask* mk;

	tx++;
	strcpy(table[tx].name, id);
	table[tx].kind = kind;
	switch (kind)
	{
	case ID_CONSTANT:
		if (num > MAXADDRESS)
		{
			error(25); // The number is too great.
			num = 0;
		}
		table[tx].value = num;
		break;
	case ID_VARIABLE:
		mk = (mask*) &table[tx];
		mk->level = level;
		mk->address = dx++;
		break;
	case ID_PROCEDURE:
		mk = (mask*) &table[tx];
		mk->level = level;
		break;
	case ID_ARRAY:
		mk = (mask*) &table[tx];
		mk->level = level;
		mk->address = arr_tx;
		pa = &(array_table[arr_tx]);
		array_table[arr_tx].dim = 0;
		array_table[arr_tx].size = 1;
		array_table[arr_tx].dim_size[1] = 0;
		arr_tx++;
		//dx change until all dimensions analyzed
		break;
	} // switch
} // enter

//////////////////////////////////////////////////////////////////////
// locates identifier in symbol table.
int position(char* id)
{
	int i;
	strcpy(table[0].name, id);
	i = tx + 1;
	while (strcmp(table[--i].name, id) != 0);
	return i;
} // position

//////////////////////////////////////////////////////////////////////
void constdeclaration()
{
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if (sym == SYM_EQU || sym == SYM_BECOMES)
		{
			if (sym == SYM_BECOMES)
				error(1); // Found ':=' when expecting '='.
			getsym();
			if (sym == SYM_NUMBER)
			{
				enter(ID_CONSTANT);
				getsym();
			}
			else
			{
				error(2); // There must be a number to follow '='.
			}
		}
		else
		{
			error(3); // There must be an '=' to follow the identifier.
		}
	} else	error(4);
	 // There must be an identifier to follow 'const', 'var', or 'procedure'.
} // constdeclaration

//////////////////////////////////////////////////////////////////////
void dimdeclaration()
{
	int i;
	if(sym == SYM_LBRACKET){
		getsym();
		switch (sym)
		{
			case SYM_NUMBER:	//number as dimension size
				//store dimension size and increase dimension count
				if(pa->dim == MAXDIM)
					error(27);	//There are too many dimensions in the array.
				if(num <= 0)
					error(34);	//Array dimension must be greater than 0.
				pa->dim ++;
				pa->dim_size[pa->dim] = num;
				pa->size *= num;
				getsym();
				if(sym == SYM_RBRACKET){
					getsym();
					dimdeclaration();
				}
				else
					error(35);	//Missing ']'.
				break;
			case SYM_IDENTIFIER:	//identifier as dimension size
				//store dimension size and increase dimension count
				if(pa->dim == MAXDIM)
					error(27);	//There are too many dimensions in the array.
				i = position(id);
				if(i == 0)
					error(11);	//Undeclared identifier.
				if(table[i].kind != ID_CONSTANT)
					error(12);	//Illegal assignment.
				if(table[i].value <= 0)
					error(34);	//Array dimension must be greater than 0.
				pa->dim ++;
				pa->dim_size[pa->dim] = table[i].value;
				pa->size *= table[i].value;
				getsym();
				if(sym == SYM_RBRACKET){
					getsym();
					dimdeclaration();
				}
				else
					error(35);	//Missing ']'.
				break;
			default:
				//missing array size
				error(36);
		}
	}
	else {
		//allocate space for array
		if(pa->dim_size[1]){
			pa->address = dx;
			dx += pa->size;
		}
	}
} // dimdeclaration

//////////////////////////////////////////////////////////////////////
void vardeclaration(void)
{
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if(sym == SYM_LBRACKET)	// array
		{
			enter(ID_ARRAY);
			dimdeclaration();
		}
		else	// not array
			enter(ID_VARIABLE);
	}
	else
	{
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
	}
} // vardeclaration

//////////////////////////////////////////////////////////////////////
void listcode(int from, int to)
{
	int i;

	printf("\n");
	for (i = from; i < to; i++)
	{
		printf("%5d %s\t%d\t%d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
	}
	printf("\n");
} // listcode

//////////////////////////////////////////////////////////////////////
void array_visit(int arr_index, int dim_index, symset fsys);

void factor(symset fsys)
{
	void expression(symset fsys);
	int i;
	int arr_index;
	symset set;

	test(facbegsys, fsys, 24); // The symbol can not be as the beginning of an expression.

	if (inset(sym, facbegsys))
	{
		if (sym == SYM_IDENTIFIER)
		{
			if ((i = position(id)) == 0)
			{
				error(11); // Undeclared identifier.
			}
			else
			{
				switch (table[i].kind)
				{
					mask* mk;
				case ID_CONSTANT:
					gen(LIT, 0, table[i].value);
					getsym();
					break;
				case ID_VARIABLE:
					mk = (mask*) &table[i];
					gen(LOD, level - mk->level, mk->address);
					getsym();
					break;
				case ID_PROCEDURE:
					call(i, fsys);
					break;
				case ID_ARRAY:
					mk = (mask*) &table[i];
					arr_index = mk->address;
					gen(LEA, level - mk->level, array_table[arr_index].address);

					set = createset(SYM_RBRACKET, SYM_NULL);
					array_visit(arr_index, 1, set);

					gen(OPR, 0, OPR_ADD);

					gen(LDA, 0, 0);	//use LDA to load array element to top
				} // switch
			}
		}
		else if (sym == SYM_NUMBER)
		{
			if (num > MAXADDRESS)
			{
				error(25); // The number is too great.
				num = 0;
			}
			gen(LIT, 0, num);
			getsym();
		}
		else if (sym == SYM_LPAREN)
		{
			getsym();
			set = uniteset(createset(SYM_RPAREN, SYM_NULL), fsys);
			expression(set);
			destroyset(set);
			if (sym == SYM_RPAREN)
			{
				getsym();
			}
			else
			{
				error(22); // Missing ')'.
			}
		}
		else if(sym == SYM_MINUS) // UMINUS,  Expr -> '-' Expr
		{
			getsym();
			factor(fsys);
			gen(OPR, 0, OPR_NEG);
		}
		// else if(sym == SYM_NOT) // UMINUS,  Expr -> '!' Expr
		// {
		// 	getsym();
		// 	factor(fsys);
		// 	gen(OPR, 0, OPR_NOT);
		// }
		test(fsys, createset(SYM_LPAREN, SYM_NULL), 23);
	} // if
} // factor

//////////////////////////////////////////////////////////////////////
void term(symset fsys)
{
	int mulop;
	symset set;

	set = uniteset(fsys, createset(SYM_TIMES, SYM_SLASH, SYM_NULL));
	factor(set);
	while (sym == SYM_TIMES || sym == SYM_SLASH)
	{
		mulop = sym;
		getsym();
		factor(set);
		if (mulop == SYM_TIMES)
		{
			gen(OPR, 0, OPR_MUL);
		}
		else
		{
			gen(OPR, 0, OPR_DIV);
		}
	} // while
	destroyset(set);
} // term

//////////////////////////////////////////////////////////////////////
void expression(symset fsys)
{
	int addop;
	symset set;

	set = uniteset(fsys, createset( SYM_PLUS, SYM_MINUS, SYM_AND, SYM_OR, SYM_EQU, SYM_GEQ, SYM_GTR, SYM_LEQ, SYM_LES, SYM_NOT, SYM_NULL));


	term(set);
	while (sym == SYM_PLUS || sym == SYM_MINUS )
	{
		addop = sym;
		getsym();
		term(set);
		if (addop == SYM_PLUS)
		{
			gen(OPR, 0, OPR_ADD);
		}
		else
		{
			gen(OPR, 0, OPR_MIN);
		}
	} // while

	destroyset(set);
} // expression

//////////////////////////////////////////////////////////////////////
void condition(symset fsys)
{
	int relop;
	symset set;

	if (sym == SYM_ODD)
	{
		getsym();
		expression(fsys);
		gen(OPR, 0, 6);
	}
	else if(sym == SYM_NOT)
	{
		getsym();
		expression(fsys);
		gen(OPR, 0, OPR_NOT);
		condition(fsys);
	}
	else
	{
		set = uniteset(relset, fsys);
		expression(set);
		destroyset(set);
		if (! inset(sym, relset))
		{
			error(20);
		}
		else
		{
			while(sym != SYM_THEN && sym != SYM_DO && sym != SYM_SEMICOLON)
			{
				relop = sym;
				// getsym();
				// expression(fsys);
				int cx1,cx2;
				switch (relop)
				{
				case SYM_EQU:
					getsym();
					expression(fsys);
					gen(OPR, 0, OPR_EQU);
					break;
				case SYM_NEQ:
					getsym();
					expression(fsys);
					gen(OPR, 0, OPR_NEQ);
					break;
				case SYM_LES:
					getsym();
					expression(fsys);
					gen(OPR, 0, OPR_LES);
					break;
				case SYM_GEQ:
					getsym();
					expression(fsys);
					gen(OPR, 0, OPR_GEQ);
					break;
				case SYM_GTR:
					getsym();
					expression(fsys);
					gen(OPR, 0, OPR_GTR);
					break;
				case SYM_LEQ:
					getsym();
					expression(fsys);
					gen(OPR, 0, OPR_LEQ);
					break;
				case SYM_AND:
					cx1 = cx;
					gen(JPC1, 0, 0);
					getsym();
					if(sym == SYM_NOT)
					{
						getsym();
						expression(fsys);
						gen(OPR, 0, OPR_NOT);
					}
					else
						expression(fsys);
					gen(OPR, 0, OPR_AND);
					code[cx1].a = cx;
					break;
				case SYM_OR:
					cx2 = cx;
					gen(JPC2, 0, 0);
					getsym();
					condition(fsys);
					gen(OPR, 0, OPR_OR);
					code[cx2].a = cx;
					break;
				} // switch
			}//while
		} // else
	} // else
} // condition

void call(int i, symset fsys)
{
	getsym();
	mask* mk = (mask*)&table[i];
	// printf("mk->address: %d\n", mk->address);
	// printf("table[i].: %s\n", table[i].name);
	int paramCount = 0;
	// printf("sym: %d\n", sym);
	if (sym == SYM_LPAREN) {
		getsym();
		while (sym == SYM_IDENTIFIER || sym == SYM_NUMBER) {
			// printf("sym:before %d\n", sym);
			paramCount++;
			// printf("paramCount: %d\n", paramCount);
			symset set = createset(SYM_COMMA, SYM_RPAREN, SYM_NULL);
			expression(set);
			destroyset(set);
			if (sym == SYM_COMMA) {
				getsym();
			}
			// printf("sym:after %d\n", sym);
		}
		if (sym == SYM_RPAREN) {
			getsym();
		} else {
			// printf("sym: %d\n", sym);
			error(22); // Missing ')'.
		}
	}
	if (paramCount != mk->paramCount) {
		// printf("paramCount: %d, mk->paramCount: %d\n", paramCount, mk->paramCount);
		error(27); // Parameter count mismatch.
	}

	gen(PAS, 0, paramCount);
	gen(CAL, level - mk->level, mk->address);

	// printf("mk0=->name: %s\n", mk->name);
	// printf("mk->address: %d\n", mk->address);

} // call

void array_visit(int arr_index, int dim_index, symset fsys)
{
	getsym();
	// printf("dim:%d sym:%d\n",dim_index,sym);
	if(sym == SYM_LBRACKET)
	{
		if(dim_index == 1)
		{
			getsym();
			expression(fsys);
			array_visit(arr_index, dim_index + 1, fsys);
		}
		else
		{
			gen(LIT, 0, array_table[arr_index].dim_size[dim_index]);
			gen(OPR, 0, OPR_MUL);
			getsym();
			expression(fsys);
			gen(OPR, 0, OPR_ADD);
			array_visit(arr_index, dim_index + 1, fsys);
		}
	}
}

//////////////////////////////////////////////////////////////////////
void statement(symset fsys)
{
	int i, cx1, cx2, cx3, cx4, cx5;
	int arr_index;
	symset set1, set;
	int j = 0;
	int cxx[100];

	if (sym == SYM_IDENTIFIER)
	{ // variable assignment
		mask* mk;
		if (! (i = position(id)))
		{
			error(11); // Undeclared identifier.
		}
		else if (table[i].kind != ID_VARIABLE && table[i].kind != ID_ARRAY)
		{
			error(12); // Illegal assignment.
			i = 0;
		}
		if (table[i].kind == ID_VARIABLE)	//variable assignment
		{
			getsym();
			if (sym == SYM_BECOMES)
			{
				getsym();
			}
			else
			{
				error(13); // ':=' expected.
			}
			expression(fsys);
			mk = (mask*) &table[i];
			if (i)
			{
				gen(STO, level - mk->level, mk->address);
			}
		}
		else if (table[i].kind == ID_ARRAY)	//array assignment
		{
			mk = (mask*) &table[i];
			arr_index = mk->address;
			gen(LEA, level - mk->level, array_table[arr_index].address);

			set1 = createset(SYM_RBRACKET, SYM_NULL);
			array_visit(arr_index, 1, set1);	//count offset

			if(sym != SYM_BECOMES)
			{
				error(13); // ':=' expected.
			}

			gen(OPR, 0, OPR_ADD);	//start address + offset

			getsym();
			expression(fsys);
			if(i)
			{
				gen(STA, 0, 0);
			}
		}
	}
	else if (sym == SYM_CALL)
	{ // procedure call
		getsym();
		if (sym != SYM_IDENTIFIER)
		{
			error(14); // There must be an identifier to follow the 'call'.
		}
		else
		{
			if (! (i = position(id)))
			{
				error(11); // Undeclared identifier.
			}
			else if (table[i].kind == ID_PROCEDURE)
			{
				// mask* mk;
				// mk = (mask*) &table[i];
				// gen(CAL, level - mk->level, mk->address);
				call(i, fsys);
			}
			else
			{
				error(15); // A constant or variable can not be called.
			}
		}
	}
	else if (sym == SYM_IF)
	{ // if statement
		getsym();
		set1 = createset(SYM_THEN, SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_THEN)
		{
			getsym();
		}
		else
		{
			error(16); // 'then' expected.
		}
		cx1 = cx;
		gen(JPC, 0, 0);
		statement(fsys);
		cxx[j++] = cx;
		gen(JMP, 0, 0); // 跳到最后

		code[cx1].a = cx; // if失败
		if (sym == SYM_SEMICOLON)
		{
			getsym();
		}
		while (sym == SYM_ELIF)
		{
			getsym();
			set1 = createset(SYM_THEN, SYM_DO, SYM_NULL);
			set = uniteset(set1, fsys);
			condition(set);
			destroyset(set1);
			destroyset(set);
			if (sym == SYM_THEN)
			{
				getsym();
			}
			else
			{
				error(16); // 'then' expected.
			}
			cx1 = cx;
			gen(JPC, 0, 0);
			statement(fsys);
			cxx[j++] = cx;
			gen(JMP, 0, 0); // 跳到最后

			code[cx1].a = cx;
		}
		if (sym == SYM_SEMICOLON)
		{
			getsym();
		}
		if (sym == SYM_ELSE)
		{
			getsym();
			statement(fsys);
			for (int k = 0; k < j; k++)
			{
				code[cxx[k]].a = cx;
			}
		}

		if (sym == SYM_SEMICOLON)
		{
			getsym();
		}
	}
	else if (sym == SYM_BEGIN)
	{ // block
		getsym();
		set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
		set = uniteset(set1, fsys);
		statement(set);
		while (sym == SYM_SEMICOLON || inset(sym, statbegsys))
		{
			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else if(sym != SYM_IF )
			{
				error(10);
			}
			statement(set);
		} // while
		destroyset(set1);
		destroyset(set);
		if (sym == SYM_END)
		{
			getsym();
		}
		else
		{
			error(17); // ';' or 'end' expected.
		}
	}
	else if (sym == SYM_WHILE)
	{ // while statement
		cx1 = cx;
		getsym();
		set1 = createset(SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		cx2 = cx;
		gen(JPC, 0, 0);
		if (sym == SYM_DO)
		{
			getsym();
		}
		else
		{
			error(18); // 'do' expected.
		}
		statement(fsys);
		gen(JMP, 0, cx1);
		code[cx2].a = cx;
	}
	else if (sym == SYM_DO){
        cx1=cx;
        gen(JMP, 0 , 0);
        getsym();
        statement(fsys);
        if(sym == SYM_SEMICOLON)
            getsym();
        if(sym == SYM_WHILE){
            cx2=cx;
            getsym();
            condition(fsys);
			int cx3;
			cx3 = cx;
            gen(JPC, 0 , 0);
			gen(JMP, 0 , cx1+1);
			code[cx3].a = cx;
        }
        else {
            error(26);// 'while' expected
        }
        code[cx1].a=cx2;
	}
	else if (sym == SYM_SWITCH){
        int n_case=0;
        int cx_case[100];
        int cx_case1[100];
        int cx_caseend[100];
        int cx_default;
        int f_default=0;
        getsym();
        set1 = createset(SYM_CASE, SYM_DEFAULT, SYM_ENDSWITCH, SYM_NULL);
        set = uniteset(set1, fsys);
        expression(set);
		while(sym == SYM_CASE){
            cx_case[n_case] = cx;
            gen(CPY, 0 , 0);
            getsym();
            expression(set);
            gen(OPR, 0 , OPR_EQU);
            cx_case1[n_case] = cx;
            gen(JPC, 0 , 0);
            statement(set);
            if(sym == SYM_SEMICOLON)
                getsym();
            cx_caseend[n_case]=cx;
            gen(JMP, 0 , 0);
            n_case++;
		}
		if(sym == SYM_DEFAULT){
            f_default=1;
            cx_default = cx;
            getsym();
            statement(set);
            if(sym == SYM_SEMICOLON)
                getsym();
		}
		if(sym == SYM_ENDSWITCH){
            int i;
            for(i=0;i<n_case;i++){
                if(i==n_case-1){
                    if(f_default)
                        code[cx_case1[i]].a=cx_default;
                    else
                        code[cx_case1[i]].a=cx;
                }
                else{
                    code[cx_case1[i]].a=cx_case[i+1];
                }
                code[cx_caseend[i]].a=cx;
            }
            getsym();
            destroyset(set1);
		    destroyset(set);
		}
		else{
            error(31);
		}
	}
	else if (sym == SYM_FOR)
	{
		getsym();

		statement(fsys);
		cx1 = cx;

		if (sym == SYM_SEMICOLON)
		{
			getsym();
		}
		else
		{
			error(19); // ';' expected.
		}

		set1 = createset(SYM_SEMICOLON, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		cx2 = cx;
		gen(JPC, 0, 0);
		cx5 = cx;
		gen(JMP, 0, 0);
		if (sym == SYM_SEMICOLON)
		{
			getsym();
		}
		else
		{
			error(19); // ';' expected.
		}
		statement(fsys);
		cx3 = cx;
		gen(JMP, 0, cx1);

		statement(fsys);
		gen(JMP, 0, cx5 + 1);
		cx4 = cx;
		code[cx2].a = cx4;
		code[cx5].a = cx3 + 1;
	}
	else if (sym == SYM_EXIT)
	{
		getsym();

		gen(EXIT, 0, 0);
	}
	else if (sym == SYM_RETURN)
	{
		getsym();
		expression(fsys);
		gen(OPR, 0, OPR_RET_VAL); // return
	}
	test(fsys, phi, 19);
} // statement

//////////////////////////////////////////////////////////////////////
void block(symset fsys, int paraCount)
{
	int cx0; // initial code index
	mask* mk;
	int block_dx;
	int savedTx;
	symset set1, set;

	dx = 3 + paraCount;
	block_dx = dx;
	mk = (mask *)&table[tx - paraCount];
	mk->address = cx;
	gen(JMP, 0, 0);
	if (level > MAXLEVEL)
	{
		error(32); // There are too many levels.
	}
	do
	{
		if (sym == SYM_CONST)
		{ // constant declarations
			getsym();
			do
			{
				constdeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					constdeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			}
			while (sym == SYM_IDENTIFIER);
		} // if

		if (sym == SYM_VAR)
		{ // variable declarations
			getsym();
			do
			{
				vardeclaration();
				while (sym == SYM_COMMA)
				{
					getsym();
					vardeclaration();
				}
				if (sym == SYM_SEMICOLON)
				{
					getsym();
				}
				else
				{
					error(5); // Missing ',' or ';'.
				}
			}
			while (sym == SYM_IDENTIFIER);
		} // if
		block_dx = dx; //save dx before handling procedure call!
		while (sym == SYM_PROCEDURE)
		{ // procedure declarations
			getsym();
			if (sym == SYM_IDENTIFIER)
			{
				getsym();
				enter(ID_PROCEDURE);
				// printf("paramCount: %d\n", mk->paramCount);

			}
			else
			{
				error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
			}
			mask *mk = (mask*)&table[tx];
			mk->paramCount = 0;
			level++;
			savedTx = tx;
			dx = 3;
			if (sym == SYM_LPAREN) {
				getsym();
				while (sym == SYM_IDENTIFIER) {
					mk->paramTypes[mk->paramCount++] = ID_VARIABLE;
					enter(ID_VARIABLE);
					getsym();
					if (sym == SYM_COMMA) {
						getsym();
					}
				}
				if (sym == SYM_RPAREN) {
					getsym();
				} else {
					error(22); // Missing ')'.
				}
			}


			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}

			set1 = createset(SYM_SEMICOLON, SYM_NULL);
			set = uniteset(set1, fsys);
			block(set,mk->paramCount);
			destroyset(set1);
			destroyset(set);
			tx = savedTx;
			level--;

			if (sym == SYM_SEMICOLON)
			{
				getsym();
				set1 = createset(SYM_IDENTIFIER, SYM_PROCEDURE, SYM_NULL);
				set = uniteset(statbegsys, set1);
				test(set, fsys, 6);
				destroyset(set1);
				destroyset(set);
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}
		} // while
		dx = block_dx; //restore dx after handling procedure call!
		set1 = createset(SYM_IDENTIFIER, SYM_NULL);
		set = uniteset(statbegsys, set1);
		test(set, declbegsys, 7);
		destroyset(set1);
		destroyset(set);
	}
	while (inset(sym, declbegsys));

	code[mk->address].a = cx;
	mk->address = cx;
	cx0 = cx;
	block_dx += paraCount;
	gen(INT, 0, block_dx);
	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_ENDSWITCH, SYM_NULL);
	set = uniteset(set1, fsys);
	statement(set);
	destroyset(set1);
	destroyset(set);
	gen(OPR, 0, OPR_RET); // return
	test(fsys, phi, 8); // test for error: Follow the statement is an incorrect symbol.
	listcode(cx0, cx);
} // block

//////////////////////////////////////////////////////////////////////
int base(int stack[], int currentLevel, int levelDiff)
{
	int b = currentLevel;

	while (levelDiff--)
		b = stack[b];
	return b;
} // base

//////////////////////////////////////////////////////////////////////
// interprets and executes codes.
void interpret()
{
	int pc;        // program counter
	int stack[STACKSIZE];
	int top;       // top of stack
	int b;         // program, base, and top-stack register
	instruction i; // instruction register
	int k;

	printf("Begin executing PL/0 program.\n");

	pc = 0;
	b = 1;
	top = 3;
	stack[1] = stack[2] = stack[3] = 0;
	do
	{
		//printf("stack: %d opcode: %d pc:%d\n",stack[top], i.f,pc);
		i = code[pc++];
		switch (i.f)
		{
		case LIT:
			stack[++top] = i.a;
			break;
		case OPR:
			switch (i.a) // operator
			{
			case OPR_RET:{
				top = b - 1;
				pc = stack[top + 3];
				b = stack[top + 2];
				break;
			}case OPR_RET_VAL:{
				int temp;
				temp = stack[top];
				top = b - 1;
				pc = stack[top + 3];
				b = stack[top + 2];
				stack[++top] = temp;
				break;
			}
			case OPR_NEG:
				stack[top] = -stack[top];
				break;
			case OPR_ADD:
				top--;
				stack[top] += stack[top + 1];
				break;
			case OPR_MIN:
				top--;
				stack[top] -= stack[top + 1];
				break;
			case OPR_MUL:
				top--;
				stack[top] *= stack[top + 1];
				break;
			case OPR_DIV:
				top--;
				if (stack[top + 1] == 0)
				{
					fprintf(stderr, "Runtime Error: Divided by zero.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] /= stack[top + 1];
				break;
			case OPR_ODD:
				stack[top] %= 2;
				break;
			case OPR_EQU:
				top--;
				stack[top] = stack[top] == stack[top + 1];
				break;
			case OPR_NEQ:
				top--;
				stack[top] = stack[top] != stack[top + 1];
				break;
			case OPR_LES:
				top--;
				stack[top] = stack[top] < stack[top + 1];
				break;
			case OPR_GEQ:
				top--;
				stack[top] = stack[top] >= stack[top + 1];
				break;
			case OPR_GTR:
				top--;
				stack[top] = stack[top] > stack[top + 1];
				break;
			case OPR_LEQ:
				top--;
				stack[top] = stack[top] <= stack[top + 1];
				break;
			case OPR_NOT:
				stack[top] = !stack[top];
				break;
			case OPR_AND:
				top--;
				stack[top] = stack[top] && stack[top + 1];
				break;
			case OPR_OR:
				top--;
				stack[top] = stack[top] || stack[top + 1];
				break;
			} // switch
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			printf("%d\n", stack[top]);
			top--;
			break;
		case CAL:
			stack[top + 1] = base(stack, b, i.l);
			// generate new block mark
			stack[top + 2] = b;
			stack[top + 3] = pc;
			b = top + 1;
			pc = i.a;
			break;
		case INT:
			top += i.a;
			break;
		case JMP:
			pc = i.a;
			break;
		case JPC:
			if (stack[top] == 0)
				pc = i.a;
			top--;
			break;
		case JPC1:
			if	(stack[top] == 0)
				pc = i.a;
			break;
		case JPC2:
			if	(stack[top] != 0)
				pc = i.a;
			break;
		case CPY:
		    stack[top+1] = stack[top];
		    top++;
		    break;
		case LEA:
			stack[++top] = base(stack, b, i.l) + i.a;
			break;
		case STA:
			stack[stack[top - 1]] = stack[top];
			printf("array assigment: %d\n",stack[top]);
			top -= 2;
			break;
		case LDA:
			stack[top] = stack[stack[top]];
			break;
		case EXIT:
			pc = 0;
			break;
		case PAS:
			for (k = i.a; k > 0; k--)
			{
				stack[top + 3] = stack[top];
				top--;
			}
			break;
		} // switch
	}
	while (pc);

	printf("End executing PL/0 program.\n");
} // interpret

//////////////////////////////////////////////////////////////////////
void main ()
{
	FILE* hbin;
	char s[80];
	int i;
	symset set, set1, set2;

	printf("Please input source file name: "); // get file name to be compiled
	scanf("%s", s);
	if ((infile = fopen(s, "r")) == NULL)
	{
		printf("File %s can't be opened.\n", s);
		exit(1);
	}

	phi = createset(SYM_NULL);
	relset = createset(SYM_EQU, SYM_NEQ, SYM_LES, SYM_LEQ, SYM_GTR, SYM_GEQ, SYM_AND, SYM_OR, SYM_NOT, SYM_THEN, SYM_NULL);

	// create begin symbol sets
	declbegsys = createset(SYM_CONST, SYM_VAR, SYM_PROCEDURE, SYM_NULL);
	statbegsys = createset(SYM_BEGIN, SYM_CALL, SYM_IF, SYM_WHILE, SYM_FOR, SYM_SWITCH, SYM_NULL);
	facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_LPAREN, SYM_MINUS, SYM_NOT, SYM_AND, SYM_OR, SYM_THEN, SYM_NULL);

	err = cc = cx = ll = 0; // initialize global variables
	ch = ' ';
	kk = MAXIDLEN;

	getsym();

	set1 = createset(SYM_PERIOD, SYM_NULL);
	set2 = uniteset(declbegsys, statbegsys);
	set = uniteset(set1, set2);
	block(set,0);
	destroyset(set1);
	destroyset(set2);
	destroyset(set);
	destroyset(phi);
	destroyset(relset);
	destroyset(declbegsys);
	destroyset(statbegsys);
	destroyset(facbegsys);

	if (sym != SYM_PERIOD)
		error(9); // '.' expected.
	if (err == 0)
	{
		hbin = fopen("hbin.txt", "w");
		for (i = 0; i < cx; i++)
			fwrite(&code[i], sizeof(instruction), 1, hbin);
		fclose(hbin);
	}
	if (err == 0)
		interpret();
	else
		printf("There are %d error(s) in PL/0 program.\n", err);
	listcode(0, cx);
} // main

//////////////////////////////////////////////////////////////////////
// eof pl0.c
