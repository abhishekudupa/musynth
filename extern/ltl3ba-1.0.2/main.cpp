/***** ltl3ba : main.c *****/

/* Written by Denis Oddoux, LIAFA, France                                 */
/* Copyright (c) 2001  Denis Oddoux                                       */
/* Modified by Paul Gastin, LSV, France                                   */
/* Copyright (c) 2007  Paul Gastin                                        */
/* Modified by Tomas Babiak, FI MU, Brno, Czech Republic                  */
/* Copyright (c) 2012  Tomas Babiak                                       */
/*                                                                        */
/* This program is free software; you can redistribute it and/or modify   */
/* it under the terms of the GNU General Public License as published by   */
/* the Free Software Foundation; either version 2 of the License, or      */
/* (at your option) any later version.                                    */
/*                                                                        */
/* This program is distributed in the hope that it will be useful,        */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of         */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          */
/* GNU General Public License for more details.                           */
/*                                                                        */
/* You should have received a copy of the GNU General Public License      */
/* along with this program; if not, write to the Free Software            */
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA*/
/*                                                                        */
/* Based on the translation algorithm by Gastin and Oddoux,               */
/* presented at the 13th International Conference on Computer Aided       */
/* Verification, CAV 2001, Paris, France.                                 */
/* Proceedings - LNCS 2102, pp. 53-65                                     */
/*                                                                        */
/* Modifications based on paper by                                        */
/* T. Babiak, M. Kretinsky, V. Rehak, and J. Strejcek,                    */
/* LTL to Buchi Automata Translation: Fast and More Deterministic         */
/* presented at the 18th International Conference on Tools and            */
/* Algorithms for the Construction and Analysis of Systems (TACAS 2012)   */
/*                                                                        */
/* Some of the code in this file was taken from the Spin software         */
/* Written by Gerard J. Holzmann, Bell Laboratories, U.S.A.               */

#include <iostream>
#include <fstream>
#include "ltl3ba.h"
#include "unistd.h"

FILE	*tl_out;

#ifdef STATS
int	tl_stats     = 0; /* time and size stats */	
#endif
int tl_simp_log  = 1; /* logical simplification */
int tl_simp_diff = 1; /* automata simplification */
int tl_simp_fly  = 1; /* on the fly simplification */
int tl_simp_scc  = 1; /* use scc simplification */
int tl_fjtofj    = 1; /* 2eme fj */
int tl_postpone  = 1; /* use suspension for TGBA construction */
int tl_f_components = 1; /* use direct building of final TGBA components corresponding to GFp_1 && .. && GFp_n*/
int tl_rem_scc   = 1; /* enable removing non-accepting strongly connected components */
int tl_alt       = 1; /* use suspension for VWAA construction */
int tl_rew_f     = 1; /* rewrite R formulae with alternating subformulae */
int tl_det_m     = 0; /* construct more deterministic automaton */
int tl_determinize  = 0; /* old (not so efficient) determinization */
int tl_bisim     = 0; /* enable basic bisimulatin reduction of BA */
int tl_bisim_r   = 0; /* enable basic bisimulatin reduction of BA and repeat until there is no reduction */
int tl_sim       = 0; /* enable strong fair simulation reduction of BA */
int tl_sim_r     = 0; /* enable strong fair simulation reduction of BA and repeat until there is no reduction */
int tl_ltl3ba    = 1; /* enable some LTL3BA specific improvements */
int tl_tgba_out  = 0;
int tl_ba_out    = 0;
int tl_errs      = 0;
int tl_verbose   = 0;
int tl_terse     = 0;
unsigned long	All_Mem = 0;

bool IsCalledFromLib = false;
BAutomaton LibLTL3BAGenAut;

std::string uform;
static int	hasuform=0, cnt=0;
std::string ltl_file;
std::string add_ltl;

static void	tl_endstats(void);
static void	non_fatal(char *, char *);

inline void
alldone(int estatus)
{
        bdd_done();
        exit(estatus);
}

char *
emalloc(int n)
{       char *tmp;

        if (!(tmp = (char *) malloc(n)))
                fatal("not enough memory", (char *)0);
        memset(tmp, 0, n);
        return tmp;
}

int
tl_Getchar(void)
{
	if (cnt < hasuform)
		return uform[cnt++];
	cnt++;
	return -1;
}

void
put_uform(void)
{
	CHECKED_FPRINTF(tl_out, "%s", uform.c_str());
}

void
tl_UnGetchar(void)
{
	if (cnt > 0) cnt--;
}

void
usage(int estatus)
{
        CHECKED_PRINTF("usage: ltl3ba [-flag] -f formula\n");
        CHECKED_PRINTF("                   or -F file\n");
        CHECKED_PRINTF(" -f \"formula\"\ttranslate LTL ");
        CHECKED_PRINTF("into never claim\n");
        CHECKED_PRINTF(" -F file\tlike -f, but with the LTL ");
        CHECKED_PRINTF("formula stored in a 1-line file\n");
        CHECKED_PRINTF(" -d\t\tdisplay automata (D)escription at each step\n");
#ifdef STATS
        CHECKED_PRINTF(" -s\t\tcomputing time and automata sizes S(t)atistics\n");
#endif
        CHECKED_PRINTF(" -l\t\tdisable (L)ogic formula simplification\n");
        CHECKED_PRINTF(" -p\t\tdisable a-(P)osteriori simplification\n");
        CHECKED_PRINTF(" -o\t\tdisable (O)n-the-fly simplification\n");
        CHECKED_PRINTF(" -c\t\tdisable strongly (C)onnected components simplification\n");
        CHECKED_PRINTF(" -a\t\tdisable trick in (A)ccepting conditions\n");
        CHECKED_PRINTF("\n  LTL3BA specific options:\n");
        CHECKED_PRINTF(" -P\t\tdisable (P)ostponing/suspension in TGBA construction\n");
        CHECKED_PRINTF(" -D\t\tdisable (D)irect building of final components\n");
        CHECKED_PRINTF(" -C\t\tdisable removing non-accepting strongly (C)onnected components\n");
        CHECKED_PRINTF(" -A\t\tdisable suspension in (A)lternating automaton construction\n");
        CHECKED_PRINTF(" -R\t\tdisable rewriting R formulae with alternating subformulae\n");
        CHECKED_PRINTF(" -M\t\ttry to produce more deter(M)inistic automaton (recomended)\n");
        CHECKED_PRINTF(" -B\t\tenable basic (B)isimulation reduction of BA\n");
        CHECKED_PRINTF(" -S\t\tenable strong fair (S)imulation reduction of BA\n");
        CHECKED_PRINTF(" -T\t\tconstruct only the TGBA and output it in SPOT's format\n");
        CHECKED_PRINTF(" -U\t\toutput final BA in SPOT's format\n");
        CHECKED_PRINTF(" -x\t\tdisable all LTL3BA specific improvements (act like LTL2BA)\n");
        CHECKED_PRINTF(" -v\t\tprint LTL3BA's version and exit\n");
        CHECKED_PRINTF(" -h\t\tprint this help\n");
	
        alldone(estatus);
}

void
print_version()
{
        CHECKED_PRINTF("LTL3BA %s\n", VERSION_NUM);
}

int
tl_main(std::string &argv)
{ 
  int i;
	for (i = 0; i < argv.length(); i++)
	{	if (argv[i] == '\t'
		||  argv[i] == '\"'
		||  argv[i] == '\n')
			argv[i] = ' ';
	}
    uform = argv;
	hasuform = uform.length();
	if (hasuform == 0) usage(1);
	tl_parse();
#ifdef STATS
	if (tl_stats) tl_endstats();
#endif
	return tl_errs;
}

#if !defined LTL3BA_LIB_BUILD

int
main(int argc, char *argv[])
{	int i;
	tl_out = stdout;

	while (argc > 1 && argv[1][0] == '-')
        {       switch (argv[1][1]) {
                case 'F': if (*(argv+2)) ltl_file = *(argv+2);
                          argc--; argv++; break;
                case 'f': if (*(argv+2)) add_ltl = *(argv+2);
                          argc--; argv++; break;
                case 'a': tl_fjtofj = 0; break;
                case 'c': tl_simp_scc = 0; tl_rem_scc = 0; break;
                case 'o': tl_simp_fly = 0; break;
                case 'p': tl_simp_diff = 0; break;
                case 'l': tl_simp_log = 0; break;
                case 'd': tl_verbose = 1; break;
#ifdef STATS
                case 't': tl_stats = 1; break;
#endif
                case 'M': tl_det_m = 1; break;
                case 'P': tl_postpone = 0; break;
                case 'D': tl_f_components = 0; break;
                case 'C': tl_rem_scc = 0; break;
                case 'A': tl_alt = 0; break;
                case 'R': tl_rew_f = 0; break;
                case 'm': tl_determinize   = 1; break;
                case 'b': tl_bisim = 1; break;
                case 'B': tl_bisim_r = 1; break;
                case 's': tl_sim = 1; break;
                case 'S': tl_sim_r = 1; break;
                case 'T': tl_tgba_out = 1; tl_ba_out = 0; break;
                case 'U': tl_ba_out = 1; tl_tgba_out = 0; break;
                case 'x': tl_postpone = 0; tl_f_components = 0; tl_ltl3ba = 0; tl_rem_scc = 0; tl_alt = 0; tl_rew_f = 0; break;
                case 'v': print_version(); alldone(0);
                case 'h': usage(0); break;
                default : CHECKED_PRINTF("ltl3ba: unknown option -- %c\n\n", argv[1][1]); usage(1); break;
                }
                argc--, argv++;
        }
  
        if(ltl_file.empty() && add_ltl.empty()) {
          CHECKED_PRINTF("ltl3ba: no formula given at input\n\n");
          usage(1);
        }

        if (!ltl_file.empty())
        {       std::ifstream in_file(ltl_file.c_str(), std::ifstream::in);
                if (!in_file.is_open())
                {       CHECKED_PRINTF("ltl3ba: cannot open %s\n", ltl_file.c_str());
                        alldone(1);
                }
                std::getline(in_file, add_ltl, '\0');
                in_file.close();
        }
        if (argc > 1)
        {       usage(1);
        }
        else if (argc > 0)
        {       exit(tl_main(add_ltl));
        }
        usage(1);
}

#else /* LTL3BA_LIB_BUILD */

int libltl3ba_main(const std::string &LTLProp, BAutomaton &Aut,
                   bool Det, bool Simp)
{
    if (Det) {
        tl_determinize = 1;
    } else {
        tl_determinize = 0;
    }
    if (Simp) {
        tl_bisim = 1;
        tl_bisim_r = 1;
        tl_sim = 1;
        tl_sim_r = 1;
    } else {
        tl_bisim = 0;
        tl_bisim_r = 0;
        tl_sim = 0;
        tl_sim_r = 0;
    }
    IsCalledFromLib = true;
    LibLTL3BAGenAut.clear();
    std::string PropLocal = LTLProp;
    int retval = tl_main(PropLocal);
    Aut = LibLTL3BAGenAut;
    IsCalledFromLib = false;
    LibLTL3BAGenAut.clear();
    return retval;
}

#endif /* LTL3BA_LIB_BUILD */

#ifdef STATS
/* Subtract the `struct timeval' values X and Y, storing the result X-Y in RESULT.
   Return 1 if the difference is negative, otherwise 0.  */
 
/*int
timeval_subtract (result, x, y)
struct timeval *result, *x, *y;*/
int timeval_subtract (timeval *result, timeval *x, timeval *y)
{ 
	if (x->tv_usec < y->tv_usec) {
		x->tv_usec += 1000000;
		x->tv_sec--;
	}
	
	/* Compute the time remaining to wait. tv_usec is certainly positive. */
	result->tv_sec = x->tv_sec - y->tv_sec;
	result->tv_usec = x->tv_usec - y->tv_usec;
	
	/* Return 1 if result is negative. */
	return x->tv_sec < y->tv_sec;
}

static void
tl_endstats(void)
{	extern int Stack_mx;
	CHECKED_PRINTF("\ntotal memory used: %9ld\n", All_Mem);
	/*CHECKED_PRINTF("largest stack sze: %9d\n", Stack_mx);*/
	/*cache_stats();*/
	a_stats();
}
#endif

#define Binop(a)		\
		CHECKED_FPRINTF(tl_out, "(");	\
		dump(n->lft);		\
		CHECKED_FPRINTF(tl_out, a);	\
		dump(n->rgt);		\
		CHECKED_FPRINTF(tl_out, ")")

void
dump(Node *n)
{
	if (!n) return;

	switch(n->ntyp) {
	case OR:	Binop(" || "); break;
	case AND:	Binop(" && "); break;
	case U_OPER:
	  if(is_F(n)) {
	    CHECKED_FPRINTF(tl_out, "F");
	    dump(n->rgt);
	  } else {
	    Binop(" U ");
	  }
	  break;
	case V_OPER:
	  if(is_G(n)) {
	    CHECKED_FPRINTF(tl_out, "G");
	    dump(n->rgt);
	  } else {
	   	Binop(" R ");
	  }
	  break;
#ifdef NXT
	case NEXT:
		CHECKED_FPRINTF(tl_out, "X");
		CHECKED_FPRINTF(tl_out, " (");
		dump(n->lft);
		CHECKED_FPRINTF(tl_out, ")");
		break;
#endif
	case NOT:
		CHECKED_FPRINTF(tl_out, "!");
		CHECKED_FPRINTF(tl_out, "(");
		dump(n->lft);
		CHECKED_FPRINTF(tl_out, ")");
		break;
	case FALSE:
		CHECKED_FPRINTF(tl_out, "false");
		break;
	case TRUE:
		CHECKED_FPRINTF(tl_out, "true");
		break;
	case PREDICATE:
		CHECKED_FPRINTF(tl_out, "(%s)", n->sym->name);
		break;
	case -1:
		CHECKED_FPRINTF(tl_out, " D ");
		break;
	default:
		CHECKED_PRINTF("Unknown token: ");
		tl_explain(n->ntyp);
		break;
	}
}

void
tl_explain(int n)
{
	switch (n) {
	case ALWAYS:	CHECKED_PRINTF("[]"); break;
	case EVENTUALLY: CHECKED_PRINTF("<>"); break;
	case IMPLIES:	CHECKED_PRINTF("->"); break;
	case EQUIV:	CHECKED_PRINTF("<->"); break;
	case PREDICATE:	CHECKED_PRINTF("predicate"); break;
	case OR:	CHECKED_PRINTF("||"); break;
	case AND:	CHECKED_PRINTF("&&"); break;
	case NOT:	CHECKED_PRINTF("!"); break;
	case U_OPER:	CHECKED_PRINTF("U"); break;
	case V_OPER:	CHECKED_PRINTF("V"); break;
#ifdef NXT
	case NEXT:	CHECKED_PRINTF("X"); break;
#endif
	case TRUE:	CHECKED_PRINTF("true"); break;
	case FALSE:	CHECKED_PRINTF("false"); break;
	case ';':	CHECKED_PRINTF("end of formula"); break;
	default:	CHECKED_PRINTF("%c", n); break;
	}
}

static void
non_fatal(const char *s1, char *s2)
{	extern int tl_yychar;
	int i;

	CHECKED_PRINTF("ltl3ba: ");
	if (s2) {
		CHECKED_PRINTF(s1, s2);
	} else {
		CHECKED_PRINTF("%s",s1);
    }
	if (tl_yychar != -1 && tl_yychar != 0)
	{	CHECKED_PRINTF(", saw '");
		tl_explain(tl_yychar);
		CHECKED_PRINTF("'");
	}
	CHECKED_PRINTF("\nltl3ba: %s\n-------", uform.c_str());
	for (i = 0; i < cnt; i++) {
		CHECKED_PRINTF("-");
    }
	CHECKED_PRINTF("^\n");
	fflush(stdout);
	tl_errs++;
}

void
tl_yyerror(const char *s1)
{
	Fatal(s1, (char *) 0);
}

void
Fatal(const char *s1, char *s2)
{
  non_fatal(s1, s2);
  alldone(1);
}

void
fatal(const char *s1, char *s2)
{
        non_fatal(s1, s2);
        alldone(1);
}


