/***** ltl3ba : ltl3ba.h *****/

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

#include <stdio.h>
#include <string.h>
#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <bdd.h>
#include <map>
#include <vector>

/* Set LTL3BA's version number */
#define VERSION_NUM "1.0.2"

/* Defines, whether to gather statistics.                                  */
/* Statistics are not relevant since ltl3ba upgrade, so they are disabled. */
/* Some tweeeks are necessary in order to gather relevant data again.      */
//#define STATS

class cset;
class cGTrans;

extern "C" {

typedef struct Symbol {
char		*name;
	struct Symbol	*next;	/* linked list, symbol table */
} Symbol;

typedef struct Node {
	short  ntyp;	/* node type */
	struct Symbol	*sym;
	struct Node	*lft;	/* tree */
	struct Node	*rgt;	/* tree */
	struct Node	*nxt;	/* if linked list */
} Node;

typedef struct Graph {
	Symbol		*name;
	Symbol		*incoming;
	Symbol		*outgoing;
	Symbol		*oldstring;
	Symbol		*nxtstring;
	Node		*New;
	Node		*Old;
	Node		*Other;
	Node		*Next;
	unsigned char	isred[64], isgrn[64];
	unsigned char	redcnt, grncnt;
	unsigned char	reachable;
	struct Graph	*nxt;
} Graph;

typedef struct Mapping {
	char	*from;
	Graph	*to;
	struct Mapping	*nxt;
} Mapping;

typedef struct ATrans {
  bdd label;
  int *bad_nodes;
  struct ATrans *nxt;
} ATrans;

typedef struct GTrans {
  bdd label;
  cset *final;
  struct GState *to;
  struct GTrans *nxt;
} GTrans;

typedef struct GState {
  int id;
  int incoming;
  cset *nodes_set;
  cGTrans* trans;
  struct GState *nxt;
  struct GState *prv;
} GState;

typedef struct BState {
  struct GState *gstate;
  int id;
  int incoming;
  int final;
  //         to    label
  std::map<BState*, bdd> *trans;
  struct BState *nxt;
  struct BState *prv;
} BState;

typedef struct GScc {
  struct GState *gstate;
  int rank;
  int theta;
  struct GScc *nxt;
} GScc;

typedef struct BScc {
  struct BState *bstate;
  int rank;
  int theta;
  struct BScc *nxt;
} BScc;

enum {
	ALWAYS=257,
	AND,		/* 258 */
	EQUIV,		/* 259 */
	EVENTUALLY,	/* 260 */
	FALSE,		/* 261 */
	IMPLIES,	/* 262 */
	NOT,		/* 263 */
	OR,		/* 264 */
	PREDICATE,	/* 265 */
	TRUE,		/* 266 */
	U_OPER,		/* 267 */
	V_OPER		/* 268 */
#ifdef NXT
	, NEXT		/* 269 */
#endif
};

// Macros for conditional printing
#define CHECKED_PRINTF(args...)  \
    {                            \
        if (!IsCalledFromLib) {  \
            printf(args);        \
        }                        \
    }


#define CHECKED_FPRINTF(_handle_, args...)      \
    {                                           \
        if (!IsCalledFromLib) {                 \
            fprintf(_handle_, args);            \
        }                                       \
    }


Node	*Canonical(Node *);
Node	*canonical(Node *);
Node	*cached(Node *);
Node	*dupnode(Node *);
Node	*getnode(Node *);
Node	*in_cache(Node *);
Node	*push_negation(Node *);
Node	*push_negation_simple(Node *);
Node	*right_linked(Node *);
Node	*tl_nn(int, Node *, Node *);

Symbol	*tl_lookup(char *);
Symbol	*getsym(Symbol *);
Symbol	*DoDump(Node *);

char	*emalloc(int);	

int	anywhere(int, Node *, Node *);
int	dump_cond(Node *, Node *, int);
int	isequal(Node *, Node *);
int	tl_Getchar(void);

void	*tl_emalloc(int);
ATrans  *emalloc_atrans();
void    free_atrans(ATrans *, int);
void    free_atrans_map(std::map<cset, ATrans*> *);
void    free_all_atrans();
GTrans  *emalloc_gtrans();
void    free_gtrans(GTrans *, GTrans *, int);
#ifdef STATS
void	a_stats(void);
#endif
void	addtrans(Graph *, char *, Node *, char *);
void	cache_stats(void);
void	dump(Node *);
void	exit(int);
void	Fatal(const char *, char *);
void	fatal(const char *, char *);
void	fsm_print(void);
void	releasenode(int, Node *);
void	tfree(void *);
void	tl_explain(int);
void	tl_UnGetchar(void);
void	tl_parse(void);
void	tl_yyerror(const char *);
void	trans(Node *);

void    mk_alternating(Node *);
void    mk_generalized();
void    mk_buchi();

ATrans *dup_trans(ATrans *);
ATrans *merge_trans(ATrans *, ATrans *);
void do_merge_trans(ATrans **, ATrans *, ATrans *);

int  *new_set(int);
int  *clear_set(int *, int);
int  *make_set(int , int);
void copy_set(int *, int *, int);
int  *dup_set(int *, int);
void merge_sets(int *, int *, int);
void do_merge_sets(int *, int *, int *, int);
int  *intersect_sets(int *, int *, int);
void add_set(int *, int);
void rem_set(int *, int);
void spin_print_set(int *, int*);
void print_set(int *, int);
void print_set_neg(int *, int);
std::ostream &print_set_out(std::ostream &, int *, int);
int  empty_set(int *, int);
int  empty_intersect_sets(int *, int *, int);
int  same_sets(int *, int *, int);
int  included_set(int *, int *, int);
int  in_set(int *, int);
int  *list_set(int *, int);
void substract_set(int *, int *, int *);
int compare_sets_lexi(int *, int *, int);

#ifdef STATS
int timeval_subtract (struct timeval *, struct timeval *, struct timeval *);
#endif

typedef struct Queue {
  int max;
  int front;
  int size;
  int *data;
} Queue;

Queue* create_queue(int max_size);
void free_queue(Queue *q);
int is_empty(Queue *q);
int is_full(Queue *q);
int push(Queue *q, int elem);
int pop(Queue *q);
void print_queue(Queue *q);

int is_G(Node *);
int is_F(Node *);
int is_Falpha(Node *);
int is_UXp(Node *);
int is_LTL(Node *);
int is_EVE(Node *);
int is_UNI(Node *);
int is_FIN(Node *);
int is_INFp(Node *);
int is_INFd(Node *);
int is_GF_component(Node *n);

}

void allsatPrintHandler(char* , int);

class cset
{
 public:

   cset(void)          { this->type = 0; s = new_set(type); }
   cset(int type)      { this->type = type; s = new_set(type); }
   cset(int el, int type)      { this->type = type; s = make_set(el, type); }
   cset(const cset &r) { this->type = r.type; s = dup_set(r.s, type); }
   
   ~cset(void)         { tfree(s); }
   
   void insert(int el);
   void erase(int el);
   int  is_elem(int el) const;
   int  empty(void) const;
   void clear();
   // Merges two sets from input and result save to this->s
   void merge(const cset &l, const cset &r);
   void substract(const cset &r);
   void substract(int *r);
   int* get_set(void) const;
   int* to_list(void) const;
   int is_subset_of(const cset &r) const;
   
   cset& operator=(const cset &r);
   bool operator<(const cset &r) const;
   bool operator==(const cset &r) const;
   bool operator!=(const cset &r) const;
   
   void print(void) const;
   
 private:
    
    int *s;
    int type;
    
    friend std::ostream &operator<<(std::ostream &, const cset &);


};

inline void cset::insert(int el) {
  add_set(s, el);
}

inline void cset::erase(int el) {
  rem_set(s, el);
}

inline int cset::is_elem(int el) const {
  return in_set(s, el);
}

inline int cset::empty(void) const {
  return empty_set(s, type);
}

inline void cset::clear() {
  clear_set(s, type);
}

inline bool cset::operator==(const cset &r) const {
  return (type == r.type && same_sets(s, r.s, type));
}

inline bool cset::operator!=(const cset &r) const {
  return (type != r.type || !same_sets(s, r.s, type));
}

inline bool cset::operator<(const cset &r) const {
  return (type < r.type || (type == r.type && compare_sets_lexi(s, r.s, type)));
}

inline void cset::print(void) const {
  print_set(s, type);
}

inline int* cset::get_set(void) const {
  return s;
}

inline int* cset::to_list(void) const {
  return list_set(s, 0);
}

inline int cset::is_subset_of(const cset &r) const {
  return (type == r.type && included_set(s, r.s, type));
}

class AProd {
  public:
    
    AProd(void)
      { prod = emalloc_atrans(); prod->label = bdd_true(); trans = (std::map<cset, ATrans*> *) 0; }
    AProd(int i, std::map<cset, ATrans*> *t) 
      { prod = (ATrans *) 0; astate = i; trans = t; if (trans) { curr_trans = trans->begin(); last_trans = --trans->end();} }
    ~AProd(void) {if (prod) free_atrans(prod, 0); }

    int astate;
    std::map<cset, ATrans*> *trans;
    std::map<cset, ATrans*>::iterator curr_trans, last_trans;
    struct ATrans *prod;
    cset prod_to;
    AProd *nxt;
    AProd *prv;
    
//    std::pair<const cset, ATrans*>* get_curr_trans();
    void merge_to_prod(AProd *p1, std::pair<const cset, ATrans*> &trans);
    void merge_to_prod(AProd *p1, int i);
    void next(void);
    bool no_next(void);
    void restart(void);

  private:
};

//inline std::pair<const cset, ATrans*>* AProd::get_curr_trans() {
//  return &(*curr_trans);
//}

inline void AProd::next(void) {
  curr_trans++;
}

inline void AProd::restart(void) {
  curr_trans = trans->begin();
}

inline bool AProd::no_next(void) {
  return (trans && curr_trans == last_trans);
}

class cGTrans {
  public:
//    cGTrans(void);
//    cGTrans(const cGTrans &t) { trans = t.trans; }
//    ~cGTrans(void);

//    bool operator<(const cGTrans &t) const;
    bool operator==(const cGTrans &t) const;
    bool operator!=(const cGTrans &t) const;
//    cGTrans& operator=(const cGTrans &t);
    
    void decrement_incoming(void);
    // check wheter the newly build transitions dominates any existing or is dominated
    // true value is returned if the new transition shoul be added
    bool check_dominance(ATrans *t, cset *t_to, cset* fin, int acc, int &state_trans, GState* s);
    bool determinize(ATrans *t, cset *t_to, cset* fin, int acc, int &state_trans, GState* s);
    bool add_trans(bdd label, cset *fin, GState* to);
    bool empty(void);
    size_t size(void);
    
    std::map<GState*, std::map<cset, bdd> >::iterator begin();
    std::map<GState*, std::map<cset, bdd> >::iterator end();
    std::map<cset, bdd>& operator[] (GState* x);
    void erase(std::map<GState*, std::map<cset, bdd> >::iterator &tx);
    
  private:
    std::map<GState*, std::map<cset, bdd> > trans;
};

//inline bool cGTrans::operator<(const cGTrans &t) const {
//  return (trans < t.trans);
//}

inline bool cGTrans::operator==(const cGTrans &t) const {
  return (trans == t.trans);
}

inline bool cGTrans::operator!=(const cGTrans &t) const {
  return (trans != t.trans);
}

//inline cGTrans& cGTrans::operator=(const cGTrans &t) {
//  trans = t.trans;
//  return (*this);
//}

inline bool cGTrans::empty(void) {
  return trans.empty();
}

inline size_t cGTrans::size(void) {
  return trans.size();
}

inline std::map<GState*, std::map<cset, bdd> >::iterator cGTrans::begin() {
  return trans.begin();
}
inline std::map<GState*, std::map<cset, bdd> >::iterator cGTrans::end() {
  return trans.end();
}

inline std::map<cset, bdd>& cGTrans::operator[] (GState* x) {
  return trans[x];
}

inline void cGTrans::erase(std::map<GState*, std::map<cset, bdd> >::iterator &tx) {
  trans.erase(tx);
}

#define ZN	(Node *)0
#define ZS	(Symbol *)0
#define Nhash	255    	
#define True	tl_nn(TRUE,  ZN, ZN)
#define False	tl_nn(FALSE, ZN, ZN)
#define Not(a)	push_negation(tl_nn(NOT, a, ZN))
#define rewrite(n)	canonical(right_linked(n))

typedef Node	*Nodeptr;
#define YYSTYPE	 Nodeptr

#define Debug(x)	{ if (0) CHECKED_PRINTF(x); }
#define Debug2(x,y)	{ if (tl_verbose) CHECKED_PRINTF(x,y); }
#define Dump(x)		{ if (0) dump(x); }
#define Explain(x)	{ if (tl_verbose) tl_explain(x); }

#define Assert(x, y)	{ if (!(x)) { tl_explain(y); \
			  Fatal(": assertion failed\n",(char *)0); } }
#define min(x,y)        ((x<y)?x:y)

/* audupa: additional decls for lib main */

struct PropLiteral {
    bool Negated;
    bool IsTrue;
    bool IsFalse;
    std::string Name;

    PropLiteral(const std::string& Name, bool Negated = false) 
    : Negated(Negated), IsTrue(false), IsFalse(false), Name(Name) {}
    PropLiteral(bool ConstVal)
    : Negated(false), IsTrue(ConstVal), IsFalse(!ConstVal), Name("") {}
};

typedef std::vector<PropLiteral> Cube;

struct BAEdge {
    std::string FromState;
    std::vector<Cube> Disjuncts;
    std::string ToState;

    BAEdge(const std::string& FromState,
           const std::vector<Cube>& Disjuncts,
           const std::string& ToState)
    : FromState(FromState), Disjuncts(Disjuncts), ToState(ToState) {}

};

struct BANode {
    std::string NodeName;
    bool Initial;
    bool Accepting;
    std::vector<BAEdge> Edges;

    BANode(const std::string& NodeName, const std::vector<BAEdge>& Edges,
           bool Initial = false, bool Accepting = false)
    : NodeName(NodeName), Initial(Initial), Accepting(Accepting), Edges(Edges) {}
    BANode()
    : NodeName(""), Initial(false), Accepting(false) {}
};


// Flag indicating whether we're called from libmain
extern bool IsCalledFromLib;
extern bool IsCalledFromCLib;

typedef std::map<std::string, BANode> BAutomaton;
extern BAutomaton LibLTL3BAGenAut;

extern int libltl3ba_main(const std::string& LTLProp, BAutomaton& Aut, 
                          bool Det = false, bool Simp = false);

#ifdef __cplusplus
extern "C" {
#endif 

extern int libltl3ba_main_pure_c(const char* Prop, bool Det, bool Simp);

#ifdef __cplusplus
}
#endif

