/***** ltl3ba : set.c *****/

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

#include "ltl3ba.h"

extern FILE *tl_out;
extern int node_size, sym_size, scc_size;
extern char **sym_table;

/*int scc_size;*/

int mod = 8 * sizeof(int);


/* type = 2 for scc set, 1 for symbol sets, 0 for nodes sets */

#define set_size(t) (t==1?sym_size:(t==2?scc_size:node_size))

int *new_set(int type) /* creates a new set */
{
  return (int *)tl_emalloc(set_size(type) * sizeof(int));
}

int *clear_set(int *l, int type) /* clears the set */
{
  int i;
  for(i = 0; i < set_size(type); i++) {
    l[i] = 0;
  }
  return l;
}

int *make_set(int n, int type) /* creates the set {n}, or the empty set if n = -1 */
{
  int *l = clear_set(new_set(type), type);
  if(n == -1) return l;
  l[n/mod] = 1 << (n%mod);
  return l;
}

void copy_set(int *from, int *to, int type) /* copies a set */
{
  int i;
  for(i = 0; i < set_size(type); i++)
    to[i] = from[i];
}

int *dup_set(int *l, int type) /* duplicates a set */
{
  int i, *m = new_set(type);
  for(i = 0; i < set_size(type); i++)
    m[i] = l[i];
  return m;
}
  
void merge_sets(int *l1, int *l2, int type) /* puts the union of the two sets in l1 */
{
  int i;
  for(i = 0; i < set_size(type); i++)
    l1[i] = l1[i] | l2[i];
}

void do_merge_sets(int *l, int *l1, int *l2, int type) /* makes the union of two sets */
{
  int i;
  for(i = 0; i < set_size(type); i++)
    l[i] = l1[i] | l2[i];
}

int *intersect_sets(int *l1, int *l2, int type) /* makes the intersection of two sets */
{
  int i, *l = new_set(type);
  for(i = 0; i < set_size(type); i++)
    l[i] = l1[i] & l2[i];
  return l;
}

int empty_intersect_sets(int *l1, int *l2, int type) /* tests intersection of two sets */
{
  int i, test = 0;
  for(i = 0; i < set_size(type); i++)
    test |= l1[i] & l2[i];
  return !test;
}

void add_set(int *l, int n) /* adds an element to a set */
{
  l[n/mod] |= 1 << (n%mod);
}

void rem_set(int *l, int n) /* removes an element from a set */
{
  l[n/mod] &= (-1 - (1 << (n%mod)));
}

std::ostream &print_set_out(std::ostream& out , int *l, int type) /* prints the content of a set to stream out*/
{
  int i, j, start = 1;
  if(type != 1) out << "{";
  for(i = 0; i < set_size(type); i++) 
    for(j = 0; j < mod; j++)
      if(l[i] & (1 << j)) {
        switch(type) {
          case 0: case 2:
            if(!start) out << ",";
            out << (mod * i + j);
            break;
          case 1:
            if(!start) out << " & ";
            out << sym_table[mod * i + j];
            break;
        }
        start = 0;
      }
  if(type != 1) out << "}";

  return out;
}

void print_set(int *l, int type) /* prints the content of a set */
{
  int i, j, start = 1;
  if(type != 1) CHECKED_FPRINTF(tl_out, "{");
  for(i = 0; i < set_size(type); i++) 
    for(j = 0; j < mod; j++)
      if(l[i] & (1 << j)) {
        switch(type) {
          case 0: case 2:
            if(!start) CHECKED_FPRINTF(tl_out, ",");
            CHECKED_FPRINTF(tl_out, "%i", mod * i + j);
            break;
          case 1:
            if(!start) CHECKED_FPRINTF(tl_out, " & ");
            CHECKED_FPRINTF(tl_out, "%s", sym_table[mod * i + j]);
            break;
        }
        start = 0;
      }
  if(type != 1) CHECKED_FPRINTF(tl_out, "}");
}

void print_set_neg(int *l, int type) /* prints the content of a set */
{
  int i, j, start = 1;
  if(type != 1) CHECKED_FPRINTF(tl_out, "{");
  for(i = 0; i < set_size(type); i++) 
    for(j = 0; j < mod; j++)
      if(l[i] & (1 << j)) {
        switch(type) {
          case 0: case 2:
            if(!start) CHECKED_FPRINTF(tl_out, ",");
            CHECKED_FPRINTF(tl_out, "!%i", mod * i + j);
            break;
          case 1:
            if(!start) CHECKED_FPRINTF(tl_out, " & ");
            CHECKED_FPRINTF(tl_out, "!%s", sym_table[mod * i + j]);
            break;
        }
        start = 0;
      }
  if(type != 1) CHECKED_FPRINTF(tl_out, "}");
}


int empty_set(int *l, int type) /* tests if a set is the empty set */
{
  int i, test = 0;
  for(i = 0; i < set_size(type); i++)
    test |= l[i];
  return !test;
}

int same_sets(int *l1, int *l2, int type) /* tests if two sets are identical */
{
  int i, test = 1;
  for(i = 0; i < set_size(type); i++)
    test &= (l1[i] == l2[i]);
  return test;
}

int included_set(int *l1, int *l2, int type) 
{                    /* tests if the first set is included in the second one */
  int i, test = 0;
  for(i = 0; i < set_size(type); i++)
    test |= (l1[i] & ~l2[i]);
  return !test;
}

int in_set(int *l, int n) /* tests if an element is in a set */
{
  return(l[n/mod] & (1 << (n%mod)));
}

int size_set(int *l, int type) {
  int i, j, size = 0;
  for(i = 0; i < set_size(type); i++)
    for(j = 0; j < mod; j++) 
      if(l[i] & (1 << j))
        size++;
  return size;
}

int return_elem(int *l, int type) {
  int i, j, size = 0;
  for(i = 0; i < set_size(type); i++)
    for(j = 0; j < mod; j++) 
      if(l[i] & (1 << j))
        return (mod * i + j);
}

int *list_set(int *l, int type) /* transforms a set into a list */
{
  int i, j, size = 1, *list;
  for(i = 0; i < set_size(type); i++)
    for(j = 0; j < mod; j++) 
      if(l[i] & (1 << j))
	size++;
  list = (int *)tl_emalloc(size * sizeof(int));
  list[0] = size;
  size = 1;
  for(i = 0; i < set_size(type); i++)
    for(j = 0; j < mod; j++) 
      if(l[i] & (1 << j))
	list[size++] = mod * i + j;
  return list;
}

void substract_set(int *l1, int *l2, int *l3) { /* substracts the elements of l3 from l2 and put to l1 */
  int i;
  for(i = 0; i < set_size(0); i++)
    l1[i] = l2[i] & ~l3[i];
}

int compare_sets_lexi(int *l1, int *l2, int type) /* compare lexicographically two sets */
{
  int i;
  for(i = 0; i < set_size(type); i++) {
    if (l1[i] != l2[i]) {
      return ((l1[i] < l2[i]) ? 1 : 0 );
    }
  }
  return 0;
}
