/* ltl3baCInt.c --- 
 * 
 * Filename: ltl3baCInt.c
 * Author: Abhishek Udupa
 * Created: Fri Apr  4 19:05:45 2014 (-0400)
 */

/* Copyright (c) 2013, Abhishek Udupa, University of Pennsylvania
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *    This product includes software developed by The University of Pennsylvania
 * 4. Neither the name of the University of Pennsylvania nor the
 *    names of its contributors may be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* Code: */

#include "ltl3baCInt.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

AutC* LTL3BAGeneratedCAut = NULL;

char* MkString(const char* cptr)
{
    int Len = strlen(cptr);
    char* Retval = (char*)malloc(sizeof(char) * (Len + 1));
    strcpy(Retval, cptr);
    return Retval;
}

void FreePropLiteralC(PropLiteralC* Lit)
{
    free(Lit->Name);
    free(Lit);
}

void FreeCubeC(CubeC* TheCube)
{
    int i;
    for (i = 0; i < TheCube->NumElems; ++i) {
        FreePropLiteralC(TheCube->Props[i]);
    }
    free(TheCube->Props);
    free(TheCube);
}

void FreePropC(PropC* Prop) 
{
    int i;
    for (i = 0; i < Prop->NumElems; ++i) {
        FreeCubeC(Prop->Disjuncts[i]);
    }
    free(Prop->Disjuncts);
    free(Prop);
}

void FreeEdgeC(EdgeC* Edge)
{
    free(Edge->FromState);
    FreePropC(Edge->Prop);
    free(Edge->ToState);
    free(Edge);
}

void FreeNodeC(NodeC* Node)
{
    int i;
    free(Node->NodeName);
    for (i = 0; i < Node->NumEdges; ++i) {
        FreeEdgeC(Node->Edges[i]);
    }
    free(Node->Edges);
    free(Node);
}

void FreeAutC(AutC* Aut)
{
    int i;
    for (i = 0; i < Aut->NumNodes; ++i) {
        FreeNodeC(Aut->Nodes[i]);
    }
    free(Aut->Nodes);
    free(Aut);
}


extern int libltl3ba_main_pure_c(const char* Prop, bool Det, bool Simp);

int libltl3ba_main_c(const char* Prop, bool Det, bool Simp)
{
    return libltl3ba_main_pure_c(Prop, Det, Simp);
}

void libltl3ba_teardown_c()
{
    if (LTL3BAGeneratedCAut != NULL) {
        FreeAutC(LTL3BAGeneratedCAut);
        LTL3BAGeneratedCAut = NULL;
    }
}


/* ltl3baCInt.c ends here */
