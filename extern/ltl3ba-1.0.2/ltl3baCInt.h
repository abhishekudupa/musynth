/* ltl3baCInt.h --- 
 * 
 * Filename: ltl3baCInt.h
 * Author: Abhishek Udupa
 * Created: Fri Apr  4 18:40:56 2014 (-0400)
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

/* strict C interface for ltl3ba */

#include <stdbool.h>

typedef struct PropLiteralC_Tag {
    bool Negated;
    bool IsTrue;
    bool IsFalse;
    char* Name;
} PropLiteralC;

typedef struct CubeC_Tag {
    int NumElems;
    PropLiteralC** Props;
} CubeC;

typedef struct PropC_Tag {
    int NumElems;
    CubeC** Disjuncts;
} PropC;

typedef struct EdgeC_Tag {
    char* FromState;
    PropC* Prop;
    char* ToState;
} EdgeC;

typedef struct NodeC_Tag {
    char* NodeName;
    int Initial;
    int Accepting;
    int NumEdges;
    EdgeC** Edges;
} NodeC;

typedef struct AutC_Tag {
    int NumNodes;
    NodeC** Nodes;
} AutC;

/* Flag indicating to main lib that we're being called from C */
extern bool IsCalledFromCLib;

#ifdef __cplusplus
extern "C" {
#endif

    char* MkString(const char* cptr);
    void FreeAutC(AutC* Aut);
    void FreeNodeC(NodeC* Node);
    void FreeEdgeC(EdgeC* Edge);
    void FreePropC(PropC* Prop);
    void FreeCubeC(CubeC* TheCube);
    void FreePropLiteralC(PropLiteralC* Lit);
    
    int libltl3ba_main_c(const char* prop, bool Det, bool Simp);
    void libltl3ba_teardown_c();

#ifdef __cplusplus
}
#endif

/* the built up automaton */
extern AutC* LTL3BAGeneratedCAut;


/* ltl3baCInt.h ends here */
