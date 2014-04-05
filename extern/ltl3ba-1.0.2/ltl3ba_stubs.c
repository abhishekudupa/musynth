/* ltl3ba_stubs.c --- 
 * 
 * Filename: ltl3ba_stubs.c
 * Author: Abhishek Udupa
 * Created: Fri Apr  4 20:14:10 2014 (-0400)
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



/* Stubs for ltl3ba libmain to be used from ocaml */

#include "ltl3baCInt.h"

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <stdio.h>

extern CAMLprim void ltl3ba_native_mk_ba(value VPropString, value VDet, value VSimp);
extern CAMLprim void ltl3ba_native_teardown();
extern CAMLprim value ltl3ba_native_translate_ba();

CAMLprim void ltl3ba_native_mk_ba(value VPropString, value VDet, value VSimp)
{
    CAMLparam3(VPropString, VDet, VSimp);
    char *PropString;
    bool Det;
    bool Simp;
    char ErrString[4096];
    int Err;

    PropString = String_val(VPropString);
    Det = Bool_val(VDet) == 0 ? false : true;
    Simp = Bool_val(VSimp) == 0 ? false : true;
    ErrString[4096];
    Err = libltl3ba_main_c(PropString, Det, Simp);

    if (Err != 0) {
        sprintf(ErrString, "Failed to translate LTL formula: \"%s\"", PropString);
        caml_failwith(ErrString);
    }

    // All good. 
    CAMLreturn0;
}

CAMLprim void ltl3ba_native_teardown()
{
    CAMLparam0();
    libltl3ba_teardown_c();
    CAMLreturn0;
}

CAMLprim value ltl3ba_native_translate_ba()
{
    CAMLparam0();
    CAMLlocal1(VNodeArray);
    CAMLlocal1(VEdgeArray);
    CAMLlocal3(VLitArray, VCubeArray, VCurLit);
    CAMLlocal2(temp1, temp2);
    int i, j, k, l;

    NodeC* CurNode;
    EdgeC* CurEdge;
    PropC* CurProp;
    CubeC* CurCube;
    PropLiteralC* CurLit;
    
    VNodeArray = caml_alloc(LTL3BAGeneratedCAut->NumNodes, 0);
    for (i = 0; i < LTL3BAGeneratedCAut->NumNodes; ++i) {
        CurNode = LTL3BAGeneratedCAut->Nodes[i];
        temp1 = caml_alloc(4, 0);
        Store_field(temp1, 0, caml_copy_string(CurNode->NodeName));
        Store_field(temp1, 1, CurNode->Initial ? Val_true : Val_false);
        Store_field(temp1, 2, CurNode->Accepting ? Val_true : Val_false);

        VEdgeArray = caml_alloc(CurNode->NumEdges, 0);
        for (j = 0; j < CurNode->NumEdges; ++j) {
            CurEdge = CurNode->Edges[j];
            temp2 = caml_alloc(3, 0);
            Store_field(temp2, 0, caml_copy_string(CurEdge->FromState));

            CurProp = CurEdge->Prop;
            VCubeArray = caml_alloc(CurProp->NumElems, 0);
            for (k = 0; k < CurProp->NumElems; ++k) {
                CurCube = CurProp->Disjuncts[k];
                VLitArray = caml_alloc(CurCube->NumElems, 0);
                for (l = 0; l < CurCube->NumElems; ++l) {
                    CurLit = CurCube->Props[l];
                    VCurLit = caml_alloc(4, 0);
                    if (CurLit->IsTrue) {
                        Store_field(VCurLit, 0, Val_true);
                        Store_field(VCurLit, 1, Val_true);
                        Store_field(VCurLit, 2, caml_copy_string(""));
                        Store_field(VCurLit, 3, Val_false);
                    } else if (CurLit->IsFalse) {
                        Store_field(VCurLit, 0, Val_true);
                        Store_field(VCurLit, 1, Val_false);
                        Store_field(VCurLit, 2, caml_copy_string(""));
                        Store_field(VCurLit, 3, Val_false);
                    } else { 
                        Store_field(VCurLit, 0, Val_false);
                        Store_field(VCurLit, 1, Val_false);
                        Store_field(VCurLit, 2, caml_copy_string(CurLit->Name));
                        if (CurLit->Negated) {
                            Store_field(VCurLit, 3, Val_true);
                        } else {
                            Store_field(VCurLit, 3, Val_false);
                        }
                    }
                    Store_field(VLitArray, l, VCurLit);
                }
                Store_field(VCubeArray, k, VLitArray);
            }

            Store_field(temp2, 1, VCubeArray);
            Store_field(temp2, 2, caml_copy_string(CurEdge->ToState));
            Store_field(VEdgeArray, j, temp2);
        }

        Store_field(temp1, 3, VEdgeArray);
        Store_field(VNodeArray, i, temp1);
    }
    CAMLreturn(VNodeArray);
}
 
/* ltl3ba_stubs.cpp ends here */

/* ltl3ba_stubs.c ends here */
