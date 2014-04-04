// ltl3ba_stubs.cpp --- 
// 
// Filename: ltl3ba_stubs.cpp
// Author: Abhishek Udupa
// Created: Thu Apr  3 12:41:21 2014 (-0400)
// 
// 
// Copyright (c) 2013, Abhishek Udupa, University of Pennsylvania
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. All advertising materials mentioning features or use of this software
//    must display the following acknowledgement:
//    This product includes software developed by The University of Pennsylvania
// 4. Neither the name of the University of Pennsylvania nor the
//    names of its contributors may be used to endorse or promote products
//    derived from this software without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ''AS IS'' AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// 
// 

// Code:

// Stubs for ltl3ba libmain to be used from ocaml

#include "ltl3ba.h"

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

extern "C" CAMLprim void ltl3ba_native_mk_ba(value VPropString, value VDet, value VSimp);
extern "C" CAMLprim void ltl3ba_native_teardown();
extern "C" CAMLprim value ltl3ba_native_translate_ba();

// Global pointer to generated automaton
BAutomaton* __LTL3BA_Generated_Automaton__ = nullptr;

CAMLprim void ltl3ba_native_mk_ba(value VPropString, value VDet, value VSimp)
{
    CAMLparam3(VPropString, VDet, VSimp);
    std::string PropString(String_val(VPropString));
    bool Det = Bool_val(VDet) == 0 ? false : true;
    bool Simp = Bool_val(VSimp) == 0 ? false : true;
    BAutomaton Aut;
    auto Err = libltl3ba_main(PropString, Aut, Det, Simp);
    if (Err != 0) {
        std::string ErrString = (std::string)"Failed to translate LTL formula: \"" + 
            PropString + "\" to a buchi automaton";
        caml_failwith(ErrString.c_str());
    }

    // All good. 
    __LTL3BA_Generated_Automaton__ = new BAutomaton();
    *__LTL3BA_Generated_Automaton__ = Aut;
    CAMLreturn0;
}

CAMLprim void ltl3ba_native_teardown()
{
    CAMLparam0();
    if (__LTL3BA_Generated_Automaton__ != nullptr) {
        delete __LTL3BA_Generated_Automaton__;
        __LTL3BA_Generated_Automaton__ = nullptr;
    }
    CAMLreturn0;
}

CAMLprim value ltl3ba_native_translate_ba()
{
    CAMLparam0();
    CAMLlocal1(VNodeArray);
    CAMLlocal1(VEdgeArray);
    CAMLlocal2(VLitArray, VCubeArray);
    CAMLlocal3(temp1, temp2, temp3);
    
    VNodeArray = caml_alloc(__LTL3BA_Generated_Automaton__->size(), 0);
    size_t i = 0;
    for (auto const& NameNodePair : *__LTL3BA_Generated_Automaton__) {
        auto const& Node = NameNodePair.second;
        temp1 = caml_alloc(4, 0);
        Store_field(temp1, 0, caml_copy_string(Node.NodeName.c_str()));
        Store_field(temp1, 1, Node.Initial ? Val_true : Val_false);
        Store_field(temp1, 2, Node.Accepting ? Val_true : Val_false);

        VEdgeArray = caml_alloc(Node.Edges.size(), 0);
        size_t j = 0;
        for (auto const& Edge : Node.Edges) {
            temp2 = caml_alloc(3, 0);
            Store_field(temp2, 0, caml_copy_string(Edge.FromState.c_str()));
            
            VCubeArray = caml_alloc(Edge.Disjuncts.size(), 0);
            size_t k = 0;
            for (auto const& CurCube : Edge.Disjuncts) {
                VLitArray = caml_alloc(CurCube.size(), 0);
                size_t l = 0;
                for (auto const& Lit : CurCube) {
                    if (Lit.IsTrue) {
                        Store_field(VLitArray, l, Val_int(0));
                    } else if (Lit.IsFalse) {
                        Store_field(VLitArray, l, Val_int(1));
                    } else {
                        temp3 = caml_alloc(2, 0);
                        Store_field(temp3, 0, caml_copy_string(Lit.Name.c_str()));
                        if (Lit.Negated) {
                            Store_field(temp3, 1, true);
                        } else {
                            Store_field(temp3, 1, false);
                        }
                        Store_field(VLitArray, l, temp3);
                    }
                    l++;
                }
                Store_field(VCubeArray, k, VLitArray);
                k++;
            }

            Store_field(temp2, 1, VCubeArray);
            Store_field(temp2, 2, caml_copy_string(Edge.ToState.c_str()));
            Store_field(VEdgeArray, j, temp2);
            j++;
        }

        Store_field(temp1, 3, VEdgeArray);
        Store_field(VNodeArray, i, VEdgeArray);
        i++;
    }
    CAMLreturn(VNodeArray);
}

// 
// ltl3ba_stubs.cpp ends here
