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

value ProcessString (const std::string& Str)
{
    CAMLlocal1(Retval);
    Retval = caml_copy_string(Str.c_str());
    return Retval;
}

value ProcessCube (const Cube& TheCube)
{

}

value ProcessCubeList (const std::vector<Cube>& Disjuncts)
{
    
}

value ProcessOneEdge (const BAEdge& Edge)
{
    CAMLlocal1(Retval);
    Retval = caml_alloc(0, 3);
    Store_field(Retval, 0, ProcessString(Edge.FromState));
    Store_field(Retval, 1, ProcessCubeList(Edge.Disjuncts));
    Store_field(Retval, 2, ProcessString(Edge.ToState));
    return Retval;
}

value ProcessEdgeList (const std::vector<BAEdge>& Edges)
{
    for (auto const& Edge : Edges) {
        
    }
}


value ProcessOneNode (const BANode& Node)
{
    CAMLlocal2(Retval);
    Retval = caml_alloc(0, 4);
    Field(Retval, 0) = ProcessString(Node.NodeName);
    Field(Retval, 1) = Node.Initial ? Val_true : Val_false;
    Field(Retval, 2) = Node.Accepting ? Val_true : Val_false;
    Field(Retval, 3) = ProcessEdgeList(Node.Edges);
    return Retval;
}

value ProcessNodes(const BAutomaton& Aut)
{
    CAMLlocal2(Retval, CurNode)
    for (auto it = Aut.rbegin(); it != Aut.rend(); ++it) {
        CurNode = caml_alloc(2, 0);
        Field(CurNode, 0) = ProcessOneNode()
    }
}

CAMLprim value ltl3ba_native(value PropString, val Det, val Simp)
{
    CAMLparam3(PropString, Det, Simp);
    std::string StringProp(String_val(PropString));
    bool BoolDet = Bool_val(Det) == 0 ? false : true;
    bool BoolSimp = Bool_val(Simp) == 0 ? false : true;
    BAutomaton Aut;
    int Retval = libltl3ba_main(StringProp, BAutomaton &Aut, BoolDet, BoolSimp);
    if (Retval != 0) {
        caml_invalid_argument("Conversion of LTL specification to BA failed");
    }

    // All good
}

// 
// ltl3ba_stubs.cpp ends here
