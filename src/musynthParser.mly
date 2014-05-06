/** MuSynth Parser */

/* Keywords */
%token VAR EQUALS DOT COMMA MAIN SYMMETRICTYPES INIT
%token SEMICOLON COLON CAPACITY MESSAGES
%token DETAUTOMATON AUTOMATON CHANNELAUTOMATON
%token LOSSY LOSSLESS DUPLICATING NONDUPLICATING
%token ORDERED UNORDERED PARTIALAUTOMATON TRANSITIONS
%token INPUTS OUTPUTS DEFINE JUSTICE COMPASSION 
%token FINITELOSS NONBLOCKING BLOCKING CANSYNCON CTLSPEC
%token LTLSPEC INVARIANT INCOMPLETE COMPLETE LBRACE RBRACE
%token LPAREN RPAREN LSQUARE RSQUARE STATES FINITEDUP
%token BCTRUE BCFALSE NEQUALS IN WITH 
%token OR AND NOT IMPLIES IFF TLGLOBAL TLFUTURE TLNEXT 
%token TLUNTIL TLRELEASE
%token FORALL FOREACH EXISTS
%token EOF

%token<MusynthTypes.identifierT> IDENT 
%token<string> STRINGCONST
%token<int> INTCONST

%type<MusynthTypes.musProgT> prog

%start prog

%{
open Lexing
open MusynthTypes
open Parsing
open Format

  
let errmsg item msg =
  let start_pos = if item <= 0 then symbol_start_pos () else rhs_start_pos item in
  let end_pos = if item <= 0 then symbol_end_pos () else rhs_start_pos item in
  let slineno = start_pos.pos_lnum in
  let scol = start_pos.pos_cnum - start_pos.pos_bol in
  let elineno = end_pos.pos_lnum in
  let ecol = end_pos.pos_cnum - end_pos.pos_bol in
  raise (ParseError (msg, (slineno, scol, elineno, ecol)))

let getrhsloc item =
  let startpos = rhs_start_pos item in
  let endpos = rhs_end_pos item in
  let slinenum = startpos.pos_lnum in
  let scolnum = startpos.pos_cnum - startpos.pos_bol in
  let elinenum = endpos.pos_lnum in
  let ecolnum = endpos.pos_cnum - endpos.pos_bol in
  (slinenum, scolnum, elinenum, ecolnum)
    
let getlhsloc () =
  let startpos = symbol_start_pos () in
  let endpos = symbol_end_pos () in
    (startpos.pos_lnum,
     startpos.pos_cnum - startpos.pos_bol,
     endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol)

%}

%%

/* productions */

prog : symTypeDecls msgDecls automatonDecls initStateDecl specs
    { ($1, $2, $3, $4, $5) }
     
symTypeDecls : SYMMETRICTYPES LBRACE symTypeDeclList RBRACE
    { $3 }
    | 
    { [] }

symTypeDeclList : symTypeDeclList oneSymTypeDecl
    { $1 @ [ $2 ] }
    | oneSymTypeDecl
    { [ $1 ] }

oneSymTypeDecl : IDENT COLON symType SEMICOLON
    { ($1, $3) }

symType : IDENT
    { SymTypeNamed ($1, (Some (getlhsloc ()))) }
    | LBRACE identList RBRACE
    { SymTypeAnon ($2, (Some (getlhsloc ()))) }

identList : identList COMMA IDENT
    { $1 @ [ $3 ] }
    | IDENT
    { [ $1 ] }

designator : IDENT
    { SimpleDesignator $1 }
    | designator LSQUARE IDENT RSQUARE
    { IndexDesignator ($1, $3, (Some (getlhsloc ()))) }
    | designator DOT IDENT
    { FieldDesignator ($1, $3, (Some (getlhsloc ()))) }

/* lisp style prefix expressions to avoid ambiguity */
prop : BCTRUE
        { PropTrue (Some (getlhsloc ())) }
    | BCFALSE
        { PropFalse (Some (getlhsloc ())) }
    | IDENT
        { PropDefine $1 }
    | LPAREN EQUALS designator designator RPAREN
        { PropEquals ($3, $4, Some (getlhsloc ())) }
    | LPAREN NEQUALS designator designator RPAREN
        { PropNEquals ($3, $4, Some (getlhsloc ())) }
    | LPAREN AND prop prop RPAREN
        { PropAnd ($3, $4, Some (getlhsloc ())) }
    | LPAREN OR prop prop RPAREN
        { PropOr ($3, $4, Some (getlhsloc ())) }
    | LPAREN IMPLIES prop prop RPAREN
        { PropImplies ($3, $4, Some (getlhsloc ())) }
    | LPAREN IFF prop prop RPAREN
        { PropIff ($3, $4, Some (getlhsloc ())) }
    | LPAREN prop RPAREN
        { $2 }
    | LPAREN FORALL identList IN symType prop RPAREN
        { PropForall ($3, $5, $6, Some (getlhsloc ())) }
    | LPAREN EXISTS identList IN symType prop RPAREN
        { PropExists ($3, $5, $6, Some (getlhsloc ())) }
    | LPAREN TLGLOBAL prop RPAREN
        { PropTLG ($3, Some (getlhsloc ())) }
    | LPAREN TLFUTURE prop RPAREN
        { PropTLF ($3, Some (getlhsloc ())) }
    | LPAREN TLNEXT prop RPAREN
        { PropTLX ($3, Some (getlhsloc ())) }
    | LPAREN TLUNTIL prop prop RPAREN
        { PropTLU ($3, $4, Some (getlhsloc ())) }
    | LPAREN TLRELEASE prop prop RPAREN
        { PropTLR ($3, $4, Some (getlhsloc ())) }

msgDecls : MESSAGES LBRACE msgList RBRACE
        { $3 }

msgList : msgList COMMA oneMsg
        { $1 @ [ $3 ] }
    | oneMsg
        { [ $1 ] }

oneMsg : designator optQuants
        {
          let q = $2 in
          match q with
          | Some (qMap, propOpt) -> DeclQuantified ($1, qMap, propOpt, Some (getlhsloc ()))
          | None -> DeclSimple ($1, Some (getlhsloc ()))
        }
          
optProp : prop
        { Some $1 }
    | /* empty */
        { None }

automatonDecls : automatonDecls oneAutomatonDecl
        { $1 @ [ $2 ] }
    | oneAutomatonDecl
        { [ $1 ] }

oneAutomatonDecl : oneCompAutomatonDecl
        { $1 }
    | oneIncompAutomatonDecl
        { $1 }
    | oneChannelAutomatonDecl
        { $1 }

oneCompAutomatonDecl : AUTOMATON optFairness designator optQuants LBRACE 
        stateDecl optInputDecl optOutputDecl transDecl RBRACE
        {
          let aut = CompleteAutomaton ($3, $6, $7, $8, $9, $2, Some (getlhsloc ())) in
          match $4 with
          | Some (qMap, propOpt) -> DeclQuantified(aut, qMap, propOpt, Some (getlhsloc ()))
          | None -> DeclSimple (aut, Some (getlhsloc ()))
        }

optFairness : WITH JUSTICE
        { FairnessTypeJustice (Some (getrhsloc 2)) }
    | WITH COMPASSION
        { FairnessTypeCompassion (Some (getrhsloc 2)) }
    | /* empty */
        { FairnessTypeNone }

oneIncompAutomatonDecl : PARTIALAUTOMATON optFairness designator optQuants LBRACE
        stateDecl optInputDecl optOutputDecl transDecl RBRACE
        {
          let aut = IncompleteAutomaton ($3, $6, $7, $8, $9, $2, Some (getlhsloc ())) in
          match $4 with
          | Some (qMap, propOpt) -> DeclQuantified(aut, qMap, propOpt, Some (getlhsloc ()))
          | None -> DeclSimple (aut, Some (getlhsloc ()))
        }

oneChannelAutomatonDecl : CHANNELAUTOMATON optFairness optLossQuant optDupQuant 
                          designator optQuants LBRACE
        chanPropDecl MESSAGES LBRACE msgList RBRACE SEMICOLON RBRACE
        {
          let aut = ChannelAutomaton ($5, $8, $11, $2, $3, $4, Some (getlhsloc ())) in
          match $6 with
          | Some (qMap, propOpt) -> DeclQuantified(aut, qMap, propOpt, Some (getlhsloc ()))
          | None -> DeclSimple (aut, Some (getlhsloc ()))
        }

optLossQuant : FINITELOSS
        { LossFairnessFinite (Some (getlhsloc ())) }
    | /* empty */
        { LossFairnessNone }

optDupQuant : FINITEDUP
        { DupFairnessFinite (Some (getlhsloc ())) }
    | /* empty */
        { DupFairnessNone }

chanPropDecl : orderDecl COMMA lossDecl COMMA dupDecl COMMA blockDecl SEMICOLON chanCapDecl
        { ($1, $3, $5, $7, $9) }

blockDecl : NONBLOCKING
        { ChanNonBlocking (Some (getlhsloc ())) }
    | BLOCKING
        { ChanBlocking (Some (getlhsloc ())) }

orderDecl : ORDERED
        { ChanOrdered (Some (getlhsloc ())) }
    | UNORDERED
        { ChanUnordered (Some (getlhsloc ())) }

lossDecl : LOSSLESS 
        { ChanLossless (Some (getlhsloc ())) }
    | LOSSY
        { ChanLossy (Some (getlhsloc ())) }

dupDecl : DUPLICATING
        { ChanDuplicating (Some (getlhsloc ())) }
    | NONDUPLICATING
        { ChanNonDuplicating (Some (getlhsloc ())) }

chanCapDecl : CAPACITY EQUALS INTCONST SEMICOLON
        { $3 }

optQuants : quantList optProp
          { Some ($1, $2) }
    | /* empty */
          { None }

quantList : quantList oneQuant
        {
          let idlist, typ = $2 in
          List.fold_left
            (fun map ident ->
              if IdentMap.mem ident map then
                let id, loc = ident in
                raise (SemanticError ("Quantified identifier " ^ id ^ " mapped more than once",
                                      (Some (getrhsloc 2))))
              else
                IdentMap.add ident typ map)
            $1 idlist
        }
    | oneQuant
      {
        let idlist, typ = $1 in
        List.fold_left
          (fun map ident ->
            if IdentMap.mem ident map then
              let id, loc = ident in
              raise (SemanticError ("Quantified identifier " ^ id ^ " mapped more than once",
                                    (Some (getrhsloc 2))))
            else
              IdentMap.add ident typ map)
          IdentMap.empty idlist
      }

oneQuant : FOREACH identList IN symType
        { ($2, $4) }

stateAnnotation : 
    | COLON COMPLETE
        { AnnotComplete (Some (getlhsloc ())) }
    | COLON INCOMPLETE
        { AnnotIncomplete (Some (getlhsloc ())) }
    | COLON INCOMPLETE INTCONST
        { AnnotIncompleteNum ($3, Some (getlhsloc ())) }
    | COLON INCOMPLETE LPAREN msgList RPAREN
        { AnnotIncompleteEventList ($4, Some (getlhsloc ())) }
    | COLON INCOMPLETE INTCONST LPAREN msgList RPAREN
        { AnnotIncompleteNumEventList ($3, $5, Some (getlhsloc ())) }
    | /* empty */
        { AnnotNone }
      
stateDecl : STATES COLON LBRACE stateDecls RBRACE SEMICOLON
        { $4 }

stateDecls : stateDecls COMMA oneStateDecl
        { $1 @ [ $3 ] }
    | oneStateDecl
        { [ $1 ] }

oneStateDecl : designator optQuants stateAnnotation
        {
          match $2 with
          | Some (qMap, propOpt) -> (DeclQuantified ($1, qMap, propOpt, Some (getlhsloc ())), $3)
          | None -> (DeclSimple ($1, Some (getlhsloc ())), $3)
        }

initStateDecl : INIT LBRACE propList RBRACE
        { $3 }

optOutputDecl : OUTPUTS LBRACE msgList RBRACE SEMICOLON
        { $3 }
    | /* empty */
        { [] }

optInputDecl : INPUTS LBRACE msgList RBRACE SEMICOLON
        { $3 }
    | /* empty */
        { [] }

transDecl : TRANSITIONS LBRACE transList RBRACE SEMICOLON
        { $3 }

transList : transList COMMA oneTrans
        { $1 @ [ $3 ] }
    | oneTrans
        { [ $1 ] }

oneTrans : LPAREN designator COMMA designator COMMA designator optQuants RPAREN
        { 
          match $7 with
          | Some (qMap, propOpt) -> DeclQuantified (($2, $4, $6), qMap, propOpt, Some (getlhsloc ()))
          | None -> DeclSimple (($2, $4, $6), Some (getlhsloc ()))
        }

specs : specs oneSpec
        { $1 @ [ $2 ] }
    | /* empty */
        { [] }

oneSpec : invariant
        { $1 }
    | ltlspec
        { $1 }
    | definespec
        { $1 }

invariant : INVARIANT STRINGCONST LBRACE prop RBRACE
        { SpecInvar ($2, $4, Some (getlhsloc ())) }

ltlspec : LTLSPEC STRINGCONST LBRACE prop optJustice optCompassion RBRACE
        { SpecLTL ($2, $4, $5, $6, Some (getlhsloc ())) }

optJustice : JUSTICE LPAREN propPairList RPAREN
        { $3 }
    | /* empty */
        { [] }

optCompassion : COMPASSION LPAREN propPairList RPAREN
        { $3 }
    | /* empty */
        { [] }

propPairList : propPairList COMMA propPair
        { $1 @ [ $3 ] }
    | propPair
        { [ $1 ] }

propPair : LPAREN prop COMMA prop RPAREN
        { ( $2, $4) }

propList : propList COMMA prop
        { $1 @ [ $3 ] }
    | prop
        { [ $1 ] }

definespec : DEFINE IDENT prop
        { SpecDefine ($2, $3, Some (getlhsloc ())) }
