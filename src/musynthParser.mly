/** MuSynth Parser */

/* Keywords */
%token VAR EQUALS DOT COMMA MAIN SYMMETRICTYPES INITSTATES
%token SEMICOLON COLON CAPACITY MESSAGES
%token DETAUTOMATON AUTOMATON CHANNELAUTOMATON
%token LOSSY LOSSLESS DUPLICATING NONDUPLICATING
%token ORDERED UNORDERED PARTIALAUTOMATON TRANSITIONS
%token INPUTS OUTPUTS DEFINE FAIRNESS CANSYNCON CTLSPEC
%token LTLSPEC INVARIANT INCOMPLETE COMPLETE LBRACE RBRACE
%token LPAREN RPAREN LSQUARE RSQUARE STATES
%token BCTRUE BCFALSE NEQUALS IN TLUNTIL
%token OR AND NOT IMPLIES IFF TLAG TLAF TLAX TLEX TLEG TLEF TLAU TLEU
%token TLFORALL TLEXISTS TLGLOBAL TLFUTURE TLNEXT
%token TLFUTURE TLGLOBAL FORALL FOREACH EXISTS
%token EOF

%token<MusynthAST.identifierT> IDENT 
%token<string> STRINGCONST
%token<int> INTCONST

%type<MusynthAST.musProgT> prog

%start prog

%{
open Lexing
open MusynthAST
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

prog : symTypeDecls automatonDecls specs
    { ($1, $2, $3) }

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
        { SymTypeNamed $1 }
    | LBRACE identList RBRACE
        { SymTypeAnon $2 }

identList : identList COMMA IDENT
        { $1 @ [ $3 ] }
    | IDENT
        { [ $1 ] }

designator : IDENT
        { SimpleDesignator $1 }
    | designator LSQUARE IDENT RSQUARE
        { IndexDesignator ($1, $3) }
    | designator DOT IDENT
        { FieldDesignator ($1, $3) }

/* lisp style prefix expressions to avoid ambiguity */
prop : BCTRUE
        { PropTrue }
    | BCFALSE
        { PropFalse }
    | LPAREN EQUALS designator designator RPAREN
        { PropEquals ($3, $4) }
    | LPAREN NEQUALS designator designator RPAREN
        { PropNEquals ($3, $4) }
    | LPAREN AND prop prop RPAREN
        { PropAnd ($3, $4) }
    | LPAREN OR prop prop RPAREN
        { PropOr ($3, $4) }
    | LPAREN IMPLIES prop prop RPAREN
        { PropImplies ($3, $4) }
    | LPAREN IFF prop prop RPAREN
        { PropIff ($3, $4) }
    | LPAREN prop RPAREN
        { $2 }
    | LPAREN FORALL identList IN symType prop RPAREN
        { PropForall ($3, $5, $6) }
    | LPAREN EXISTS identList IN symType prop RPAREN
        { PropExists ($3, $5, $6) }
    | LPAREN TLAG prop RPAREN
        { PropCTLAG $3 }
    | LPAREN TLAF prop RPAREN
        { PropCTLAF $3 }
    | LPAREN TLAX prop RPAREN
        { PropCTLAX $3 }
    | LPAREN TLEG prop RPAREN
        { PropCTLEG $3 }
    | LPAREN TLEF prop RPAREN
        { PropCTLEF $3 }
    | LPAREN TLEX prop RPAREN
        { PropCTLEX $3 }
    | LPAREN TLAU prop prop RPAREN
        { PropCTLAU ($3, $4) }
    | LPAREN TLEU prop prop RPAREN
        { PropCTLEU ($3, $4) }

msgList : msgList COMMA oneMsg
        { $1 @ [ $3 ] }
    | oneMsg
        { [ $1 ] }

oneMsg : designator optQuants
        {
          let q = $2 in
          match q with
          | Some (qMap, propOpt) -> DeclQuantified ($1, qMap, propOpt)
          | None -> DeclSimple $1
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

oneCompAutomatonDecl : AUTOMATON designator optQuants LBRACE 
        stateDecl initStateDecl optInputDecl optOutputDecl transDecl RBRACE
        {
          let aut = CompleteAutomaton ($2, $5, $6, $7, $8, $9) in
          match $3 with
          | Some (qMap, propOpt) -> DeclQuantified(aut, qMap, propOpt)
          | None -> DeclSimple aut
        }

oneIncompAutomatonDecl : PARTIALAUTOMATON designator optQuants LBRACE
        stateDecl initStateDecl optInputDecl optOutputDecl transDecl RBRACE
        {
          let aut = IncompleteAutomaton ($2, $5, $6, $7, $8, $9) in
          match $3 with
          | Some (qMap, propOpt) -> DeclQuantified(aut, qMap, propOpt)
          | None -> DeclSimple aut
        }

oneChannelAutomatonDecl : CHANNELAUTOMATON designator optQuants LBRACE
        chanPropDecl MESSAGES LBRACE msgList RBRACE SEMICOLON RBRACE
        {
          let aut = ChannelAutomaton ($2, $5, $8) in
          match $3 with
          | Some (qMap, propOpt) -> DeclQuantified(aut, qMap, propOpt)
          | None -> DeclSimple aut
        }

chanPropDecl : orderDecl COMMA lossDecl COMMA dupDecl SEMICOLON chanCapDecl
        { ($1, $3, $5, $7) }

orderDecl : ORDERED
        { ChanOrdered }
    | UNORDERED
        { ChanUnordered }

lossDecl : LOSSLESS 
        { ChanLossless }
    | LOSSY
        { ChanLossy }

dupDecl : DUPLICATING
        { ChanDuplicating }
    | NONDUPLICATING
        { ChanNonDuplicating }

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
                                      getrhsloc 2))
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
                                    getrhsloc 2))
            else
              IdentMap.add ident typ map)
          IdentMap.empty idlist
      }

oneQuant : FOREACH identList IN symType
        { ($2, $4) }

stateAnnotation : 
    | COLON COMPLETE
        { AnnotComplete }
    | COLON INCOMPLETE
        { AnnotIncomplete }
    | COLON INCOMPLETE INTCONST
        { AnnotIncompleteNum $3 }
    | COLON INCOMPLETE LPAREN msgList RPAREN
        { AnnotIncompleteEventList $4 }
    | COLON INCOMPLETE INTCONST LPAREN msgList RPAREN
        { AnnotIncompleteNumEventList ($3, $5) }
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
          | Some (qMap, propOpt) -> (DeclQuantified ($1, qMap, propOpt), $3)
          | None -> (DeclSimple $1, $3)
        }

initStateDecl : INITSTATES COLON LBRACE initStateDecls RBRACE SEMICOLON
        { $4 }

initStateDecls : initStateDecls COMMA oneInitStateDecl
        { $1 @ [ $3 ] }
    | oneInitStateDecl
        { [ $1 ] }

oneInitStateDecl : designator optQuants
        {
          match $2 with
          | Some (qMap, propOpt) -> (DeclQuantified ($1, qMap, propOpt), AnnotNone)
          | None -> (DeclSimple $1, AnnotNone)
        }

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
          | Some (qMap, propOpt) -> DeclQuantified (($2, $4, $6), qMap, propOpt)
          | None -> DeclSimple (($2, $4, $6))
        }

specs : specs oneSpec
        { $1 @ [ $2 ] }
    | /* empty */
        { [] }

oneSpec : invariant
        { $1 }
    | ctlspec
        { $1 }

invariant : INVARIANT STRINGCONST LBRACE prop RBRACE
        { SpecInvar ($2, $4) }

ctlspec : CTLSPEC STRINGCONST LBRACE prop RBRACE
        { SpecCTL ($2, $4) }
