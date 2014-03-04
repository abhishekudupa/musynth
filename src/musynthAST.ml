(* types and functions for the parse tree *)

open Format

(* file name * start line num * start col * end line num * end col *)
type sourcelocation = string * int * int * int * int

(* Filename * Line Num * Col num *)
exception ParseError of string * sourcelocation
exception SemanticError of string * sourcelocation

module StringMap = Map.Make
    (struct 
      type t = string 
      let compare = Pervasives.compare 
    end)

type identifierT = string * sourcelocation option

module IdentMap = Map.Make 
    (struct 
      type t = identifierT 
      let compare = fun ident1 ident2 -> 
        let id1, loc1 = ident1 in
        let id2, loc2 = ident2 in
        Pervasives.compare id1 id2
    end)

type musSymTypeT = 
  | SymTypeNamed of identifierT
  | SymTypeAnon of identifierT list

type musSymTypeDeclT = identifierT * musSymTypeT

type musSymTypeDeclBlockT = musSymTypeDeclT list

type musDesignatorT =
  | SimpleDesignator of identifierT
  | IndexDesignator of musDesignatorT * identifierT
  | FieldDesignator of musDesignatorT * identifierT

(* Only CTL at present *)
type musPropT =
  | PropTrue
  | PropFalse
  | PropEquals of (musDesignatorT * musDesignatorT)
  | PropNEquals of (musDesignatorT * musDesignatorT)
  | PropAnd of musPropT * musPropT
  | PropOr of musPropT * musPropT
  | PropImplies of musPropT * musPropT
  | PropIff of musPropT * musPropT
  | PropForall of identifierT list * musSymTypeT * musPropT
  | PropExists of identifierT list * musSymTypeT * musPropT
  | PropCTLAG of musPropT
  | PropCTLAF of musPropT
  | PropCTLAX of musPropT
  | PropCTLEG of musPropT
  | PropCTLEF of musPropT
  | PropCTLEX of musPropT
  | PropCTLAU of musPropT * musPropT
  | PropCTLEU of musPropT * musPropT

type 'a musDeclType = 
  | DeclSimple of 'a
  | DeclQuantified of 'a * (musSymTypeT IdentMap.t) * musPropT option

type musMsgDeclT = musDesignatorT musDeclType

type musMsgDeclBlockT = musMsgDeclT list

type musStateAnnotationT = 
  | AnnotNone
  | AnnotComplete
  | AnnotIncomplete
  | AnnotIncompleteEventList of musMsgDeclT list
  | AnnotIncompleteNum of int
  | AnnotIncompleteNumEventList of int * musMsgDeclT list

type musStateDeclT = musDesignatorT musDeclType * musStateAnnotationT

type musStateDeclBlockT = musStateDeclT list

type musTransDeclT = (musDesignatorT * musDesignatorT * musDesignatorT) musDeclType

type musTransDeclBlockT = musTransDeclT list

type musChanDupT = 
  | ChanDuplicating
  | ChanNonDuplicating

type musChanOrdT = 
  | ChanOrdered
  | ChanUnordered

type musChanLossT = 
  | ChanLossy
  | ChanLossless

type musChanPropT = musChanOrdT * musChanLossT * musChanDupT * int

type musAutomatonDeclType = 
  | CompleteAutomaton of musDesignatorT * musStateDeclBlockT * musStateDeclBlockT * 
        musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT
  | IncompleteAutomaton of musDesignatorT * musStateDeclBlockT * musStateDeclBlockT * 
        musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT
  | ChannelAutomaton of musDesignatorT * musChanPropT * musMsgDeclBlockT

type musAutomatonDeclT = musAutomatonDeclType musDeclType

type musSpecT = 
  | SpecInvar of string * musPropT
  | SpecCTL of string * musPropT

type musProgT = musSymTypeDeclBlockT * musAutomatonDeclT list * musSpecT list
