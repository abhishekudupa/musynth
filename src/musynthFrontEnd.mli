module Lower :
  sig
    module ST :
      sig
        val createSymTab : unit -> 'a MusynthTypes.IdentMap.t ref list ref
        val push : 'a MusynthTypes.IdentMap.t ref list ref -> unit
        val pushScope : 'a list ref -> 'a -> unit
        val pop : 'a list ref -> 'a
        val peek : 'a list ref -> 'a
        val getNumScopes : 'a list ref -> int
        val lookup :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a option
        val bind :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a -> unit
        val bindGlobal :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a -> unit
        val lookupOrFail :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a
        val lookupVar :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
        val lookupType :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
        val lookupGMsg :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupAMsg :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupState :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupAutomaton :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupConst :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
      end
    module CK :
      sig
        module ST :
          sig
            val createSymTab :
              unit -> 'a MusynthTypes.IdentMap.t ref list ref
            val push : 'a MusynthTypes.IdentMap.t ref list ref -> unit
            val pushScope : 'a list ref -> 'a -> unit
            val pop : 'a list ref -> 'a
            val peek : 'a list ref -> 'a
            val getNumScopes : 'a list ref -> int
            val lookup :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a option
            val bind :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a -> unit
            val bindGlobal :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a -> unit
            val lookupOrFail :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a
            val lookupVar :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
            val lookupType :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
            val lookupGMsg :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupAMsg :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupState :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupAutomaton :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupConst :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
          end
        val curAutomatonName : string ref
        val resolveSymType :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT
        val checkSymTypeDecl :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT -> unit
        val destructDesigDecl :
          MusynthTypes.musDesignatorT ->
          MusynthTypes.identifierT * MusynthTypes.identifierT list
        val getObligationsForIdent :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key ->
          (string * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musPropT option
        val getTypeObligationsForIdent :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT list
        val checkTypeLists :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          string * MusynthTypes.sourcelocation option ->
          MusynthTypes.musSymTypeT list ->
          MusynthTypes.IdentMap.key list -> unit
        val getDesigType :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry
        val checkTypeCompatibility :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT ->
          MusynthTypes.musDesignatorT ->
          MusynthTypes.sourcelocation option ->
          MusynthTypes.symtabEntry * MusynthTypes.symtabEntry
        val checkPureProp :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
        val checkPureQProp :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
        val desigDeclChecker :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.musDesignatorT ->
          'a ->
          MusynthTypes.identifierT *
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
        val transChecker :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
          MusynthTypes.musDesignatorT ->
          MusynthTypes.sourcelocation option -> string * 'a list
        val autMsgDeclChecker :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT ->
          MusynthTypes.sourcelocation option ->
          MusynthTypes.identifierT *
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
        val checkDecl :
          MusynthTypes.symTabScope list ref ->
          (MusynthTypes.symTabScope list ref ->
           'a -> MusynthTypes.sourcelocation option -> 'b * 'c) ->
          'a MusynthTypes.musDeclType ->
          'b * 'c * MusynthTypes.musPropT option * MusynthTypes.symTabScope
        val cvtParamTypeListForSymtab :
          (('a * 'b) * 'c) list -> ('a * 'c) list
        val checkGlobalMsgDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkStateDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkAutomatonMsgDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.msgType ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkAnnotationMsgDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkAnnotation :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musStateAnnotationT -> unit
        val checkStateDeclBlock :
          MusynthTypes.symTabScope list ref ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> bool -> unit
        val checkGlobalMsgDeclBlock :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val checkAutomatonMsgDeclBlock :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.msgType ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val convertDesigToPrimed :
          MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
        val convertDesigDeclToPrimed :
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType
        val checkAutDef :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musAutomatonDeclType ->
          'a ->
          MusynthTypes.identifierT *
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
        val initStateDeclChecker :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musPropT list -> unit
        val checkProp :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
        val checkSpec :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musSpecT -> unit
        val checkProg :
          MusynthTypes.symTabScope list ref ->
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val checkLLProg :
          MusynthTypes.LLDesigSet.elt list * MusynthTypes.llAutomatonT list *
          'a * 'b -> unit
      end
    module Utils :
      sig
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module MSSet :
          sig
            type elt = int MusynthTypes.LLDesigMap.t
            type t = MusynthUtils.MSSet.t
            val empty : t
            val is_empty : t -> bool
            val mem : elt -> t -> bool
            val add : elt -> t -> t
            val singleton : elt -> t
            val remove : elt -> t -> t
            val union : t -> t -> t
            val inter : t -> t -> t
            val diff : t -> t -> t
            val compare : t -> t -> int
            val equal : t -> t -> bool
            val subset : t -> t -> bool
            val iter : (elt -> unit) -> t -> unit
            val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
            val for_all : (elt -> bool) -> t -> bool
            val exists : (elt -> bool) -> t -> bool
            val filter : (elt -> bool) -> t -> t
            val partition : (elt -> bool) -> t -> t * t
            val cardinal : t -> int
            val elements : t -> elt list
            val min_elt : t -> elt
            val max_elt : t -> elt
            val choose : t -> elt
            val split : elt -> t -> t * bool * t
          end
        val splatList : 'a -> int -> 'a list
        val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
        val msToList :
          int MusynthTypes.LLDesigMap.t -> MusynthTypes.LLDesigMap.key list
        val msToStr :
          (MusynthTypes.LLDesigMap.key -> string) ->
          int MusynthTypes.LLDesigMap.t -> string
        val addToMs :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val delFromMS :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val makeEmptyMSWithAlphabet :
          MusynthTypes.LLDesigMap.key list -> int MusynthTypes.LLDesigMap.t
        val enumerateMS :
          MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
        val enumerateLists : 'a list -> int -> 'a list list
        val listToStr : ('a -> string) -> 'a list -> string
        val nextuid : int ref
        val getuid : unit -> int
        val resetuid : unit -> unit
        val crossProduct : 'a list list -> 'a list list
        val identConstPairList2Map :
          (MusynthTypes.IdentMap.key * 'a) list -> 'a MusynthTypes.IdentMap.t
        val sDesigToIdent :
          MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
        val conjoinPropOpts :
          MusynthTypes.musPropT option ->
          MusynthTypes.musPropT option -> MusynthTypes.musPropT option
        val mergeIdentMaps :
          'a MusynthTypes.IdentMap.t ->
          'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
        val evalProp :
          MusynthTypes.musPropT -> ('a * 'b) MusynthTypes.IdentMap.t -> bool
        val getMapsForProp :
          MusynthTypes.IdentMap.key list ->
          MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
          MusynthTypes.musPropT option ->
          MusynthTypes.identifierT MusynthTypes.IdentMap.t list
        val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
        val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
        val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
        val getMsgsForAut :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
        val getNameForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
        val getTransitionsForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
        val getStatesForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
        val getAutomatonByName :
          MusynthTypes.llAutomatonT list ->
          MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
        val getFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
        val getLFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
        val getDFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
        val getSender :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
        val getReceivers :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT list
        val getStateNameForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getStateNamePForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getCSPredsForMsg :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
        val getCSPredsForMsgAll :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
        val getMsgsToSyncOnFromState :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val getStatesFromWhichMsgSync :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val canonicalizeProp : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val canonicalizePropFP : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val makeFormatterOfName : string -> out_channel * Format.formatter
        val makeConjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
        val makeDisjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
      end
    module AST :
      sig
        val pLoc : Format.formatter -> int * int * int * int -> unit
        val pLocOpt :
          Format.formatter -> (int * int * int * int) option -> unit
        val pIdentifier : Format.formatter -> string * 'a -> unit
        val identToName : 'a * 'b -> 'a
        val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
        val pList :
          string ->
          bool ->
          bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> 'a list -> unit
        val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
        val pSymTypeDecl :
          Format.formatter ->
          (string * 'a) * MusynthTypes.musSymTypeT -> unit
        val musMakeIndentedBox :
          Format.formatter ->
          (Format.formatter -> 'a -> unit) ->
          'a ->
          (Format.formatter -> 'b -> unit) ->
          'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
        val pSymTypeDeclBlock :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
        val pDesignator :
          Format.formatter -> MusynthTypes.musDesignatorT -> unit
        val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
        val pPropOpt :
          Format.formatter -> MusynthTypes.musPropT option -> unit
        val pDecl :
          (Format.formatter -> 'a -> 'b) ->
          (Format.formatter -> 'a -> 'c) ->
          Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
        val noopPrinter : 'a -> 'b -> unit
        val pMsgDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val pMsgDeclBlock :
          string ->
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pMessagesDeclBlock :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pStateAnnot :
          Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
        val pStateDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
          MusynthTypes.musStateAnnotationT -> unit
        val pStateDeclBlock :
          string ->
          Format.formatter ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> unit
        val pTransDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType -> unit
        val pTransDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType list -> unit
        val pInitStateConstraint :
          Format.formatter -> MusynthTypes.musPropT -> unit
        val pInitStateDeclBlock :
          Format.formatter -> MusynthTypes.musPropT list -> unit
        val pChanProp :
          Format.formatter ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
          unit
        val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
        val pLossFairness :
          Format.formatter -> MusynthTypes.musLossFairnessT -> unit
        val pDupFairness :
          Format.formatter -> MusynthTypes.musDupFairnessT -> unit
        val pAutomatonDecl :
          Format.formatter ->
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
        val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
        val pProg :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val pLLDesignator :
          Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLVar :
          Format.formatter ->
          MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
        val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
        val pLLFairness :
          Format.formatter -> MusynthTypes.llFairnessT -> unit
        val pLLDupFairness :
          Format.formatter -> MusynthTypes.llDupFairnessT -> unit
        val pLLLossFairness :
          Format.formatter -> MusynthTypes.llLossFairnessT -> unit
        val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
        val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
        val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
        val pLLAutomaton :
          Format.formatter -> MusynthTypes.llAutomatonT -> unit
        val pLLProg :
          Format.formatter ->
          MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
          MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
      end
    module Chan :
      sig
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module Utils :
          sig
            module AST :
              sig
                val pLoc : Format.formatter -> int * int * int * int -> unit
                val pLocOpt :
                  Format.formatter -> (int * int * int * int) option -> unit
                val pIdentifier : Format.formatter -> string * 'a -> unit
                val identToName : 'a * 'b -> 'a
                val astToString :
                  (Format.formatter -> 'a -> 'b) -> 'a -> string
                val pList :
                  string ->
                  bool ->
                  bool ->
                  (Format.formatter -> 'a -> unit) ->
                  Format.formatter -> 'a list -> unit
                val pSymType :
                  Format.formatter -> MusynthTypes.musSymTypeT -> unit
                val pSymTypeDecl :
                  Format.formatter ->
                  (string * 'a) * MusynthTypes.musSymTypeT -> unit
                val musMakeIndentedBox :
                  Format.formatter ->
                  (Format.formatter -> 'a -> unit) ->
                  'a ->
                  (Format.formatter -> 'b -> unit) ->
                  'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
                val pSymTypeDeclBlock :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
                val pDesignator :
                  Format.formatter -> MusynthTypes.musDesignatorT -> unit
                val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
                val pPropOpt :
                  Format.formatter -> MusynthTypes.musPropT option -> unit
                val pDecl :
                  (Format.formatter -> 'a -> 'b) ->
                  (Format.formatter -> 'a -> 'c) ->
                  Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
                val noopPrinter : 'a -> 'b -> unit
                val pMsgDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val pMsgDeclBlock :
                  string ->
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pMessagesDeclBlock :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pStateAnnot :
                  Format.formatter ->
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDeclBlock :
                  string ->
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                   MusynthTypes.musStateAnnotationT)
                  list -> unit
                val pTransDecl :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType -> unit
                val pTransDeclBlock :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType list -> unit
                val pInitStateConstraint :
                  Format.formatter -> MusynthTypes.musPropT -> unit
                val pInitStateDeclBlock :
                  Format.formatter -> MusynthTypes.musPropT list -> unit
                val pChanProp :
                  Format.formatter ->
                  MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
                  MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
                  int -> unit
                val pFairness :
                  Format.formatter -> MusynthTypes.musFairnessT -> unit
                val pLossFairness :
                  Format.formatter -> MusynthTypes.musLossFairnessT -> unit
                val pAutomatonDecl :
                  Format.formatter ->
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
                  unit
                val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
                val pProg :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list *
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType
                  list * MusynthTypes.musPropT list *
                  MusynthTypes.musSpecT list -> unit
                val pLLDesignator :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLIdent :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLVar :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t ->
                  unit
                val pLLAnnot :
                  Format.formatter -> MusynthTypes.llAnnotT -> unit
                val pLLTrans :
                  Format.formatter -> MusynthTypes.llTransT -> unit
                val pLLProp :
                  Format.formatter -> MusynthTypes.llPropT -> unit
                val pLLSpec :
                  Format.formatter -> MusynthTypes.llSpecT -> unit
                val pLLAutomaton :
                  Format.formatter -> MusynthTypes.llAutomatonT -> unit
                val pLLProg :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT list *
                  MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
                  MusynthTypes.llSpecT list -> unit
              end
            module MSSet :
              sig
                type elt = int MusynthTypes.LLDesigMap.t
                type t = MusynthUtils.MSSet.t
                val empty : t
                val is_empty : t -> bool
                val mem : elt -> t -> bool
                val add : elt -> t -> t
                val singleton : elt -> t
                val remove : elt -> t -> t
                val union : t -> t -> t
                val inter : t -> t -> t
                val diff : t -> t -> t
                val compare : t -> t -> int
                val equal : t -> t -> bool
                val subset : t -> t -> bool
                val iter : (elt -> unit) -> t -> unit
                val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
                val for_all : (elt -> bool) -> t -> bool
                val exists : (elt -> bool) -> t -> bool
                val filter : (elt -> bool) -> t -> t
                val partition : (elt -> bool) -> t -> t * t
                val cardinal : t -> int
                val elements : t -> elt list
                val min_elt : t -> elt
                val max_elt : t -> elt
                val choose : t -> elt
                val split : elt -> t -> t * bool * t
              end
            val splatList : 'a -> int -> 'a list
            val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
            val msToList :
              int MusynthTypes.LLDesigMap.t ->
              MusynthTypes.LLDesigMap.key list
            val msToStr :
              (MusynthTypes.LLDesigMap.key -> string) ->
              int MusynthTypes.LLDesigMap.t -> string
            val addToMs :
              MusynthTypes.LLDesigMap.key ->
              int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
            val delFromMS :
              MusynthTypes.LLDesigMap.key ->
              int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
            val makeEmptyMSWithAlphabet :
              MusynthTypes.LLDesigMap.key list ->
              int MusynthTypes.LLDesigMap.t
            val enumerateMS :
              MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
            val enumerateLists : 'a list -> int -> 'a list list
            val listToStr : ('a -> string) -> 'a list -> string
            val nextuid : int ref
            val getuid : unit -> int
            val resetuid : unit -> unit
            val crossProduct : 'a list list -> 'a list list
            val identConstPairList2Map :
              (MusynthTypes.IdentMap.key * 'a) list ->
              'a MusynthTypes.IdentMap.t
            val sDesigToIdent :
              MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
            val conjoinPropOpts :
              MusynthTypes.musPropT option ->
              MusynthTypes.musPropT option -> MusynthTypes.musPropT option
            val mergeIdentMaps :
              'a MusynthTypes.IdentMap.t ->
              'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
            val evalProp :
              MusynthTypes.musPropT ->
              ('a * 'b) MusynthTypes.IdentMap.t -> bool
            val getMapsForProp :
              MusynthTypes.IdentMap.key list ->
              MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
              MusynthTypes.musPropT option ->
              MusynthTypes.identifierT MusynthTypes.IdentMap.t list
            val getMsgsForAut :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
            val getNameForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
            val getTransitionsForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
            val getStatesForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
            val getAutomatonByName :
              MusynthTypes.llAutomatonT list ->
              MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
            val getSender :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
            val getReceivers :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list ->
              MusynthTypes.llAutomatonT list
            val getStateNameForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
            val getStateNamePForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
            val getCSPredsForMsg :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
            val getCSPredsForMsgAll :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
            val getMsgsToSyncOnFromState :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
            val getStatesFromWhichMsgSync :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
            val canonicalizeProp :
              MusynthTypes.llPropT -> MusynthTypes.llPropT
            val canonicalizePropFP :
              MusynthTypes.llPropT -> MusynthTypes.llPropT
            val makeFormatterOfName :
              string -> out_channel * Format.formatter
            val makeConjunction :
              MusynthTypes.llPropT list -> MusynthTypes.llPropT
            val makeDisjunction :
              MusynthTypes.llPropT list -> MusynthTypes.llPropT
            val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
            val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
          end
        val getSimpleDesigForMS :
          int MusynthTypes.LLDesigMap.t -> MusynthTypes.llDesignatorT
        val getSimpleDesigForList :
          MusynthTypes.llDesignatorT list -> MusynthTypes.llDesignatorT
        val addToList : 'a -> 'a list -> 'a list
        val addToMS :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val delFromList : 'a -> 'b list -> 'b list
        val delFromMS :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val listContains : 'a -> 'a list -> bool
        val msContains :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> bool
        val listLen : 'a list -> int
        val msLen : int MusynthTypes.LLDesigMap.t -> int
        val makeChanTran :
          (MusynthTypes.llIdentT -> 'a -> 'a) ->
          (MusynthTypes.LLDesigMap.key -> 'a -> 'a) ->
          (MusynthTypes.LLDesigMap.key -> 'a -> bool) ->
          ('a -> int) ->
          ('a -> MusynthTypes.llIdentT) ->
          'b * MusynthTypes.musChanLossT * MusynthTypes.musChanDupT *
          MusynthTypes.musChanBlockT * int ->
          'c ->
          MusynthTypes.LLDesigMap.key list ->
          MusynthTypes.llIdentT list -> 'a list -> MusynthTypes.llTransT list
        val buildChannelAutomaton :
          MusynthTypes.LLDesigMap.key list ->
          MusynthTypes.llIdentT list ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int ->
          MusynthTypes.llDesignatorT list * MusynthTypes.llTransT list
      end
    val lowerQMap :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t
    val instantiateDecl :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      (MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
       MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
       MusynthTypes.musPropT option -> 'a -> 'b) ->
      'a MusynthTypes.musDeclType -> 'b
    val substInDecl :
      'a ->
      ('a -> 'b -> 'c) ->
      'b MusynthTypes.musDeclType -> 'c MusynthTypes.musDeclType
    val makeLLInstantiation :
      (string * 'a) MusynthTypes.IdentMap.t list ->
      MusynthTypes.IdentMap.key list ->
      string -> MusynthTypes.llDesignatorT list
    val lowerDesignator :
      MusynthTypes.musDesignatorT -> MusynthTypes.llDesignatorT
    val desigDeclInstantiator :
      'a ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.musDesignatorT -> MusynthTypes.llDesignatorT list
    val transDeclInstantiator :
      'a ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
      MusynthTypes.musDesignatorT -> MusynthTypes.llTransT list
    val desigSubstitutor :
      MusynthTypes.identifierT MusynthTypes.IdentMap.t ->
      MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
    val lldesigSubstitutor :
      string MusynthTypes.StringMap.t ->
      MusynthTypes.llDesignatorT -> MusynthTypes.llDesignatorT
    val lltransSubstitutor :
      string MusynthTypes.StringMap.t ->
      MusynthTypes.llTransT -> MusynthTypes.llTransT
    val transSubstitutor :
      MusynthTypes.identifierT MusynthTypes.IdentMap.t ->
      MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
      MusynthTypes.musDesignatorT ->
      MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
      MusynthTypes.musDesignatorT
    val instantiateDesigBlock :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
      MusynthTypes.llDesignatorT list
    val instantiateTransBlock :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list -> MusynthTypes.llTransT list
    val substituteInDesigBlock :
      MusynthTypes.identifierT MusynthTypes.IdentMap.t ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list
    val substituteInTransBlock :
      MusynthTypes.identifierT MusynthTypes.IdentMap.t ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list
    val stateAnnotationInstantiator :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.llIdentT list ->
      MusynthTypes.musStateAnnotationT -> MusynthTypes.llAnnotT
    val lowerFairness : MusynthTypes.musFairnessT -> MusynthTypes.llFairnessT
    val lowerLossFairness :
      MusynthTypes.musLossFairnessT -> MusynthTypes.llLossFairnessT
    val lowerDupFairness :
      MusynthTypes.musDupFairnessT -> MusynthTypes.llDupFairnessT
    val stateAnnotSubstitutor :
      MusynthTypes.identifierT MusynthTypes.IdentMap.t ->
      MusynthTypes.musStateAnnotationT -> MusynthTypes.musStateAnnotationT
    val instantiateCompleteAutomaton :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.musDesignatorT ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType * 'a) list ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list ->
      MusynthTypes.musFairnessT -> MusynthTypes.llAutomatonT list
    val convertLLDesigToPrimed :
      MusynthTypes.llDesignatorT -> MusynthTypes.llDesignatorT
    val instantiateChannelAutomaton :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.musDesignatorT ->
      MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
      MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
      MusynthTypes.musFairnessT ->
      MusynthTypes.musLossFairnessT ->
      MusynthTypes.musDupFairnessT -> MusynthTypes.llAutomatonT list
    val checkParamCompatibility :
      MusynthTypes.llDesignatorT -> string list -> bool
    val getParamsFromLLIdent :
      MusynthTypes.llDesignatorT -> string * string list
    val checkTransitionDefined : ('a * 'b * 'c) list -> 'a -> 'b -> bool
    val mkLLDesigSet :
      MusynthTypes.LLDesigSet.elt list -> MusynthTypes.LLDesigSet.t
    val identMapToStringMap :
      ('a * 'b) MusynthTypes.IdentMap.t -> 'a MusynthTypes.StringMap.t
    val instantiateIncompleteAutomaton :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.musDesignatorT ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
       MusynthTypes.musStateAnnotationT)
      list ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list ->
      MusynthTypes.musFairnessT -> MusynthTypes.llAutomatonT list
    val autDeclInstantiator :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.musAutomatonDeclType -> MusynthTypes.llAutomatonT list
    val substInLProp :
      string MusynthTypes.StringMap.t ->
      MusynthTypes.llPropT -> MusynthTypes.llPropT
    val lowerProp :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musPropT -> MusynthTypes.llPropT
    val lowerAndCanonicalizeProp :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musPropT -> MusynthTypes.llPropT
    val lowerSpec :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSpecT -> MusynthTypes.llSpecT
    val lowerProg :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      'a * MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      MusynthTypes.musPropT list * MusynthTypes.musSpecT list ->
      MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
      MusynthTypes.llPropT * MusynthTypes.llSpecT list
  end
module CK :
  sig
    module ST :
      sig
        val createSymTab : unit -> 'a MusynthTypes.IdentMap.t ref list ref
        val push : 'a MusynthTypes.IdentMap.t ref list ref -> unit
        val pushScope : 'a list ref -> 'a -> unit
        val pop : 'a list ref -> 'a
        val peek : 'a list ref -> 'a
        val getNumScopes : 'a list ref -> int
        val lookup :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a option
        val bind :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a -> unit
        val bindGlobal :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a -> unit
        val lookupOrFail :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a
        val lookupVar :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
        val lookupType :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
        val lookupGMsg :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupAMsg :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupState :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupAutomaton :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupConst :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
      end
    val curAutomatonName : string ref
    val resolveSymType :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT
    val checkSymTypeDecl :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT -> unit
    val destructDesigDecl :
      MusynthTypes.musDesignatorT ->
      MusynthTypes.identifierT * MusynthTypes.identifierT list
    val getObligationsForIdent :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key ->
      (string * MusynthTypes.musSymTypeT) list * MusynthTypes.musPropT option
    val getTypeObligationsForIdent :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT list
    val checkTypeLists :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      string * MusynthTypes.sourcelocation option ->
      MusynthTypes.musSymTypeT list -> MusynthTypes.IdentMap.key list -> unit
    val getDesigType :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry
    val checkTypeCompatibility :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT ->
      MusynthTypes.musDesignatorT ->
      MusynthTypes.sourcelocation option ->
      MusynthTypes.symtabEntry * MusynthTypes.symtabEntry
    val checkPureProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val checkPureQProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val desigDeclChecker :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musDesignatorT ->
      'a ->
      MusynthTypes.identifierT *
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
    val transChecker :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
      MusynthTypes.musDesignatorT ->
      MusynthTypes.sourcelocation option -> string * 'a list
    val autMsgDeclChecker :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT ->
      MusynthTypes.sourcelocation option ->
      MusynthTypes.identifierT *
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
    val checkDecl :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.symTabScope list ref ->
       'a -> MusynthTypes.sourcelocation option -> 'b * 'c) ->
      'a MusynthTypes.musDeclType ->
      'b * 'c * MusynthTypes.musPropT option * MusynthTypes.symTabScope
    val cvtParamTypeListForSymtab : (('a * 'b) * 'c) list -> ('a * 'c) list
    val checkGlobalMsgDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkStateDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkAutomatonMsgDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.msgType ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkAnnotationMsgDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkAnnotation :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musStateAnnotationT -> unit
    val checkStateDeclBlock :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
       MusynthTypes.musStateAnnotationT)
      list -> bool -> unit
    val checkGlobalMsgDeclBlock :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val checkAutomatonMsgDeclBlock :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.msgType ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val convertDesigToPrimed :
      MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
    val convertDesigDeclToPrimed :
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType
    val checkAutDef :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musAutomatonDeclType ->
      'a ->
      MusynthTypes.identifierT *
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
    val initStateDeclChecker :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT list -> unit
    val checkProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val checkSpec :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musSpecT -> unit
    val checkProg :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list *
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> unit
    val checkLLProg :
      MusynthTypes.LLDesigSet.elt list * MusynthTypes.llAutomatonT list *
      'a * 'b -> unit
  end
module Parser :
  sig
    type token =
      MusynthParser.token =
        VAR
      | EQUALS
      | DOT
      | COMMA
      | MAIN
      | SYMMETRICTYPES
      | INIT
      | SEMICOLON
      | COLON
      | CAPACITY
      | MESSAGES
      | DETAUTOMATON
      | AUTOMATON
      | CHANNELAUTOMATON
      | LOSSY
      | LOSSLESS
      | DUPLICATING
      | NONDUPLICATING
      | ORDERED
      | UNORDERED
      | PARTIALAUTOMATON
      | TRANSITIONS
      | INPUTS
      | OUTPUTS
      | DEFINE
      | JUSTICE
      | COMPASSION
      | FINITELOSS
      | NONBLOCKING
      | BLOCKING
      | CANSYNCON
      | CTLSPEC
      | LTLSPEC
      | INVARIANT
      | INCOMPLETE
      | COMPLETE
      | LBRACE
      | RBRACE
      | LPAREN
      | RPAREN
      | LSQUARE
      | RSQUARE
      | STATES
      | FINITEDUP
      | BCTRUE
      | BCFALSE
      | NEQUALS
      | IN
      | WITH
      | OR
      | AND
      | NOT
      | IMPLIES
      | IFF
      | TLGLOBAL
      | TLFUTURE
      | TLNEXT
      | TLUNTIL
      | TLRELEASE
      | FORALL
      | FOREACH
      | EXISTS
      | EOF
      | IDENT of MusynthTypes.identifierT
      | STRINGCONST of string
      | INTCONST of int
    val prog :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> MusynthTypes.musProgT
  end
module AST :
  sig
    val pLoc : Format.formatter -> int * int * int * int -> unit
    val pLocOpt : Format.formatter -> (int * int * int * int) option -> unit
    val pIdentifier : Format.formatter -> string * 'a -> unit
    val identToName : 'a * 'b -> 'a
    val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
    val pList :
      string ->
      bool ->
      bool ->
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
    val pSymTypeDecl :
      Format.formatter -> (string * 'a) * MusynthTypes.musSymTypeT -> unit
    val musMakeIndentedBox :
      Format.formatter ->
      (Format.formatter -> 'a -> unit) ->
      'a ->
      (Format.formatter -> 'b -> unit) ->
      'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
    val pSymTypeDeclBlock :
      Format.formatter ->
      ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
    val pDesignator : Format.formatter -> MusynthTypes.musDesignatorT -> unit
    val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
    val pPropOpt : Format.formatter -> MusynthTypes.musPropT option -> unit
    val pDecl :
      (Format.formatter -> 'a -> 'b) ->
      (Format.formatter -> 'a -> 'c) ->
      Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
    val noopPrinter : 'a -> 'b -> unit
    val pMsgDecl :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val pMsgDeclBlock :
      string ->
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val pMessagesDeclBlock :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val pStateAnnot :
      Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
    val pStateDecl :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
      MusynthTypes.musStateAnnotationT -> unit
    val pStateDeclBlock :
      string ->
      Format.formatter ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
       MusynthTypes.musStateAnnotationT)
      list -> unit
    val pTransDecl :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType -> unit
    val pTransDeclBlock :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list -> unit
    val pInitStateConstraint :
      Format.formatter -> MusynthTypes.musPropT -> unit
    val pInitStateDeclBlock :
      Format.formatter -> MusynthTypes.musPropT list -> unit
    val pChanProp :
      Format.formatter ->
      MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
      MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
      unit
    val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
    val pLossFairness :
      Format.formatter -> MusynthTypes.musLossFairnessT -> unit
    val pDupFairness :
      Format.formatter -> MusynthTypes.musDupFairnessT -> unit
    val pAutomatonDecl :
      Format.formatter ->
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
    val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
    val pProg :
      Format.formatter ->
      ((string * 'a) * MusynthTypes.musSymTypeT) list *
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> unit
    val pLLDesignator :
      Format.formatter -> MusynthTypes.llDesignatorT -> unit
    val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
    val pLLVar :
      Format.formatter ->
      MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
    val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
    val pLLFairness : Format.formatter -> MusynthTypes.llFairnessT -> unit
    val pLLDupFairness :
      Format.formatter -> MusynthTypes.llDupFairnessT -> unit
    val pLLLossFairness :
      Format.formatter -> MusynthTypes.llLossFairnessT -> unit
    val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
    val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
    val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
    val pLLAutomaton : Format.formatter -> MusynthTypes.llAutomatonT -> unit
    val pLLProg :
      Format.formatter ->
      MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
      MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
  end
module ST :
  sig
    val createSymTab : unit -> 'a MusynthTypes.IdentMap.t ref list ref
    val push : 'a MusynthTypes.IdentMap.t ref list ref -> unit
    val pushScope : 'a list ref -> 'a -> unit
    val pop : 'a list ref -> 'a
    val peek : 'a list ref -> 'a
    val getNumScopes : 'a list ref -> int
    val lookup :
      'a MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> 'a option
    val bind :
      'a MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> 'a -> unit
    val bindGlobal :
      'a MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> 'a -> unit
    val lookupOrFail :
      'a MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> 'a
    val lookupVar :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
    val lookupType :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
    val lookupGMsg :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
    val lookupAMsg :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
    val lookupState :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
    val lookupAutomaton :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
    val lookupConst :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
  end
module Enc :
  sig
    module AST :
      sig
        val pLoc : Format.formatter -> int * int * int * int -> unit
        val pLocOpt :
          Format.formatter -> (int * int * int * int) option -> unit
        val pIdentifier : Format.formatter -> string * 'a -> unit
        val identToName : 'a * 'b -> 'a
        val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
        val pList :
          string ->
          bool ->
          bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> 'a list -> unit
        val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
        val pSymTypeDecl :
          Format.formatter ->
          (string * 'a) * MusynthTypes.musSymTypeT -> unit
        val musMakeIndentedBox :
          Format.formatter ->
          (Format.formatter -> 'a -> unit) ->
          'a ->
          (Format.formatter -> 'b -> unit) ->
          'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
        val pSymTypeDeclBlock :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
        val pDesignator :
          Format.formatter -> MusynthTypes.musDesignatorT -> unit
        val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
        val pPropOpt :
          Format.formatter -> MusynthTypes.musPropT option -> unit
        val pDecl :
          (Format.formatter -> 'a -> 'b) ->
          (Format.formatter -> 'a -> 'c) ->
          Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
        val noopPrinter : 'a -> 'b -> unit
        val pMsgDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val pMsgDeclBlock :
          string ->
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pMessagesDeclBlock :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pStateAnnot :
          Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
        val pStateDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
          MusynthTypes.musStateAnnotationT -> unit
        val pStateDeclBlock :
          string ->
          Format.formatter ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> unit
        val pTransDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType -> unit
        val pTransDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType list -> unit
        val pInitStateConstraint :
          Format.formatter -> MusynthTypes.musPropT -> unit
        val pInitStateDeclBlock :
          Format.formatter -> MusynthTypes.musPropT list -> unit
        val pChanProp :
          Format.formatter ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
          unit
        val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
        val pLossFairness :
          Format.formatter -> MusynthTypes.musLossFairnessT -> unit
        val pDupFairness :
          Format.formatter -> MusynthTypes.musDupFairnessT -> unit
        val pAutomatonDecl :
          Format.formatter ->
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
        val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
        val pProg :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val pLLDesignator :
          Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLVar :
          Format.formatter ->
          MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
        val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
        val pLLFairness :
          Format.formatter -> MusynthTypes.llFairnessT -> unit
        val pLLDupFairness :
          Format.formatter -> MusynthTypes.llDupFairnessT -> unit
        val pLLLossFairness :
          Format.formatter -> MusynthTypes.llLossFairnessT -> unit
        val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
        val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
        val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
        val pLLAutomaton :
          Format.formatter -> MusynthTypes.llAutomatonT -> unit
        val pLLProg :
          Format.formatter ->
          MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
          MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
      end
    module Utils :
      sig
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module MSSet :
          sig
            type elt = int MusynthTypes.LLDesigMap.t
            type t = MusynthUtils.MSSet.t
            val empty : t
            val is_empty : t -> bool
            val mem : elt -> t -> bool
            val add : elt -> t -> t
            val singleton : elt -> t
            val remove : elt -> t -> t
            val union : t -> t -> t
            val inter : t -> t -> t
            val diff : t -> t -> t
            val compare : t -> t -> int
            val equal : t -> t -> bool
            val subset : t -> t -> bool
            val iter : (elt -> unit) -> t -> unit
            val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
            val for_all : (elt -> bool) -> t -> bool
            val exists : (elt -> bool) -> t -> bool
            val filter : (elt -> bool) -> t -> t
            val partition : (elt -> bool) -> t -> t * t
            val cardinal : t -> int
            val elements : t -> elt list
            val min_elt : t -> elt
            val max_elt : t -> elt
            val choose : t -> elt
            val split : elt -> t -> t * bool * t
          end
        val splatList : 'a -> int -> 'a list
        val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
        val msToList :
          int MusynthTypes.LLDesigMap.t -> MusynthTypes.LLDesigMap.key list
        val msToStr :
          (MusynthTypes.LLDesigMap.key -> string) ->
          int MusynthTypes.LLDesigMap.t -> string
        val addToMs :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val delFromMS :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val makeEmptyMSWithAlphabet :
          MusynthTypes.LLDesigMap.key list -> int MusynthTypes.LLDesigMap.t
        val enumerateMS :
          MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
        val enumerateLists : 'a list -> int -> 'a list list
        val listToStr : ('a -> string) -> 'a list -> string
        val nextuid : int ref
        val getuid : unit -> int
        val resetuid : unit -> unit
        val crossProduct : 'a list list -> 'a list list
        val identConstPairList2Map :
          (MusynthTypes.IdentMap.key * 'a) list -> 'a MusynthTypes.IdentMap.t
        val sDesigToIdent :
          MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
        val conjoinPropOpts :
          MusynthTypes.musPropT option ->
          MusynthTypes.musPropT option -> MusynthTypes.musPropT option
        val mergeIdentMaps :
          'a MusynthTypes.IdentMap.t ->
          'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
        val evalProp :
          MusynthTypes.musPropT -> ('a * 'b) MusynthTypes.IdentMap.t -> bool
        val getMapsForProp :
          MusynthTypes.IdentMap.key list ->
          MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
          MusynthTypes.musPropT option ->
          MusynthTypes.identifierT MusynthTypes.IdentMap.t list
        val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
        val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
        val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
        val getMsgsForAut :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
        val getNameForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
        val getTransitionsForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
        val getStatesForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
        val getAutomatonByName :
          MusynthTypes.llAutomatonT list ->
          MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
        val getFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
        val getLFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
        val getDFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
        val getSender :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
        val getReceivers :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT list
        val getStateNameForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getStateNamePForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getCSPredsForMsg :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
        val getCSPredsForMsgAll :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
        val getMsgsToSyncOnFromState :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val getStatesFromWhichMsgSync :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val canonicalizeProp : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val canonicalizePropFP : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val makeFormatterOfName : string -> out_channel * Format.formatter
        val makeConjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
        val makeDisjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
      end
    module Debug :
      sig
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val reorderMethods : string list
          end
        val debugOC : out_channel option ref
        val debugFmt : Format.formatter option ref
        val debugEnabled : unit -> bool
        val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
        val getDebugFmt : unit -> Format.formatter
        val initDebugSubsys : string -> unit
        val shutDownDebugSubsys : unit -> unit
        val dprintf :
          MusynthTypes.StringSet.elt ->
          ('a, Format.formatter, unit) format -> 'a
        val dflush : unit -> unit
      end
    module LTL :
      sig
        module Utils :
          sig
            module AST :
              sig
                val pLoc : Format.formatter -> int * int * int * int -> unit
                val pLocOpt :
                  Format.formatter -> (int * int * int * int) option -> unit
                val pIdentifier : Format.formatter -> string * 'a -> unit
                val identToName : 'a * 'b -> 'a
                val astToString :
                  (Format.formatter -> 'a -> 'b) -> 'a -> string
                val pList :
                  string ->
                  bool ->
                  bool ->
                  (Format.formatter -> 'a -> unit) ->
                  Format.formatter -> 'a list -> unit
                val pSymType :
                  Format.formatter -> MusynthTypes.musSymTypeT -> unit
                val pSymTypeDecl :
                  Format.formatter ->
                  (string * 'a) * MusynthTypes.musSymTypeT -> unit
                val musMakeIndentedBox :
                  Format.formatter ->
                  (Format.formatter -> 'a -> unit) ->
                  'a ->
                  (Format.formatter -> 'b -> unit) ->
                  'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
                val pSymTypeDeclBlock :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
                val pDesignator :
                  Format.formatter -> MusynthTypes.musDesignatorT -> unit
                val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
                val pPropOpt :
                  Format.formatter -> MusynthTypes.musPropT option -> unit
                val pDecl :
                  (Format.formatter -> 'a -> 'b) ->
                  (Format.formatter -> 'a -> 'c) ->
                  Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
                val noopPrinter : 'a -> 'b -> unit
                val pMsgDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val pMsgDeclBlock :
                  string ->
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pMessagesDeclBlock :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pStateAnnot :
                  Format.formatter ->
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDeclBlock :
                  string ->
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                   MusynthTypes.musStateAnnotationT)
                  list -> unit
                val pTransDecl :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType -> unit
                val pTransDeclBlock :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType list -> unit
                val pInitStateConstraint :
                  Format.formatter -> MusynthTypes.musPropT -> unit
                val pInitStateDeclBlock :
                  Format.formatter -> MusynthTypes.musPropT list -> unit
                val pChanProp :
                  Format.formatter ->
                  MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
                  MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
                  int -> unit
                val pFairness :
                  Format.formatter -> MusynthTypes.musFairnessT -> unit
                val pLossFairness :
                  Format.formatter -> MusynthTypes.musLossFairnessT -> unit
                val pAutomatonDecl :
                  Format.formatter ->
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
                  unit
                val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
                val pProg :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list *
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType
                  list * MusynthTypes.musPropT list *
                  MusynthTypes.musSpecT list -> unit
                val pLLDesignator :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLIdent :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLVar :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t ->
                  unit
                val pLLAnnot :
                  Format.formatter -> MusynthTypes.llAnnotT -> unit
                val pLLTrans :
                  Format.formatter -> MusynthTypes.llTransT -> unit
                val pLLProp :
                  Format.formatter -> MusynthTypes.llPropT -> unit
                val pLLSpec :
                  Format.formatter -> MusynthTypes.llSpecT -> unit
                val pLLAutomaton :
                  Format.formatter -> MusynthTypes.llAutomatonT -> unit
                val pLLProg :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT list *
                  MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
                  MusynthTypes.llSpecT list -> unit
              end
            module MSSet :
              sig
                type elt = int MusynthTypes.LLDesigMap.t
                type t = MusynthUtils.MSSet.t
                val empty : t
                val is_empty : t -> bool
                val mem : elt -> t -> bool
                val add : elt -> t -> t
                val singleton : elt -> t
                val remove : elt -> t -> t
                val union : t -> t -> t
                val inter : t -> t -> t
                val diff : t -> t -> t
                val compare : t -> t -> int
                val equal : t -> t -> bool
                val subset : t -> t -> bool
                val iter : (elt -> unit) -> t -> unit
                val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
                val for_all : (elt -> bool) -> t -> bool
                val exists : (elt -> bool) -> t -> bool
                val filter : (elt -> bool) -> t -> t
                val partition : (elt -> bool) -> t -> t * t
                val cardinal : t -> int
                val elements : t -> elt list
                val min_elt : t -> elt
                val max_elt : t -> elt
                val choose : t -> elt
                val split : elt -> t -> t * bool * t
              end
            val splatList : 'a -> int -> 'a list
            val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
            val msToList :
              int MusynthTypes.LLDesigMap.t ->
              MusynthTypes.LLDesigMap.key list
            val msToStr :
              (MusynthTypes.LLDesigMap.key -> string) ->
              int MusynthTypes.LLDesigMap.t -> string
            val addToMs :
              MusynthTypes.LLDesigMap.key ->
              int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
            val delFromMS :
              MusynthTypes.LLDesigMap.key ->
              int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
            val makeEmptyMSWithAlphabet :
              MusynthTypes.LLDesigMap.key list ->
              int MusynthTypes.LLDesigMap.t
            val enumerateMS :
              MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
            val enumerateLists : 'a list -> int -> 'a list list
            val listToStr : ('a -> string) -> 'a list -> string
            val nextuid : int ref
            val getuid : unit -> int
            val resetuid : unit -> unit
            val crossProduct : 'a list list -> 'a list list
            val identConstPairList2Map :
              (MusynthTypes.IdentMap.key * 'a) list ->
              'a MusynthTypes.IdentMap.t
            val sDesigToIdent :
              MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
            val conjoinPropOpts :
              MusynthTypes.musPropT option ->
              MusynthTypes.musPropT option -> MusynthTypes.musPropT option
            val mergeIdentMaps :
              'a MusynthTypes.IdentMap.t ->
              'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
            val evalProp :
              MusynthTypes.musPropT ->
              ('a * 'b) MusynthTypes.IdentMap.t -> bool
            val getMapsForProp :
              MusynthTypes.IdentMap.key list ->
              MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
              MusynthTypes.musPropT option ->
              MusynthTypes.identifierT MusynthTypes.IdentMap.t list
            val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
            val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
            val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
            val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
            val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
            val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
            val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
            val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
            val getMsgsForAut :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
            val getNameForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
            val getTransitionsForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
            val getStatesForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
            val getAutomatonByName :
              MusynthTypes.llAutomatonT list ->
              MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
            val getFairnessForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
            val getLFairnessForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
            val getDFairnessForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
            val getSender :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
            val getReceivers :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list ->
              MusynthTypes.llAutomatonT list
            val getStateNameForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
            val getStateNamePForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
            val getCSPredsForMsg :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
            val getCSPredsForMsgAll :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
            val getMsgsToSyncOnFromState :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
            val getStatesFromWhichMsgSync :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
            val canonicalizeProp :
              MusynthTypes.llPropT -> MusynthTypes.llPropT
            val canonicalizePropFP :
              MusynthTypes.llPropT -> MusynthTypes.llPropT
            val makeFormatterOfName :
              string -> out_channel * Format.formatter
            val makeConjunction :
              MusynthTypes.llPropT list -> MusynthTypes.llPropT
            val makeDisjunction :
              MusynthTypes.llPropT list -> MusynthTypes.llPropT
          end
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val tracePrintMode : string ref
            val jumpStep : int ref
            val reorderMethods : string list
          end
        module Debug :
          sig
            module Opts :
              sig
                val debugDisabled : bool ref
                val debugOptions : MusynthTypes.StringSet.t ref
                val debugFileName : string ref
                val onlySafety : bool ref
                val conjunctivePart : bool ref
                val inputFileName : string ref
                val numSolsRequested : int ref
                val reorderEnabled : bool ref
                val reorderMethod : Cudd.Man.reorder ref
                val reorderMethods : string list
              end
            val debugOC : out_channel option ref
            val debugFmt : Format.formatter option ref
            val debugEnabled : unit -> bool
            val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
            val getDebugFmt : unit -> Format.formatter
            val initDebugSubsys : string -> unit
            val shutDownDebugSubsys : unit -> unit
            val dprintf :
              MusynthTypes.StringSet.elt ->
              ('a, Format.formatter, unit) format -> 'a
            val dflush : unit -> unit
          end
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pDupFairness :
              Format.formatter -> MusynthTypes.musDupFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLFairness :
              Format.formatter -> MusynthTypes.llFairnessT -> unit
            val pLLDupFairness :
              Format.formatter -> MusynthTypes.llDupFairnessT -> unit
            val pLLLossFairness :
              Format.formatter -> MusynthTypes.llLossFairnessT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        val constructEnabledProp :
          MusynthTypes.llAutomatonT list ->
          MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
        val constructFairnessSpecsAut :
          MusynthTypes.llAutomatonT list ->
          MusynthTypes.llAutomatonT -> MusynthTypes.fairnessSpecT
        val constructFiniteLossFairness :
          MusynthTypes.llAutomatonT -> MusynthTypes.fairnessSpecT list
        val constructFiniteDupFairness :
          MusynthTypes.llAutomatonT -> MusynthTypes.fairnessSpecT list
        val constructFairnessSpecsChan :
          MusynthTypes.llAutomatonT list ->
          MusynthTypes.llAutomatonT -> MusynthTypes.fairnessSpecT list
        val constructFairnessSpecs :
          'a * MusynthTypes.llAutomatonT list * 'b * 'c ->
          MusynthTypes.fairnessSpecT list
        val createTablueaxVars :
          MusynthTypes.PropMap.key ->
          MusynthTypes.llDesignatorT MusynthTypes.PropMap.t *
          MusynthTypes.PropMap.key MusynthTypes.LLDesigMap.t *
          MusynthTypes.PropMap.key MusynthTypes.PropMap.t
        val getXFormulaVarPairs :
          'a MusynthTypes.PropMap.t -> (MusynthTypes.PropMap.key * 'a) list
        val getUFormulaVarPairs :
          'a MusynthTypes.PropMap.t -> (MusynthTypes.PropMap.key * 'a) list
        val constructPrimedVarMap :
          'a MusynthTypes.LLDesigMap.t ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t
        val substForVars :
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t ->
          MusynthTypes.llPropT -> MusynthTypes.llPropT
        val constructTableau :
          MusynthTypes.llPropT ->
          MusynthTypes.llDesignatorT MusynthTypes.PropMap.t *
          MusynthTypes.PropMap.key MusynthTypes.LLDesigMap.t *
          MusynthTypes.PropMap.key MusynthTypes.PropMap.t *
          MusynthTypes.PropMap.key * MusynthTypes.llPropT *
          MusynthTypes.fairnessSpecT list
      end
    val constructDLProps :
      MusynthTypes.llIdentT list ->
      MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
    val encodeStateVariables :
      < registerStateVariable : MusynthTypes.llDesignatorT ->
                                MusynthTypes.llIdentT list -> 'a;
        .. > ->
      MusynthTypes.llAutomatonT -> 'a
    val encodeParamVariables :
      < registerParamVariable : MusynthTypes.llDesignatorT ->
                                MusynthTypes.LLDesigSet.elt list -> 'a;
        .. > ->
      MusynthTypes.llAutomatonT -> 'a list
    val getNextStatePropForTrans :
      MusynthTypes.llDesignatorT ->
      MusynthTypes.llTransT -> MusynthTypes.llPropT
    val getNextStatePropOnAllForMsg :
      MusynthTypes.llAutomatonT list ->
      MusynthTypes.llIdentT -> MusynthTypes.llPropT
    val getTransitionRelationForAut :
      MusynthTypes.llAutomatonT -> 'a -> MusynthTypes.llPropT
    val encodeTransitionRelation :
      MusynthTypes.llIdentT list ->
      MusynthTypes.llAutomatonT list ->
      MusynthTypes.llPropT MusynthTypes.LLDesigMap.t
    val encodeProg :
      < prop2BDD : MusynthTypes.PropMap.key -> 'a;
        registerInternalStateVariable : MusynthTypes.LLDesigMap.key ->
                                        MusynthTypes.llDesignatorT list ->
                                        unit;
        registerParamVariable : MusynthTypes.llDesignatorT ->
                                MusynthTypes.LLDesigSet.elt list -> 'b;
        registerStateVariable : MusynthTypes.llDesignatorT ->
                                MusynthTypes.llIdentT list -> 'c;
        .. > ->
      MusynthTypes.llIdentT list * MusynthTypes.llAutomatonT list *
      MusynthTypes.llPropT * MusynthTypes.llSpecT list ->
      'a MusynthTypes.LLDesigMap.t * 'a * 'a * 'a *
      (MusynthTypes.llDesignatorT MusynthTypes.PropMap.t *
       MusynthTypes.PropMap.key MusynthTypes.LLDesigMap.t *
       MusynthTypes.PropMap.key MusynthTypes.PropMap.t * 'a * 'a * 'a list *
       ('a * 'a) list)
      MusynthTypes.StringMap.t
  end
module MC :
  sig
    module AST :
      sig
        val pLoc : Format.formatter -> int * int * int * int -> unit
        val pLocOpt :
          Format.formatter -> (int * int * int * int) option -> unit
        val pIdentifier : Format.formatter -> string * 'a -> unit
        val identToName : 'a * 'b -> 'a
        val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
        val pList :
          string ->
          bool ->
          bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> 'a list -> unit
        val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
        val pSymTypeDecl :
          Format.formatter ->
          (string * 'a) * MusynthTypes.musSymTypeT -> unit
        val musMakeIndentedBox :
          Format.formatter ->
          (Format.formatter -> 'a -> unit) ->
          'a ->
          (Format.formatter -> 'b -> unit) ->
          'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
        val pSymTypeDeclBlock :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
        val pDesignator :
          Format.formatter -> MusynthTypes.musDesignatorT -> unit
        val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
        val pPropOpt :
          Format.formatter -> MusynthTypes.musPropT option -> unit
        val pDecl :
          (Format.formatter -> 'a -> 'b) ->
          (Format.formatter -> 'a -> 'c) ->
          Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
        val noopPrinter : 'a -> 'b -> unit
        val pMsgDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val pMsgDeclBlock :
          string ->
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pMessagesDeclBlock :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pStateAnnot :
          Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
        val pStateDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
          MusynthTypes.musStateAnnotationT -> unit
        val pStateDeclBlock :
          string ->
          Format.formatter ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> unit
        val pTransDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType -> unit
        val pTransDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType list -> unit
        val pInitStateConstraint :
          Format.formatter -> MusynthTypes.musPropT -> unit
        val pInitStateDeclBlock :
          Format.formatter -> MusynthTypes.musPropT list -> unit
        val pChanProp :
          Format.formatter ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
          unit
        val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
        val pLossFairness :
          Format.formatter -> MusynthTypes.musLossFairnessT -> unit
        val pDupFairness :
          Format.formatter -> MusynthTypes.musDupFairnessT -> unit
        val pAutomatonDecl :
          Format.formatter ->
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
        val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
        val pProg :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val pLLDesignator :
          Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLVar :
          Format.formatter ->
          MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
        val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
        val pLLFairness :
          Format.formatter -> MusynthTypes.llFairnessT -> unit
        val pLLDupFairness :
          Format.formatter -> MusynthTypes.llDupFairnessT -> unit
        val pLLLossFairness :
          Format.formatter -> MusynthTypes.llLossFairnessT -> unit
        val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
        val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
        val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
        val pLLAutomaton :
          Format.formatter -> MusynthTypes.llAutomatonT -> unit
        val pLLProg :
          Format.formatter ->
          MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
          MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
      end
    module Debug :
      sig
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val reorderMethods : string list
          end
        val debugOC : out_channel option ref
        val debugFmt : Format.formatter option ref
        val debugEnabled : unit -> bool
        val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
        val getDebugFmt : unit -> Format.formatter
        val initDebugSubsys : string -> unit
        val shutDownDebugSubsys : unit -> unit
        val dprintf :
          MusynthTypes.StringSet.elt ->
          ('a, Format.formatter, unit) format -> 'a
        val dflush : unit -> unit
      end
    module Opts :
      sig
        val debugDisabled : bool ref
        val debugOptions : MusynthTypes.StringSet.t ref
        val debugFileName : string ref
        val onlySafety : bool ref
        val conjunctivePart : bool ref
        val inputFileName : string ref
        val numSolsRequested : int ref
        val reorderEnabled : bool ref
        val reorderMethod : Cudd.Man.reorder ref
        val tracePrintMode : string ref
        val jumpStep : int ref
        val reorderMethods : string list
      end
    module Trace :
      sig
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val tracePrintMode : string ref
            val reorderMethods : string list
          end
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module Debug :
          sig
            module Opts :
              sig
                val debugDisabled : bool ref
                val debugOptions : MusynthTypes.StringSet.t ref
                val debugFileName : string ref
                val onlySafety : bool ref
                val conjunctivePart : bool ref
                val inputFileName : string ref
                val numSolsRequested : int ref
                val reorderEnabled : bool ref
                val reorderMethod : Cudd.Man.reorder ref
                val reorderMethods : string list
              end
            val debugOC : out_channel option ref
            val debugFmt : Format.formatter option ref
            val debugEnabled : unit -> bool
            val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
            val getDebugFmt : unit -> Format.formatter
            val initDebugSubsys : string -> unit
            val shutDownDebugSubsys : unit -> unit
            val dprintf :
              MusynthTypes.StringSet.elt ->
              ('a, Format.formatter, unit) format -> 'a
            val dflush : unit -> unit
          end
        val printState :
          Format.formatter ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t -> unit
        val printTraceFullSafety :
          Format.formatter ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
        val computeDiff :
          'a MusynthTypes.LLDesigMap.t ->
          'a MusynthTypes.LLDesigMap.t -> 'a MusynthTypes.LLDesigMap.t
        val printTraceDiffSafety :
          Format.formatter ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
        val printTraceDiffLiveness :
          Format.formatter ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
        val printTraceFullLiveness :
          Format.formatter ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
        val printTraceSafety :
          Format.formatter ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
        val printTraceLiveness :
          Format.formatter ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list ->
          MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
      end
    module MGR :
      sig
        module CK :
          sig
            module ST :
              sig
                val createSymTab :
                  unit -> 'a MusynthTypes.IdentMap.t ref list ref
                val push : 'a MusynthTypes.IdentMap.t ref list ref -> unit
                val pushScope : 'a list ref -> 'a -> unit
                val pop : 'a list ref -> 'a
                val peek : 'a list ref -> 'a
                val getNumScopes : 'a list ref -> int
                val lookup :
                  'a MusynthTypes.IdentMap.t ref list ref ->
                  MusynthTypes.IdentMap.key -> 'a option
                val bind :
                  'a MusynthTypes.IdentMap.t ref list ref ->
                  MusynthTypes.IdentMap.key -> 'a -> unit
                val bindGlobal :
                  'a MusynthTypes.IdentMap.t ref list ref ->
                  MusynthTypes.IdentMap.key -> 'a -> unit
                val lookupOrFail :
                  'a MusynthTypes.IdentMap.t ref list ref ->
                  MusynthTypes.IdentMap.key -> 'a
                val lookupVar :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
                val lookupType :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
                val lookupGMsg :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                val lookupAMsg :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                val lookupState :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                val lookupAutomaton :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                val lookupConst :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
              end
            val curAutomatonName : string ref
            val resolveSymType :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT
            val checkSymTypeDecl :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT -> unit
            val destructDesigDecl :
              MusynthTypes.musDesignatorT ->
              MusynthTypes.identifierT * MusynthTypes.identifierT list
            val getObligationsForIdent :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key ->
              (string * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musPropT option
            val getTypeObligationsForIdent :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT list
            val checkTypeLists :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              string * MusynthTypes.sourcelocation option ->
              MusynthTypes.musSymTypeT list ->
              MusynthTypes.IdentMap.key list -> unit
            val getDesigType :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry
            val checkTypeCompatibility :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT ->
              MusynthTypes.musDesignatorT ->
              MusynthTypes.sourcelocation option ->
              MusynthTypes.symtabEntry * MusynthTypes.symtabEntry
            val checkPureProp :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musPropT -> unit
            val checkPureQProp :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musPropT -> unit
            val desigDeclChecker :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.musDesignatorT ->
              'a ->
              MusynthTypes.identifierT *
              (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
            val transChecker :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
              MusynthTypes.musDesignatorT ->
              MusynthTypes.sourcelocation option -> string * 'a list
            val autMsgDeclChecker :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT ->
              MusynthTypes.sourcelocation option ->
              MusynthTypes.identifierT *
              (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
            val checkDecl :
              MusynthTypes.symTabScope list ref ->
              (MusynthTypes.symTabScope list ref ->
               'a -> MusynthTypes.sourcelocation option -> 'b * 'c) ->
              'a MusynthTypes.musDeclType ->
              'b * 'c * MusynthTypes.musPropT option *
              MusynthTypes.symTabScope
            val cvtParamTypeListForSymtab :
              (('a * 'b) * 'c) list -> ('a * 'c) list
            val checkGlobalMsgDecl :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val checkStateDecl :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val checkAutomatonMsgDecl :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.msgType ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val checkAnnotationMsgDecl :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val checkAnnotation :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musStateAnnotationT -> unit
            val checkStateDeclBlock :
              MusynthTypes.symTabScope list ref ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> bool -> unit
            val checkGlobalMsgDeclBlock :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val checkAutomatonMsgDeclBlock :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.msgType ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val convertDesigToPrimed :
              MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
            val convertDesigDeclToPrimed :
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType
            val checkAutDef :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musAutomatonDeclType ->
              'a ->
              MusynthTypes.identifierT *
              (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
            val initStateDeclChecker :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musPropT list -> unit
            val checkProp :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musPropT -> unit
            val checkSpec :
              MusynthTypes.symTabScope list ref ->
              MusynthTypes.musSpecT -> unit
            val checkProg :
              MusynthTypes.symTabScope list ref ->
              (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val checkLLProg :
              MusynthTypes.LLDesigSet.elt list *
              MusynthTypes.llAutomatonT list * 'a * 'b -> unit
          end
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pDupFairness :
              Format.formatter -> MusynthTypes.musDupFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLFairness :
              Format.formatter -> MusynthTypes.llFairnessT -> unit
            val pLLDupFairness :
              Format.formatter -> MusynthTypes.llDupFairnessT -> unit
            val pLLLossFairness :
              Format.formatter -> MusynthTypes.llLossFairnessT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module Utils :
          sig
            module AST :
              sig
                val pLoc : Format.formatter -> int * int * int * int -> unit
                val pLocOpt :
                  Format.formatter -> (int * int * int * int) option -> unit
                val pIdentifier : Format.formatter -> string * 'a -> unit
                val identToName : 'a * 'b -> 'a
                val astToString :
                  (Format.formatter -> 'a -> 'b) -> 'a -> string
                val pList :
                  string ->
                  bool ->
                  bool ->
                  (Format.formatter -> 'a -> unit) ->
                  Format.formatter -> 'a list -> unit
                val pSymType :
                  Format.formatter -> MusynthTypes.musSymTypeT -> unit
                val pSymTypeDecl :
                  Format.formatter ->
                  (string * 'a) * MusynthTypes.musSymTypeT -> unit
                val musMakeIndentedBox :
                  Format.formatter ->
                  (Format.formatter -> 'a -> unit) ->
                  'a ->
                  (Format.formatter -> 'b -> unit) ->
                  'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
                val pSymTypeDeclBlock :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
                val pDesignator :
                  Format.formatter -> MusynthTypes.musDesignatorT -> unit
                val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
                val pPropOpt :
                  Format.formatter -> MusynthTypes.musPropT option -> unit
                val pDecl :
                  (Format.formatter -> 'a -> 'b) ->
                  (Format.formatter -> 'a -> 'c) ->
                  Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
                val noopPrinter : 'a -> 'b -> unit
                val pMsgDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val pMsgDeclBlock :
                  string ->
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pMessagesDeclBlock :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pStateAnnot :
                  Format.formatter ->
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDeclBlock :
                  string ->
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                   MusynthTypes.musStateAnnotationT)
                  list -> unit
                val pTransDecl :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType -> unit
                val pTransDeclBlock :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType list -> unit
                val pInitStateConstraint :
                  Format.formatter -> MusynthTypes.musPropT -> unit
                val pInitStateDeclBlock :
                  Format.formatter -> MusynthTypes.musPropT list -> unit
                val pChanProp :
                  Format.formatter ->
                  MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
                  MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
                  int -> unit
                val pFairness :
                  Format.formatter -> MusynthTypes.musFairnessT -> unit
                val pLossFairness :
                  Format.formatter -> MusynthTypes.musLossFairnessT -> unit
                val pAutomatonDecl :
                  Format.formatter ->
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
                  unit
                val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
                val pProg :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list *
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType
                  list * MusynthTypes.musPropT list *
                  MusynthTypes.musSpecT list -> unit
                val pLLDesignator :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLIdent :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLVar :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t ->
                  unit
                val pLLAnnot :
                  Format.formatter -> MusynthTypes.llAnnotT -> unit
                val pLLTrans :
                  Format.formatter -> MusynthTypes.llTransT -> unit
                val pLLProp :
                  Format.formatter -> MusynthTypes.llPropT -> unit
                val pLLSpec :
                  Format.formatter -> MusynthTypes.llSpecT -> unit
                val pLLAutomaton :
                  Format.formatter -> MusynthTypes.llAutomatonT -> unit
                val pLLProg :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT list *
                  MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
                  MusynthTypes.llSpecT list -> unit
              end
            module MSSet :
              sig
                type elt = int MusynthTypes.LLDesigMap.t
                type t = MusynthUtils.MSSet.t
                val empty : t
                val is_empty : t -> bool
                val mem : elt -> t -> bool
                val add : elt -> t -> t
                val singleton : elt -> t
                val remove : elt -> t -> t
                val union : t -> t -> t
                val inter : t -> t -> t
                val diff : t -> t -> t
                val compare : t -> t -> int
                val equal : t -> t -> bool
                val subset : t -> t -> bool
                val iter : (elt -> unit) -> t -> unit
                val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
                val for_all : (elt -> bool) -> t -> bool
                val exists : (elt -> bool) -> t -> bool
                val filter : (elt -> bool) -> t -> t
                val partition : (elt -> bool) -> t -> t * t
                val cardinal : t -> int
                val elements : t -> elt list
                val min_elt : t -> elt
                val max_elt : t -> elt
                val choose : t -> elt
                val split : elt -> t -> t * bool * t
              end
            val splatList : 'a -> int -> 'a list
            val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
            val msToList :
              int MusynthTypes.LLDesigMap.t ->
              MusynthTypes.LLDesigMap.key list
            val msToStr :
              (MusynthTypes.LLDesigMap.key -> string) ->
              int MusynthTypes.LLDesigMap.t -> string
            val addToMs :
              MusynthTypes.LLDesigMap.key ->
              int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
            val delFromMS :
              MusynthTypes.LLDesigMap.key ->
              int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
            val makeEmptyMSWithAlphabet :
              MusynthTypes.LLDesigMap.key list ->
              int MusynthTypes.LLDesigMap.t
            val enumerateMS :
              MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
            val enumerateLists : 'a list -> int -> 'a list list
            val listToStr : ('a -> string) -> 'a list -> string
            val nextuid : int ref
            val getuid : unit -> int
            val resetuid : unit -> unit
            val crossProduct : 'a list list -> 'a list list
            val identConstPairList2Map :
              (MusynthTypes.IdentMap.key * 'a) list ->
              'a MusynthTypes.IdentMap.t
            val sDesigToIdent :
              MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
            val conjoinPropOpts :
              MusynthTypes.musPropT option ->
              MusynthTypes.musPropT option -> MusynthTypes.musPropT option
            val mergeIdentMaps :
              'a MusynthTypes.IdentMap.t ->
              'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
            val evalProp :
              MusynthTypes.musPropT ->
              ('a * 'b) MusynthTypes.IdentMap.t -> bool
            val getMapsForProp :
              MusynthTypes.IdentMap.key list ->
              MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
              MusynthTypes.musPropT option ->
              MusynthTypes.identifierT MusynthTypes.IdentMap.t list
            val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
            val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
            val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
            val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
            val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
            val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
            val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
            val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
            val getMsgsForAut :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
            val getNameForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
            val getTransitionsForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
            val getStatesForAut :
              MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
            val getAutomatonByName :
              MusynthTypes.llAutomatonT list ->
              MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
            val getFairnessForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
            val getLFairnessForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
            val getDFairnessForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
            val getSender :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
            val getReceivers :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list ->
              MusynthTypes.llAutomatonT list
            val getStateNameForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
            val getStateNamePForAutomaton :
              MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
            val getCSPredsForMsg :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
            val getCSPredsForMsgAll :
              MusynthTypes.llIdentT ->
              MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
            val getMsgsToSyncOnFromState :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
            val getStatesFromWhichMsgSync :
              MusynthTypes.llAutomatonT ->
              MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
            val canonicalizeProp :
              MusynthTypes.llPropT -> MusynthTypes.llPropT
            val canonicalizePropFP :
              MusynthTypes.llPropT -> MusynthTypes.llPropT
            val makeFormatterOfName :
              string -> out_channel * Format.formatter
            val makeConjunction :
              MusynthTypes.llPropT list -> MusynthTypes.llPropT
            val makeDisjunction :
              MusynthTypes.llPropT list -> MusynthTypes.llPropT
          end
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val tracePrintMode : string ref
            val jumpStep : int ref
            val reorderMethods : string list
          end
        module Debug :
          sig
            module Opts :
              sig
                val debugDisabled : bool ref
                val debugOptions : MusynthTypes.StringSet.t ref
                val debugFileName : string ref
                val onlySafety : bool ref
                val conjunctivePart : bool ref
                val inputFileName : string ref
                val numSolsRequested : int ref
                val reorderEnabled : bool ref
                val reorderMethod : Cudd.Man.reorder ref
                val reorderMethods : string list
              end
            val debugOC : out_channel option ref
            val debugFmt : Format.formatter option ref
            val debugEnabled : unit -> bool
            val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
            val getDebugFmt : unit -> Format.formatter
            val initDebugSubsys : string -> unit
            val shutDownDebugSubsys : unit -> unit
            val dprintf :
              MusynthTypes.StringSet.elt ->
              ('a, Format.formatter, unit) format -> 'a
            val dflush : unit -> unit
          end
        class bddManager :
          object
            val mutable bitNameToBddMap :
              Cudd.Man.d Cudd.Bdd.t MusynthTypes.StringMap.t
            val mutable cachedAllButParamCube : Cudd.Man.d Cudd.Bdd.t option
            val mutable cachedAllVarCube : Cudd.Man.d Cudd.Bdd.t option
            val mutable cachedAllVarPrinter :
              (Format.formatter -> Cudd.Man.tbool array -> unit) option
            val mutable cachedConstraintsOnAllVars :
              Cudd.Man.d Cudd.Bdd.t option
            val mutable cachedConstraintsOnParams :
              Cudd.Man.d Cudd.Bdd.t option
            val mutable cachedCubePrinter :
              (Format.formatter -> Cudd.Man.tbool array -> unit) option
            val mutable cachedP2USubstTable :
              Cudd.Man.d Cudd.Bdd.t array option
            val mutable cachedParamVarCube : Cudd.Man.d Cudd.Bdd.t option
            val mutable cachedParamVarPrinter :
              (Format.formatter -> Cudd.Man.tbool array -> unit) option
            val mutable cachedPrimedVarCube : Cudd.Man.d Cudd.Bdd.t option
            val mutable cachedStateVarPrinter :
              (Format.formatter -> Cudd.Man.tbool array -> unit) option
            val mutable cachedU2PSubstTable :
              Cudd.Man.d Cudd.Bdd.t array option
            val mutable cachedUnprimedVarCube : Cudd.Man.d Cudd.Bdd.t option
            val mutable cachedVarCubes :
              Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigSetMap.t
            val mutable indexToBitNameMap : string MusynthTypes.IntMap.t
            val mutable internalStateVars : MusynthTypes.LLDesigSet.t
            val mutable manager : Cudd.Man.d Cudd.Man.t
            val mutable numInternalStateBits : int
            val mutable numParamBits : int
            val mutable numStateBits : int
            val mutable numTotalBits : MusynthTypes.IntSet.elt
            val mutable pStateBitSet : MusynthTypes.IntSet.t
            val mutable paramBitSet : MusynthTypes.IntSet.t
            val mutable paramVars : MusynthTypes.LLDesigSet.t
            val mutable stateBitSet : MusynthTypes.IntSet.t
            val mutable stateVars :
              MusynthTypes.LLDesigMap.key MusynthTypes.LLDesigMap.t
            val mutable varMap :
              (MusynthTypes.LLDesigSet.elt list * MusynthTypes.IntSet.elt *
               int * MusynthTypes.StringMap.key list *
               Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
               MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
               (Cudd.Man.tbool array -> MusynthTypes.LLDesigSet.elt) *
               Cudd.Man.d Cudd.Bdd.t)
              MusynthTypes.LLDesigMap.t
            method private checkVarReregister :
              MusynthTypes.LLDesigMap.key ->
              MusynthTypes.LLDesigSet.elt list ->
              MusynthTypes.LLDesigSet.elt list option
            method cubeOfMinTerm :
              Cudd.Man.tbool array -> Cudd.Man.d Cudd.Bdd.t
            method private determinizeOnSet :
              MusynthTypes.IntSet.t ->
              Cudd.Man.tbool array -> Cudd.Man.tbool array
            method getAllButParamCube : unit -> Cudd.Man.d Cudd.Bdd.t
            method getAllVarPrinter :
              unit -> Format.formatter -> Cudd.Man.tbool array -> unit
            method getBitPrinter :
              unit -> Format.formatter -> MusynthTypes.IntMap.key -> unit
            method getConstraintsOnAllVars : unit -> Cudd.Man.d Cudd.Bdd.t
            method getConstraintsOnParams : unit -> Cudd.Man.d Cudd.Bdd.t
            method getCubeForAllVars : unit -> Cudd.Man.d Cudd.Bdd.t
            method private getCubeForOneVar :
              MusynthTypes.LLDesigMap.key -> Cudd.Man.d Cudd.Bdd.t
            method getCubeForParamVars : unit -> Cudd.Man.d Cudd.Bdd.t
            method getCubeForPrimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
            method getCubeForUnprimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
            method getCubeForVar :
              MusynthTypes.LLDesigSet.elt -> Cudd.Man.d Cudd.Bdd.t
            method getCubeForVars :
              MusynthTypes.LLDesigSet.elt list -> Cudd.Man.d Cudd.Bdd.t
            method getCubePrinter :
              unit -> Format.formatter -> Cudd.Man.tbool array -> unit
            method getNParamVars :
              int ->
              Cudd.Man.d Cudd.Bdd.t ->
              MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list
            method getNStateVars :
              int ->
              Cudd.Man.d Cudd.Bdd.t ->
              MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list
            method getNumInternalStateBits : unit -> int
            method getNumMinTerms : Cudd.Man.d Cudd.Bdd.t -> float
            method getNumMinTermsParam : Cudd.Man.d Cudd.Bdd.t -> float
            method getNumMinTermsState : Cudd.Man.d Cudd.Bdd.t -> float
            method getNumMinTermsStateNI : Cudd.Man.d Cudd.Bdd.t -> float
            method getNumParamBits : unit -> int
            method getNumParamVars : unit -> int
            method getNumStateBits : unit -> int
            method getNumTotalBits : unit -> MusynthTypes.IntSet.elt
            method getParamVarNames :
              unit -> MusynthTypes.LLDesigSet.elt list
            method getParamVarPrinter :
              unit -> Format.formatter -> Cudd.Man.tbool array -> unit
            method getParamVars :
              Cudd.Man.d Cudd.Bdd.t ->
              MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t
            method getPeakBDDSize : unit -> int
            method getStateVarNames :
              unit -> MusynthTypes.LLDesigMap.key list
            method getStateVarNamesNI :
              unit -> MusynthTypes.LLDesigMap.key list
            method getStateVarPrinter :
              unit -> Format.formatter -> Cudd.Man.tbool array -> unit
            method getStateVars :
              Cudd.Man.d Cudd.Bdd.t ->
              MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t
            method getSubstTableP2U : unit -> Cudd.Man.d Cudd.Bdd.t array
            method getSubstTableU2P : unit -> Cudd.Man.d Cudd.Bdd.t array
            method private invalidateCaches : unit -> unit
            method isFalse : Cudd.Man.d Cudd.Bdd.t -> bool
            method private lg : int -> int
            method lookupVar :
              MusynthTypes.LLDesigMap.key ->
              (MusynthTypes.LLDesigSet.elt list * MusynthTypes.IntSet.elt *
               int * MusynthTypes.StringMap.key list *
               Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
               MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
               (Cudd.Man.tbool array -> MusynthTypes.LLDesigSet.elt) *
               Cudd.Man.d Cudd.Bdd.t)
              option
            method private makeBDDForRepr :
              MusynthTypes.IntSet.elt ->
              int -> MusynthTypes.IntMap.key -> Cudd.Man.d Cudd.Bdd.t
            method makeFalse : unit -> Cudd.Man.d Cudd.Bdd.t
            method makeTrue : unit -> Cudd.Man.d Cudd.Bdd.t
            method minimize : unit -> unit
            method pickMinTermOnPStates :
              Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
            method pickMinTermOnParams :
              Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
            method pickMinTermOnStates :
              Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
            method printCubes :
              int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
            method printParamVars :
              int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
            method printStateVars :
              int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
            method prop2BDD : MusynthTypes.llPropT -> Cudd.Man.d Cudd.Bdd.t
            method private registerBits :
              MusynthTypes.LLDesigMap.key ->
              int ->
              MusynthTypes.IntSet.elt * MusynthTypes.StringMap.key list
            method private registerBitsForVar :
              MusynthTypes.IntSet.elt -> int -> MusynthTypes.IntSet.t
            method registerInternalStateVariable :
              MusynthTypes.LLDesigMap.key ->
              MusynthTypes.LLDesigSet.elt list -> unit
            method registerParamVariable :
              MusynthTypes.LLDesigMap.key ->
              MusynthTypes.LLDesigSet.elt list -> unit
            method registerStateVariable :
              MusynthTypes.LLDesigMap.key ->
              MusynthTypes.LLDesigSet.elt list -> unit
            method registerVar :
              MusynthTypes.LLDesigMap.key ->
              MusynthTypes.LLDesigSet.elt list -> unit
            method reset : unit -> unit
            method private substOneVarInTable :
              Cudd.Man.d Cudd.Bdd.t array ->
              MusynthTypes.LLDesigMap.key ->
              MusynthTypes.LLDesigMap.key -> unit
          end
      end
    module MCU :
      sig
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pDupFairness :
              Format.formatter -> MusynthTypes.musDupFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLFairness :
              Format.formatter -> MusynthTypes.llFairnessT -> unit
            val pLLDupFairness :
              Format.formatter -> MusynthTypes.llDupFairnessT -> unit
            val pLLLossFairness :
              Format.formatter -> MusynthTypes.llLossFairnessT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module Debug :
          sig
            module Opts :
              sig
                val debugDisabled : bool ref
                val debugOptions : MusynthTypes.StringSet.t ref
                val debugFileName : string ref
                val onlySafety : bool ref
                val conjunctivePart : bool ref
                val inputFileName : string ref
                val numSolsRequested : int ref
                val reorderEnabled : bool ref
                val reorderMethod : Cudd.Man.reorder ref
                val reorderMethods : string list
              end
            val debugOC : out_channel option ref
            val debugFmt : Format.formatter option ref
            val debugEnabled : unit -> bool
            val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
            val getDebugFmt : unit -> Format.formatter
            val initDebugSubsys : string -> unit
            val shutDownDebugSubsys : unit -> unit
            val dprintf :
              MusynthTypes.StringSet.elt ->
              ('a, Format.formatter, unit) format -> 'a
            val dflush : unit -> unit
          end
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val tracePrintMode : string ref
            val jumpStep : int ref
            val reorderMethods : string list
          end
        module MGR :
          sig
            module CK :
              sig
                module ST :
                  sig
                    val createSymTab :
                      unit -> 'a MusynthTypes.IdentMap.t ref list ref
                    val push :
                      'a MusynthTypes.IdentMap.t ref list ref -> unit
                    val pushScope : 'a list ref -> 'a -> unit
                    val pop : 'a list ref -> 'a
                    val peek : 'a list ref -> 'a
                    val getNumScopes : 'a list ref -> int
                    val lookup :
                      'a MusynthTypes.IdentMap.t ref list ref ->
                      MusynthTypes.IdentMap.key -> 'a option
                    val bind :
                      'a MusynthTypes.IdentMap.t ref list ref ->
                      MusynthTypes.IdentMap.key -> 'a -> unit
                    val bindGlobal :
                      'a MusynthTypes.IdentMap.t ref list ref ->
                      MusynthTypes.IdentMap.key -> 'a -> unit
                    val lookupOrFail :
                      'a MusynthTypes.IdentMap.t ref list ref ->
                      MusynthTypes.IdentMap.key -> 'a
                    val lookupVar :
                      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref
                      list ref ->
                      MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
                    val lookupType :
                      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref
                      list ref ->
                      MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
                    val lookupGMsg :
                      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref
                      list ref ->
                      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                    val lookupAMsg :
                      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref
                      list ref ->
                      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                    val lookupState :
                      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref
                      list ref ->
                      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                    val lookupAutomaton :
                      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref
                      list ref ->
                      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                    val lookupConst :
                      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref
                      list ref ->
                      MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
                  end
                val curAutomatonName : string ref
                val resolveSymType :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref -> MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT
                val checkSymTypeDecl :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT ->
                  unit
                val destructDesigDecl :
                  MusynthTypes.musDesignatorT ->
                  MusynthTypes.identifierT * MusynthTypes.identifierT list
                val getObligationsForIdent :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key ->
                  (string * MusynthTypes.musSymTypeT) list *
                  MusynthTypes.musPropT option
                val getTypeObligationsForIdent :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT list
                val checkTypeLists :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  string * MusynthTypes.sourcelocation option ->
                  MusynthTypes.musSymTypeT list ->
                  MusynthTypes.IdentMap.key list -> unit
                val getDesigType :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry
                val checkTypeCompatibility :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT ->
                  MusynthTypes.musDesignatorT ->
                  MusynthTypes.sourcelocation option ->
                  MusynthTypes.symtabEntry * MusynthTypes.symtabEntry
                val checkPureProp :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musPropT -> unit
                val checkPureQProp :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musPropT -> unit
                val desigDeclChecker :
                  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list
                  ref ->
                  MusynthTypes.musDesignatorT ->
                  'a ->
                  MusynthTypes.identifierT *
                  (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
                val transChecker :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
                  MusynthTypes.musDesignatorT ->
                  MusynthTypes.sourcelocation option -> string * 'a list
                val autMsgDeclChecker :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT ->
                  MusynthTypes.sourcelocation option ->
                  MusynthTypes.identifierT *
                  (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
                val checkDecl :
                  MusynthTypes.symTabScope list ref ->
                  (MusynthTypes.symTabScope list ref ->
                   'a -> MusynthTypes.sourcelocation option -> 'b * 'c) ->
                  'a MusynthTypes.musDeclType ->
                  'b * 'c * MusynthTypes.musPropT option *
                  MusynthTypes.symTabScope
                val cvtParamTypeListForSymtab :
                  (('a * 'b) * 'c) list -> ('a * 'c) list
                val checkGlobalMsgDecl :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val checkStateDecl :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val checkAutomatonMsgDecl :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.msgType ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val checkAnnotationMsgDecl :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val checkAnnotation :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musStateAnnotationT -> unit
                val checkStateDeclBlock :
                  MusynthTypes.symTabScope list ref ->
                  (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                   MusynthTypes.musStateAnnotationT)
                  list -> bool -> unit
                val checkGlobalMsgDeclBlock :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val checkAutomatonMsgDeclBlock :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.msgType ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val convertDesigToPrimed :
                  MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
                val convertDesigDeclToPrimed :
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType
                val checkAutDef :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musAutomatonDeclType ->
                  'a ->
                  MusynthTypes.identifierT *
                  (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
                val initStateDeclChecker :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musPropT list -> unit
                val checkProp :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musPropT -> unit
                val checkSpec :
                  MusynthTypes.symTabScope list ref ->
                  MusynthTypes.musSpecT -> unit
                val checkProg :
                  MusynthTypes.symTabScope list ref ->
                  (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list *
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType
                  list * MusynthTypes.musPropT list *
                  MusynthTypes.musSpecT list -> unit
                val checkLLProg :
                  MusynthTypes.LLDesigSet.elt list *
                  MusynthTypes.llAutomatonT list * 'a * 'b -> unit
              end
            module AST :
              sig
                val pLoc : Format.formatter -> int * int * int * int -> unit
                val pLocOpt :
                  Format.formatter -> (int * int * int * int) option -> unit
                val pIdentifier : Format.formatter -> string * 'a -> unit
                val identToName : 'a * 'b -> 'a
                val astToString :
                  (Format.formatter -> 'a -> 'b) -> 'a -> string
                val pList :
                  string ->
                  bool ->
                  bool ->
                  (Format.formatter -> 'a -> unit) ->
                  Format.formatter -> 'a list -> unit
                val pSymType :
                  Format.formatter -> MusynthTypes.musSymTypeT -> unit
                val pSymTypeDecl :
                  Format.formatter ->
                  (string * 'a) * MusynthTypes.musSymTypeT -> unit
                val musMakeIndentedBox :
                  Format.formatter ->
                  (Format.formatter -> 'a -> unit) ->
                  'a ->
                  (Format.formatter -> 'b -> unit) ->
                  'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
                val pSymTypeDeclBlock :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
                val pDesignator :
                  Format.formatter -> MusynthTypes.musDesignatorT -> unit
                val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
                val pPropOpt :
                  Format.formatter -> MusynthTypes.musPropT option -> unit
                val pDecl :
                  (Format.formatter -> 'a -> 'b) ->
                  (Format.formatter -> 'a -> 'c) ->
                  Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
                val noopPrinter : 'a -> 'b -> unit
                val pMsgDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                  unit
                val pMsgDeclBlock :
                  string ->
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pMessagesDeclBlock :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
                  unit
                val pStateAnnot :
                  Format.formatter ->
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDecl :
                  Format.formatter ->
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                  MusynthTypes.musStateAnnotationT -> unit
                val pStateDeclBlock :
                  string ->
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                   MusynthTypes.musStateAnnotationT)
                  list -> unit
                val pTransDecl :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType -> unit
                val pTransDeclBlock :
                  Format.formatter ->
                  (MusynthTypes.musDesignatorT *
                   MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT)
                  MusynthTypes.musDeclType list -> unit
                val pInitStateConstraint :
                  Format.formatter -> MusynthTypes.musPropT -> unit
                val pInitStateDeclBlock :
                  Format.formatter -> MusynthTypes.musPropT list -> unit
                val pChanProp :
                  Format.formatter ->
                  MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
                  MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
                  int -> unit
                val pFairness :
                  Format.formatter -> MusynthTypes.musFairnessT -> unit
                val pLossFairness :
                  Format.formatter -> MusynthTypes.musLossFairnessT -> unit
                val pDupFairness :
                  Format.formatter -> MusynthTypes.musDupFairnessT -> unit
                val pAutomatonDecl :
                  Format.formatter ->
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
                  unit
                val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
                val pProg :
                  Format.formatter ->
                  ((string * 'a) * MusynthTypes.musSymTypeT) list *
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType
                  list * MusynthTypes.musPropT list *
                  MusynthTypes.musSpecT list -> unit
                val pLLDesignator :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLIdent :
                  Format.formatter -> MusynthTypes.llDesignatorT -> unit
                val pLLVar :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t ->
                  unit
                val pLLAnnot :
                  Format.formatter -> MusynthTypes.llAnnotT -> unit
                val pLLFairness :
                  Format.formatter -> MusynthTypes.llFairnessT -> unit
                val pLLDupFairness :
                  Format.formatter -> MusynthTypes.llDupFairnessT -> unit
                val pLLLossFairness :
                  Format.formatter -> MusynthTypes.llLossFairnessT -> unit
                val pLLTrans :
                  Format.formatter -> MusynthTypes.llTransT -> unit
                val pLLProp :
                  Format.formatter -> MusynthTypes.llPropT -> unit
                val pLLSpec :
                  Format.formatter -> MusynthTypes.llSpecT -> unit
                val pLLAutomaton :
                  Format.formatter -> MusynthTypes.llAutomatonT -> unit
                val pLLProg :
                  Format.formatter ->
                  MusynthTypes.llDesignatorT list *
                  MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
                  MusynthTypes.llSpecT list -> unit
              end
            module Utils :
              sig
                module AST :
                  sig
                    val pLoc :
                      Format.formatter -> int * int * int * int -> unit
                    val pLocOpt :
                      Format.formatter ->
                      (int * int * int * int) option -> unit
                    val pIdentifier : Format.formatter -> string * 'a -> unit
                    val identToName : 'a * 'b -> 'a
                    val astToString :
                      (Format.formatter -> 'a -> 'b) -> 'a -> string
                    val pList :
                      string ->
                      bool ->
                      bool ->
                      (Format.formatter -> 'a -> unit) ->
                      Format.formatter -> 'a list -> unit
                    val pSymType :
                      Format.formatter -> MusynthTypes.musSymTypeT -> unit
                    val pSymTypeDecl :
                      Format.formatter ->
                      (string * 'a) * MusynthTypes.musSymTypeT -> unit
                    val musMakeIndentedBox :
                      Format.formatter ->
                      (Format.formatter -> 'a -> unit) ->
                      'a ->
                      (Format.formatter -> 'b -> unit) ->
                      'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
                    val pSymTypeDeclBlock :
                      Format.formatter ->
                      ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
                    val pDesignator :
                      Format.formatter -> MusynthTypes.musDesignatorT -> unit
                    val pProp :
                      Format.formatter -> MusynthTypes.musPropT -> unit
                    val pPropOpt :
                      Format.formatter ->
                      MusynthTypes.musPropT option -> unit
                    val pDecl :
                      (Format.formatter -> 'a -> 'b) ->
                      (Format.formatter -> 'a -> 'c) ->
                      Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
                    val noopPrinter : 'a -> 'b -> unit
                    val pMsgDecl :
                      Format.formatter ->
                      MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                      unit
                    val pMsgDeclBlock :
                      string ->
                      Format.formatter ->
                      MusynthTypes.musDesignatorT MusynthTypes.musDeclType
                      list -> unit
                    val pMessagesDeclBlock :
                      Format.formatter ->
                      MusynthTypes.musDesignatorT MusynthTypes.musDeclType
                      list -> unit
                    val pStateAnnot :
                      Format.formatter ->
                      MusynthTypes.musStateAnnotationT -> unit
                    val pStateDecl :
                      Format.formatter ->
                      MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                      MusynthTypes.musStateAnnotationT -> unit
                    val pStateDeclBlock :
                      string ->
                      Format.formatter ->
                      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
                       MusynthTypes.musStateAnnotationT)
                      list -> unit
                    val pTransDecl :
                      Format.formatter ->
                      (MusynthTypes.musDesignatorT *
                       MusynthTypes.musDesignatorT *
                       MusynthTypes.musDesignatorT)
                      MusynthTypes.musDeclType -> unit
                    val pTransDeclBlock :
                      Format.formatter ->
                      (MusynthTypes.musDesignatorT *
                       MusynthTypes.musDesignatorT *
                       MusynthTypes.musDesignatorT)
                      MusynthTypes.musDeclType list -> unit
                    val pInitStateConstraint :
                      Format.formatter -> MusynthTypes.musPropT -> unit
                    val pInitStateDeclBlock :
                      Format.formatter -> MusynthTypes.musPropT list -> unit
                    val pChanProp :
                      Format.formatter ->
                      MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
                      MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT *
                      int -> unit
                    val pFairness :
                      Format.formatter -> MusynthTypes.musFairnessT -> unit
                    val pLossFairness :
                      Format.formatter ->
                      MusynthTypes.musLossFairnessT -> unit
                    val pAutomatonDecl :
                      Format.formatter ->
                      MusynthTypes.musAutomatonDeclType
                      MusynthTypes.musDeclType -> unit
                    val pSpec :
                      Format.formatter -> MusynthTypes.musSpecT -> unit
                    val pProg :
                      Format.formatter ->
                      ((string * 'a) * MusynthTypes.musSymTypeT) list *
                      MusynthTypes.musDesignatorT MusynthTypes.musDeclType
                      list *
                      MusynthTypes.musAutomatonDeclType
                      MusynthTypes.musDeclType list *
                      MusynthTypes.musPropT list * MusynthTypes.musSpecT list ->
                      unit
                    val pLLDesignator :
                      Format.formatter -> MusynthTypes.llDesignatorT -> unit
                    val pLLIdent :
                      Format.formatter -> MusynthTypes.llDesignatorT -> unit
                    val pLLVar :
                      Format.formatter ->
                      MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t ->
                      unit
                    val pLLAnnot :
                      Format.formatter -> MusynthTypes.llAnnotT -> unit
                    val pLLTrans :
                      Format.formatter -> MusynthTypes.llTransT -> unit
                    val pLLProp :
                      Format.formatter -> MusynthTypes.llPropT -> unit
                    val pLLSpec :
                      Format.formatter -> MusynthTypes.llSpecT -> unit
                    val pLLAutomaton :
                      Format.formatter -> MusynthTypes.llAutomatonT -> unit
                    val pLLProg :
                      Format.formatter ->
                      MusynthTypes.llDesignatorT list *
                      MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
                      MusynthTypes.llSpecT list -> unit
                  end
                module MSSet :
                  sig
                    type elt = int MusynthTypes.LLDesigMap.t
                    type t = MusynthUtils.MSSet.t
                    val empty : t
                    val is_empty : t -> bool
                    val mem : elt -> t -> bool
                    val add : elt -> t -> t
                    val singleton : elt -> t
                    val remove : elt -> t -> t
                    val union : t -> t -> t
                    val inter : t -> t -> t
                    val diff : t -> t -> t
                    val compare : t -> t -> int
                    val equal : t -> t -> bool
                    val subset : t -> t -> bool
                    val iter : (elt -> unit) -> t -> unit
                    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
                    val for_all : (elt -> bool) -> t -> bool
                    val exists : (elt -> bool) -> t -> bool
                    val filter : (elt -> bool) -> t -> t
                    val partition : (elt -> bool) -> t -> t * t
                    val cardinal : t -> int
                    val elements : t -> elt list
                    val min_elt : t -> elt
                    val max_elt : t -> elt
                    val choose : t -> elt
                    val split : elt -> t -> t * bool * t
                  end
                val splatList : 'a -> int -> 'a list
                val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
                val msToList :
                  int MusynthTypes.LLDesigMap.t ->
                  MusynthTypes.LLDesigMap.key list
                val msToStr :
                  (MusynthTypes.LLDesigMap.key -> string) ->
                  int MusynthTypes.LLDesigMap.t -> string
                val addToMs :
                  MusynthTypes.LLDesigMap.key ->
                  int MusynthTypes.LLDesigMap.t ->
                  int MusynthTypes.LLDesigMap.t
                val delFromMS :
                  MusynthTypes.LLDesigMap.key ->
                  int MusynthTypes.LLDesigMap.t ->
                  int MusynthTypes.LLDesigMap.t
                val makeEmptyMSWithAlphabet :
                  MusynthTypes.LLDesigMap.key list ->
                  int MusynthTypes.LLDesigMap.t
                val enumerateMS :
                  MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
                val enumerateLists : 'a list -> int -> 'a list list
                val listToStr : ('a -> string) -> 'a list -> string
                val nextuid : int ref
                val getuid : unit -> int
                val resetuid : unit -> unit
                val crossProduct : 'a list list -> 'a list list
                val identConstPairList2Map :
                  (MusynthTypes.IdentMap.key * 'a) list ->
                  'a MusynthTypes.IdentMap.t
                val sDesigToIdent :
                  MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
                val conjoinPropOpts :
                  MusynthTypes.musPropT option ->
                  MusynthTypes.musPropT option ->
                  MusynthTypes.musPropT option
                val mergeIdentMaps :
                  'a MusynthTypes.IdentMap.t ->
                  'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
                val evalProp :
                  MusynthTypes.musPropT ->
                  ('a * 'b) MusynthTypes.IdentMap.t -> bool
                val getMapsForProp :
                  MusynthTypes.IdentMap.key list ->
                  MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
                  MusynthTypes.musPropT option ->
                  MusynthTypes.identifierT MusynthTypes.IdentMap.t list
                val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
                val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
                val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
                val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
                val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
                val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
                val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
                val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
                val getMsgsForAut :
                  MusynthTypes.llAutomatonT ->
                  MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
                val getNameForAut :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
                val getTransitionsForAut :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
                val getStatesForAut :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
                val getAutomatonByName :
                  MusynthTypes.llAutomatonT list ->
                  MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
                val getFairnessForAutomaton :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
                val getLFairnessForAutomaton :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
                val getDFairnessForAutomaton :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
                val getSender :
                  MusynthTypes.llIdentT ->
                  MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
                val getReceivers :
                  MusynthTypes.llIdentT ->
                  MusynthTypes.llAutomatonT list ->
                  MusynthTypes.llAutomatonT list
                val getStateNameForAutomaton :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
                val getStateNamePForAutomaton :
                  MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
                val getCSPredsForMsg :
                  MusynthTypes.llIdentT ->
                  MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
                val getCSPredsForMsgAll :
                  MusynthTypes.llIdentT ->
                  MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
                val getMsgsToSyncOnFromState :
                  MusynthTypes.llAutomatonT ->
                  MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
                val getStatesFromWhichMsgSync :
                  MusynthTypes.llAutomatonT ->
                  MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
                val canonicalizeProp :
                  MusynthTypes.llPropT -> MusynthTypes.llPropT
                val canonicalizePropFP :
                  MusynthTypes.llPropT -> MusynthTypes.llPropT
                val makeFormatterOfName :
                  string -> out_channel * Format.formatter
                val makeConjunction :
                  MusynthTypes.llPropT list -> MusynthTypes.llPropT
                val makeDisjunction :
                  MusynthTypes.llPropT list -> MusynthTypes.llPropT
              end
            module Opts :
              sig
                val debugDisabled : bool ref
                val debugOptions : MusynthTypes.StringSet.t ref
                val debugFileName : string ref
                val onlySafety : bool ref
                val conjunctivePart : bool ref
                val inputFileName : string ref
                val numSolsRequested : int ref
                val reorderEnabled : bool ref
                val reorderMethod : Cudd.Man.reorder ref
                val tracePrintMode : string ref
                val jumpStep : int ref
                val reorderMethods : string list
              end
            module Debug :
              sig
                module Opts :
                  sig
                    val debugDisabled : bool ref
                    val debugOptions : MusynthTypes.StringSet.t ref
                    val debugFileName : string ref
                    val onlySafety : bool ref
                    val conjunctivePart : bool ref
                    val inputFileName : string ref
                    val numSolsRequested : int ref
                    val reorderEnabled : bool ref
                    val reorderMethod : Cudd.Man.reorder ref
                    val reorderMethods : string list
                  end
                val debugOC : out_channel option ref
                val debugFmt : Format.formatter option ref
                val debugEnabled : unit -> bool
                val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
                val getDebugFmt : unit -> Format.formatter
                val initDebugSubsys : string -> unit
                val shutDownDebugSubsys : unit -> unit
                val dprintf :
                  MusynthTypes.StringSet.elt ->
                  ('a, Format.formatter, unit) format -> 'a
                val dflush : unit -> unit
              end
            class bddManager :
              object
                val mutable bitNameToBddMap :
                  Cudd.Man.d Cudd.Bdd.t MusynthTypes.StringMap.t
                val mutable cachedAllButParamCube :
                  Cudd.Man.d Cudd.Bdd.t option
                val mutable cachedAllVarCube : Cudd.Man.d Cudd.Bdd.t option
                val mutable cachedAllVarPrinter :
                  (Format.formatter -> Cudd.Man.tbool array -> unit) option
                val mutable cachedConstraintsOnAllVars :
                  Cudd.Man.d Cudd.Bdd.t option
                val mutable cachedConstraintsOnParams :
                  Cudd.Man.d Cudd.Bdd.t option
                val mutable cachedCubePrinter :
                  (Format.formatter -> Cudd.Man.tbool array -> unit) option
                val mutable cachedP2USubstTable :
                  Cudd.Man.d Cudd.Bdd.t array option
                val mutable cachedParamVarCube : Cudd.Man.d Cudd.Bdd.t option
                val mutable cachedParamVarPrinter :
                  (Format.formatter -> Cudd.Man.tbool array -> unit) option
                val mutable cachedPrimedVarCube :
                  Cudd.Man.d Cudd.Bdd.t option
                val mutable cachedStateVarPrinter :
                  (Format.formatter -> Cudd.Man.tbool array -> unit) option
                val mutable cachedU2PSubstTable :
                  Cudd.Man.d Cudd.Bdd.t array option
                val mutable cachedUnprimedVarCube :
                  Cudd.Man.d Cudd.Bdd.t option
                val mutable cachedVarCubes :
                  Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigSetMap.t
                val mutable indexToBitNameMap : string MusynthTypes.IntMap.t
                val mutable internalStateVars : MusynthTypes.LLDesigSet.t
                val mutable manager : Cudd.Man.d Cudd.Man.t
                val mutable numInternalStateBits : int
                val mutable numParamBits : int
                val mutable numStateBits : int
                val mutable numTotalBits : MusynthTypes.IntSet.elt
                val mutable pStateBitSet : MusynthTypes.IntSet.t
                val mutable paramBitSet : MusynthTypes.IntSet.t
                val mutable paramVars : MusynthTypes.LLDesigSet.t
                val mutable stateBitSet : MusynthTypes.IntSet.t
                val mutable stateVars :
                  MusynthTypes.LLDesigMap.key MusynthTypes.LLDesigMap.t
                val mutable varMap :
                  (MusynthTypes.LLDesigSet.elt list *
                   MusynthTypes.IntSet.elt * int *
                   MusynthTypes.StringMap.key list *
                   Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
                   MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
                   (Cudd.Man.tbool array -> MusynthTypes.LLDesigSet.elt) *
                   Cudd.Man.d Cudd.Bdd.t)
                  MusynthTypes.LLDesigMap.t
                method private checkVarReregister :
                  MusynthTypes.LLDesigMap.key ->
                  MusynthTypes.LLDesigSet.elt list ->
                  MusynthTypes.LLDesigSet.elt list option
                method cubeOfMinTerm :
                  Cudd.Man.tbool array -> Cudd.Man.d Cudd.Bdd.t
                method private determinizeOnSet :
                  MusynthTypes.IntSet.t ->
                  Cudd.Man.tbool array -> Cudd.Man.tbool array
                method getAllButParamCube : unit -> Cudd.Man.d Cudd.Bdd.t
                method getAllVarPrinter :
                  unit -> Format.formatter -> Cudd.Man.tbool array -> unit
                method getBitPrinter :
                  unit -> Format.formatter -> MusynthTypes.IntMap.key -> unit
                method getConstraintsOnAllVars :
                  unit -> Cudd.Man.d Cudd.Bdd.t
                method getConstraintsOnParams : unit -> Cudd.Man.d Cudd.Bdd.t
                method getCubeForAllVars : unit -> Cudd.Man.d Cudd.Bdd.t
                method private getCubeForOneVar :
                  MusynthTypes.LLDesigMap.key -> Cudd.Man.d Cudd.Bdd.t
                method getCubeForParamVars : unit -> Cudd.Man.d Cudd.Bdd.t
                method getCubeForPrimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
                method getCubeForUnprimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
                method getCubeForVar :
                  MusynthTypes.LLDesigSet.elt -> Cudd.Man.d Cudd.Bdd.t
                method getCubeForVars :
                  MusynthTypes.LLDesigSet.elt list -> Cudd.Man.d Cudd.Bdd.t
                method getCubePrinter :
                  unit -> Format.formatter -> Cudd.Man.tbool array -> unit
                method getNParamVars :
                  int ->
                  Cudd.Man.d Cudd.Bdd.t ->
                  MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list
                method getNStateVars :
                  int ->
                  Cudd.Man.d Cudd.Bdd.t ->
                  MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list
                method getNumInternalStateBits : unit -> int
                method getNumMinTerms : Cudd.Man.d Cudd.Bdd.t -> float
                method getNumMinTermsParam : Cudd.Man.d Cudd.Bdd.t -> float
                method getNumMinTermsState : Cudd.Man.d Cudd.Bdd.t -> float
                method getNumMinTermsStateNI : Cudd.Man.d Cudd.Bdd.t -> float
                method getNumParamBits : unit -> int
                method getNumParamVars : unit -> int
                method getNumStateBits : unit -> int
                method getNumTotalBits : unit -> MusynthTypes.IntSet.elt
                method getParamVarNames :
                  unit -> MusynthTypes.LLDesigSet.elt list
                method getParamVarPrinter :
                  unit -> Format.formatter -> Cudd.Man.tbool array -> unit
                method getParamVars :
                  Cudd.Man.d Cudd.Bdd.t ->
                  MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t
                method getPeakBDDSize : unit -> int
                method getStateVarNames :
                  unit -> MusynthTypes.LLDesigMap.key list
                method getStateVarNamesNI :
                  unit -> MusynthTypes.LLDesigMap.key list
                method getStateVarPrinter :
                  unit -> Format.formatter -> Cudd.Man.tbool array -> unit
                method getStateVars :
                  Cudd.Man.d Cudd.Bdd.t ->
                  MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t
                method getSubstTableP2U : unit -> Cudd.Man.d Cudd.Bdd.t array
                method getSubstTableU2P : unit -> Cudd.Man.d Cudd.Bdd.t array
                method private invalidateCaches : unit -> unit
                method isFalse : Cudd.Man.d Cudd.Bdd.t -> bool
                method private lg : int -> int
                method lookupVar :
                  MusynthTypes.LLDesigMap.key ->
                  (MusynthTypes.LLDesigSet.elt list *
                   MusynthTypes.IntSet.elt * int *
                   MusynthTypes.StringMap.key list *
                   Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
                   MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
                   (Cudd.Man.tbool array -> MusynthTypes.LLDesigSet.elt) *
                   Cudd.Man.d Cudd.Bdd.t)
                  option
                method private makeBDDForRepr :
                  MusynthTypes.IntSet.elt ->
                  int -> MusynthTypes.IntMap.key -> Cudd.Man.d Cudd.Bdd.t
                method makeFalse : unit -> Cudd.Man.d Cudd.Bdd.t
                method makeTrue : unit -> Cudd.Man.d Cudd.Bdd.t
                method minimize : unit -> unit
                method pickMinTermOnPStates :
                  Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
                method pickMinTermOnParams :
                  Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
                method pickMinTermOnStates :
                  Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
                method printCubes :
                  int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
                method printParamVars :
                  int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
                method printStateVars :
                  int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
                method prop2BDD :
                  MusynthTypes.llPropT -> Cudd.Man.d Cudd.Bdd.t
                method private registerBits :
                  MusynthTypes.LLDesigMap.key ->
                  int ->
                  MusynthTypes.IntSet.elt * MusynthTypes.StringMap.key list
                method private registerBitsForVar :
                  MusynthTypes.IntSet.elt -> int -> MusynthTypes.IntSet.t
                method registerInternalStateVariable :
                  MusynthTypes.LLDesigMap.key ->
                  MusynthTypes.LLDesigSet.elt list -> unit
                method registerParamVariable :
                  MusynthTypes.LLDesigMap.key ->
                  MusynthTypes.LLDesigSet.elt list -> unit
                method registerStateVariable :
                  MusynthTypes.LLDesigMap.key ->
                  MusynthTypes.LLDesigSet.elt list -> unit
                method registerVar :
                  MusynthTypes.LLDesigMap.key ->
                  MusynthTypes.LLDesigSet.elt list -> unit
                method reset : unit -> unit
                method private substOneVarInTable :
                  Cudd.Man.d Cudd.Bdd.t array ->
                  MusynthTypes.LLDesigMap.key ->
                  MusynthTypes.LLDesigMap.key -> unit
              end
          end
        val post :
          < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
        val pre :
          < getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableU2P : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
        val inclusionFixPointTester : 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> bool
        val eqFixPointTester : 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> bool
        val postAndTransformer :
          < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
        val postOrTransformer :
          < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
        val preAndTransformer :
          < getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableU2P : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
        val preOrTransformer :
          < getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableU2P : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
        val postK :
          int ->
          < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t MusynthTypes.execExitStatT
        val preK :
          int ->
          < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t -> 'a Cudd.Bdd.t MusynthTypes.execExitStatT
        val computeFixPoint : ('a -> 'a) -> ('a -> 'a -> bool) -> 'a -> 'a
        val findPathCube :
          < cubeOfMinTerm : 'a -> 'b Cudd.Bdd.t;
            getCubeForPrimedVars : unit -> 'b Cudd.Bdd.t;
            getCubeForUnprimedVars : unit -> 'b Cudd.Bdd.t;
            getSubstTableP2U : unit -> 'b Cudd.Bdd.t array;
            getSubstTableU2P : unit -> 'b Cudd.Bdd.t array;
            pickMinTermOnStates : 'b Cudd.Bdd.t -> 'a; .. > ->
          'b Cudd.Bdd.t ->
          'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t list
        val findPath :
          < cubeOfMinTerm : 'a -> 'b Cudd.Bdd.t;
            getCubeForPrimedVars : unit -> 'b Cudd.Bdd.t;
            getCubeForUnprimedVars : unit -> 'b Cudd.Bdd.t;
            getStateVars : 'b Cudd.Bdd.t -> 'c;
            getSubstTableP2U : unit -> 'b Cudd.Bdd.t array;
            getSubstTableU2P : unit -> 'b Cudd.Bdd.t array;
            pickMinTermOnStates : 'b Cudd.Bdd.t -> 'a; .. > ->
          'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'c list
        val findLoopCube :
          < cubeOfMinTerm : Cudd.Man.tbool array -> 'a Cudd.Bdd.t;
            getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
            getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
            getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
            getSubstTableU2P : unit -> 'a Cudd.Bdd.t array;
            pickMinTermOnStates : 'a Cudd.Bdd.t -> Cudd.Man.tbool array; .. > ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t list ->
          ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list ->
          'a Cudd.Bdd.t list * 'a Cudd.Bdd.t list
        val findLoop :
          < cubeOfMinTerm : Cudd.Man.tbool array -> 'a Cudd.Bdd.t;
            getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
            getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
            getStateVars : 'a Cudd.Bdd.t -> 'b;
            getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
            getSubstTableU2P : unit -> 'a Cudd.Bdd.t array;
            pickMinTermOnStates : 'a Cudd.Bdd.t -> Cudd.Man.tbool array; .. > ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t ->
          'a Cudd.Bdd.t list ->
          ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list -> 'b list * 'b list
      end
    module Utils :
      sig
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module MSSet :
          sig
            type elt = int MusynthTypes.LLDesigMap.t
            type t = MusynthUtils.MSSet.t
            val empty : t
            val is_empty : t -> bool
            val mem : elt -> t -> bool
            val add : elt -> t -> t
            val singleton : elt -> t
            val remove : elt -> t -> t
            val union : t -> t -> t
            val inter : t -> t -> t
            val diff : t -> t -> t
            val compare : t -> t -> int
            val equal : t -> t -> bool
            val subset : t -> t -> bool
            val iter : (elt -> unit) -> t -> unit
            val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
            val for_all : (elt -> bool) -> t -> bool
            val exists : (elt -> bool) -> t -> bool
            val filter : (elt -> bool) -> t -> t
            val partition : (elt -> bool) -> t -> t * t
            val cardinal : t -> int
            val elements : t -> elt list
            val min_elt : t -> elt
            val max_elt : t -> elt
            val choose : t -> elt
            val split : elt -> t -> t * bool * t
          end
        val splatList : 'a -> int -> 'a list
        val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
        val msToList :
          int MusynthTypes.LLDesigMap.t -> MusynthTypes.LLDesigMap.key list
        val msToStr :
          (MusynthTypes.LLDesigMap.key -> string) ->
          int MusynthTypes.LLDesigMap.t -> string
        val addToMs :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val delFromMS :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val makeEmptyMSWithAlphabet :
          MusynthTypes.LLDesigMap.key list -> int MusynthTypes.LLDesigMap.t
        val enumerateMS :
          MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
        val enumerateLists : 'a list -> int -> 'a list list
        val listToStr : ('a -> string) -> 'a list -> string
        val nextuid : int ref
        val getuid : unit -> int
        val resetuid : unit -> unit
        val crossProduct : 'a list list -> 'a list list
        val identConstPairList2Map :
          (MusynthTypes.IdentMap.key * 'a) list -> 'a MusynthTypes.IdentMap.t
        val sDesigToIdent :
          MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
        val conjoinPropOpts :
          MusynthTypes.musPropT option ->
          MusynthTypes.musPropT option -> MusynthTypes.musPropT option
        val mergeIdentMaps :
          'a MusynthTypes.IdentMap.t ->
          'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
        val evalProp :
          MusynthTypes.musPropT -> ('a * 'b) MusynthTypes.IdentMap.t -> bool
        val getMapsForProp :
          MusynthTypes.IdentMap.key list ->
          MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
          MusynthTypes.musPropT option ->
          MusynthTypes.identifierT MusynthTypes.IdentMap.t list
        val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
        val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
        val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
        val getMsgsForAut :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
        val getNameForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
        val getTransitionsForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
        val getStatesForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
        val getAutomatonByName :
          MusynthTypes.llAutomatonT list ->
          MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
        val getFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
        val getLFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
        val getDFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
        val getSender :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
        val getReceivers :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT list
        val getStateNameForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getStateNamePForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getCSPredsForMsg :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
        val getCSPredsForMsgAll :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
        val getMsgsToSyncOnFromState :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val getStatesFromWhichMsgSync :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val canonicalizeProp : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val canonicalizePropFP : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val makeFormatterOfName : string -> out_channel * Format.formatter
        val makeConjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
        val makeDisjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
      end
    val getSafetyParams :
      < cubeOfMinTerm : 'a -> 'b Cudd.Bdd.t;
        getAllButParamCube : unit -> 'b Cudd.Bdd.t;
        getCubeForPrimedVars : unit -> 'b Cudd.Bdd.t;
        getCubeForUnprimedVars : unit -> 'b Cudd.Bdd.t;
        getStateVars : 'b Cudd.Bdd.t ->
                       MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t;
        getSubstTableP2U : unit -> 'b Cudd.Bdd.t array;
        getSubstTableU2P : unit -> 'b Cudd.Bdd.t array;
        makeTrue : unit -> 'b Cudd.Bdd.t;
        pickMinTermOnStates : 'b Cudd.Bdd.t -> 'a; .. > ->
      'b Cudd.Bdd.t ->
      'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t
    val getFeasible :
      < getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
        getSubstTableU2P : unit -> 'a Cudd.Bdd.t array; .. > ->
      'b ->
      'a Cudd.Bdd.t ->
      'c ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t list ->
      ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list -> 'a Cudd.Bdd.t
    val getParamsForInfeasible :
      < cubeOfMinTerm : Cudd.Man.tbool array -> 'a Cudd.Bdd.t;
        getAllButParamCube : unit -> 'a Cudd.Bdd.t;
        getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
        getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
        getStateVars : 'a Cudd.Bdd.t ->
                       MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t;
        getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
        getSubstTableU2P : unit -> 'a Cudd.Bdd.t array;
        makeTrue : unit -> 'a Cudd.Bdd.t;
        pickMinTermOnStates : 'a Cudd.Bdd.t -> Cudd.Man.tbool array; .. > ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t list ->
      ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list -> string -> 'a Cudd.Bdd.t
    val getParamsForKSteps :
      int ->
      'a Cudd.Bdd.t ->
      < cubeOfMinTerm : Cudd.Man.tbool array -> 'a Cudd.Bdd.t;
        getAllButParamCube : unit -> 'a Cudd.Bdd.t;
        getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
        getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
        getNumMinTermsParam : 'a Cudd.Bdd.t -> float;
        getStateVars : 'a Cudd.Bdd.t ->
                       MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t;
        getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
        getSubstTableU2P : unit -> 'a Cudd.Bdd.t array;
        makeTrue : unit -> 'a Cudd.Bdd.t;
        pickMinTermOnStates : 'a Cudd.Bdd.t -> Cudd.Man.tbool array; .. > ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      ('b * 'c * 'd * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t list *
       ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list)
      MusynthTypes.StringMap.t -> 'a Cudd.Bdd.t MusynthTypes.execExitStatT
    val conjoinTransitionRels :
      < makeTrue : unit -> 'a Cudd.Bdd.t; .. > ->
      'a Cudd.Bdd.t MusynthTypes.LLDesigMap.t -> 'a Cudd.Bdd.t
    val synthFrontEndInternal :
      < cubeOfMinTerm : Cudd.Man.tbool array -> 'a Cudd.Bdd.t;
        getAllButParamCube : unit -> 'a Cudd.Bdd.t;
        getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
        getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
        getNumMinTermsParam : 'a Cudd.Bdd.t -> float;
        getStateVars : 'a Cudd.Bdd.t ->
                       MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t;
        getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
        getSubstTableU2P : unit -> 'a Cudd.Bdd.t array;
        makeTrue : unit -> 'a Cudd.Bdd.t; minimize : unit -> 'b;
        pickMinTermOnStates : 'a Cudd.Bdd.t -> Cudd.Man.tbool array; .. > ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t MusynthTypes.LLDesigMap.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      ('c * 'd * 'e * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t list *
       ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list)
      MusynthTypes.StringMap.t -> 'a Cudd.Bdd.t
    val synthFrontEnd :
      < cubeOfMinTerm : Cudd.Man.tbool array -> 'a Cudd.Bdd.t;
        getAllButParamCube : unit -> 'a Cudd.Bdd.t;
        getBitPrinter : unit -> Format.formatter -> int -> unit;
        getConstraintsOnParams : unit -> 'a Cudd.Bdd.t;
        getCubeForParamVars : unit -> 'a Cudd.Bdd.t;
        getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
        getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
        getNumMinTermsParam : 'a Cudd.Bdd.t -> float;
        getNumParamBits : unit -> int; getNumStateBits : unit -> int;
        getParamVarNames : unit -> MusynthTypes.llDesignatorT list;
        getStateVars : 'a Cudd.Bdd.t ->
                       MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t;
        getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
        getSubstTableU2P : unit -> 'a Cudd.Bdd.t array;
        makeFalse : unit -> 'a Cudd.Bdd.t; makeTrue : unit -> 'a Cudd.Bdd.t;
        minimize : unit -> 'b;
        pickMinTermOnStates : 'a Cudd.Bdd.t -> Cudd.Man.tbool array;
        prop2BDD : MusynthTypes.llPropT -> 'a Cudd.Bdd.t; .. > ->
      'a Cudd.Bdd.t MusynthTypes.LLDesigMap.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      ('c * 'd * 'e * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t list *
       ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list)
      MusynthTypes.StringMap.t -> 'a Cudd.Bdd.t
    val check :
      < cubeOfMinTerm : Cudd.Man.tbool array -> 'a Cudd.Bdd.t;
        getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
        getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
        getStateVars : 'a Cudd.Bdd.t ->
                       MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t;
        getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
        getSubstTableU2P : unit -> 'a Cudd.Bdd.t array;
        makeTrue : unit -> 'a Cudd.Bdd.t;
        pickMinTermOnStates : 'a Cudd.Bdd.t -> Cudd.Man.tbool array; .. > ->
      'a Cudd.Bdd.t MusynthTypes.LLDesigMap.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      'a Cudd.Bdd.t ->
      ('b * 'c * 'd * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t * 'a Cudd.Bdd.t list *
       ('a Cudd.Bdd.t * 'a Cudd.Bdd.t) list)
      MusynthTypes.StringMap.t ->
      'a Cudd.Bdd.t MusynthTypes.modelCheckingStatusT
  end
module Opts :
  sig
    val debugDisabled : bool ref
    val debugOptions : MusynthTypes.StringSet.t ref
    val debugFileName : string ref
    val onlySafety : bool ref
    val conjunctivePart : bool ref
    val inputFileName : string ref
    val numSolsRequested : int ref
    val reorderEnabled : bool ref
    val reorderMethod : Cudd.Man.reorder ref
    val tracePrintMode : string ref
    val jumpStep : int ref
    val reorderMethods : string list
  end
module Utils :
  sig
    module AST :
      sig
        val pLoc : Format.formatter -> int * int * int * int -> unit
        val pLocOpt :
          Format.formatter -> (int * int * int * int) option -> unit
        val pIdentifier : Format.formatter -> string * 'a -> unit
        val identToName : 'a * 'b -> 'a
        val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
        val pList :
          string ->
          bool ->
          bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> 'a list -> unit
        val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
        val pSymTypeDecl :
          Format.formatter ->
          (string * 'a) * MusynthTypes.musSymTypeT -> unit
        val musMakeIndentedBox :
          Format.formatter ->
          (Format.formatter -> 'a -> unit) ->
          'a ->
          (Format.formatter -> 'b -> unit) ->
          'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
        val pSymTypeDeclBlock :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
        val pDesignator :
          Format.formatter -> MusynthTypes.musDesignatorT -> unit
        val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
        val pPropOpt :
          Format.formatter -> MusynthTypes.musPropT option -> unit
        val pDecl :
          (Format.formatter -> 'a -> 'b) ->
          (Format.formatter -> 'a -> 'c) ->
          Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
        val noopPrinter : 'a -> 'b -> unit
        val pMsgDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val pMsgDeclBlock :
          string ->
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pMessagesDeclBlock :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pStateAnnot :
          Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
        val pStateDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
          MusynthTypes.musStateAnnotationT -> unit
        val pStateDeclBlock :
          string ->
          Format.formatter ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> unit
        val pTransDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType -> unit
        val pTransDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType list -> unit
        val pInitStateConstraint :
          Format.formatter -> MusynthTypes.musPropT -> unit
        val pInitStateDeclBlock :
          Format.formatter -> MusynthTypes.musPropT list -> unit
        val pChanProp :
          Format.formatter ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
          unit
        val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
        val pLossFairness :
          Format.formatter -> MusynthTypes.musLossFairnessT -> unit
        val pAutomatonDecl :
          Format.formatter ->
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
        val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
        val pProg :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val pLLDesignator :
          Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLVar :
          Format.formatter ->
          MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
        val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
        val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
        val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
        val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
        val pLLAutomaton :
          Format.formatter -> MusynthTypes.llAutomatonT -> unit
        val pLLProg :
          Format.formatter ->
          MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
          MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
      end
    module MSSet :
      sig
        type elt = int MusynthTypes.LLDesigMap.t
        type t = MusynthUtils.MSSet.t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val max_elt : t -> elt
        val choose : t -> elt
        val split : elt -> t -> t * bool * t
      end
    val splatList : 'a -> int -> 'a list
    val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
    val msToList :
      int MusynthTypes.LLDesigMap.t -> MusynthTypes.LLDesigMap.key list
    val msToStr :
      (MusynthTypes.LLDesigMap.key -> string) ->
      int MusynthTypes.LLDesigMap.t -> string
    val addToMs :
      MusynthTypes.LLDesigMap.key ->
      int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
    val delFromMS :
      MusynthTypes.LLDesigMap.key ->
      int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
    val makeEmptyMSWithAlphabet :
      MusynthTypes.LLDesigMap.key list -> int MusynthTypes.LLDesigMap.t
    val enumerateMS :
      MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
    val enumerateLists : 'a list -> int -> 'a list list
    val listToStr : ('a -> string) -> 'a list -> string
    val nextuid : int ref
    val getuid : unit -> int
    val resetuid : unit -> unit
    val crossProduct : 'a list list -> 'a list list
    val identConstPairList2Map :
      (MusynthTypes.IdentMap.key * 'a) list -> 'a MusynthTypes.IdentMap.t
    val sDesigToIdent :
      MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
    val conjoinPropOpts :
      MusynthTypes.musPropT option ->
      MusynthTypes.musPropT option -> MusynthTypes.musPropT option
    val mergeIdentMaps :
      'a MusynthTypes.IdentMap.t ->
      'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
    val evalProp :
      MusynthTypes.musPropT -> ('a * 'b) MusynthTypes.IdentMap.t -> bool
    val getMapsForProp :
      MusynthTypes.IdentMap.key list ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.identifierT MusynthTypes.IdentMap.t list
    val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
    val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
    val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
    val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
    val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
    val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
    val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
    val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
    val getMsgsForAut :
      MusynthTypes.llAutomatonT ->
      MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
    val getNameForAut : MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
    val getTransitionsForAut :
      MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
    val getStatesForAut :
      MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
    val getAutomatonByName :
      MusynthTypes.llAutomatonT list ->
      MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
    val getFairnessForAutomaton :
      MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
    val getLFairnessForAutomaton :
      MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
    val getDFairnessForAutomaton :
      MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
    val getSender :
      MusynthTypes.llIdentT ->
      MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
    val getReceivers :
      MusynthTypes.llIdentT ->
      MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT list
    val getStateNameForAutomaton :
      MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
    val getStateNamePForAutomaton :
      MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
    val getCSPredsForMsg :
      MusynthTypes.llIdentT ->
      MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
    val getCSPredsForMsgAll :
      MusynthTypes.llIdentT ->
      MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
    val getMsgsToSyncOnFromState :
      MusynthTypes.llAutomatonT ->
      MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
    val getStatesFromWhichMsgSync :
      MusynthTypes.llAutomatonT ->
      MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
    val canonicalizeProp : MusynthTypes.llPropT -> MusynthTypes.llPropT
    val canonicalizePropFP : MusynthTypes.llPropT -> MusynthTypes.llPropT
    val makeFormatterOfName : string -> out_channel * Format.formatter
    val makeConjunction : MusynthTypes.llPropT list -> MusynthTypes.llPropT
    val makeDisjunction : MusynthTypes.llPropT list -> MusynthTypes.llPropT
  end
module Mgr :
  sig
    module CK :
      sig
        module ST :
          sig
            val createSymTab :
              unit -> 'a MusynthTypes.IdentMap.t ref list ref
            val push : 'a MusynthTypes.IdentMap.t ref list ref -> unit
            val pushScope : 'a list ref -> 'a -> unit
            val pop : 'a list ref -> 'a
            val peek : 'a list ref -> 'a
            val getNumScopes : 'a list ref -> int
            val lookup :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a option
            val bind :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a -> unit
            val bindGlobal :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a -> unit
            val lookupOrFail :
              'a MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> 'a
            val lookupVar :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
            val lookupType :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
            val lookupGMsg :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupAMsg :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupState :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupAutomaton :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
            val lookupConst :
              MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
              MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
          end
        val curAutomatonName : string ref
        val resolveSymType :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT
        val checkSymTypeDecl :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT -> unit
        val destructDesigDecl :
          MusynthTypes.musDesignatorT ->
          MusynthTypes.identifierT * MusynthTypes.identifierT list
        val getObligationsForIdent :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key ->
          (string * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musPropT option
        val getTypeObligationsForIdent :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT list
        val checkTypeLists :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          string * MusynthTypes.sourcelocation option ->
          MusynthTypes.musSymTypeT list ->
          MusynthTypes.IdentMap.key list -> unit
        val getDesigType :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry
        val checkTypeCompatibility :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT ->
          MusynthTypes.musDesignatorT ->
          MusynthTypes.sourcelocation option ->
          MusynthTypes.symtabEntry * MusynthTypes.symtabEntry
        val checkPureProp :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
        val checkPureQProp :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
        val desigDeclChecker :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.musDesignatorT ->
          'a ->
          MusynthTypes.identifierT *
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
        val transChecker :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
          MusynthTypes.musDesignatorT ->
          MusynthTypes.sourcelocation option -> string * 'a list
        val autMsgDeclChecker :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT ->
          MusynthTypes.sourcelocation option ->
          MusynthTypes.identifierT *
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
        val checkDecl :
          MusynthTypes.symTabScope list ref ->
          (MusynthTypes.symTabScope list ref ->
           'a -> MusynthTypes.sourcelocation option -> 'b * 'c) ->
          'a MusynthTypes.musDeclType ->
          'b * 'c * MusynthTypes.musPropT option * MusynthTypes.symTabScope
        val cvtParamTypeListForSymtab :
          (('a * 'b) * 'c) list -> ('a * 'c) list
        val checkGlobalMsgDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkStateDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkAutomatonMsgDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.msgType ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkAnnotationMsgDecl :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val checkAnnotation :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musStateAnnotationT -> unit
        val checkStateDeclBlock :
          MusynthTypes.symTabScope list ref ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> bool -> unit
        val checkGlobalMsgDeclBlock :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val checkAutomatonMsgDeclBlock :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.msgType ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val convertDesigToPrimed :
          MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
        val convertDesigDeclToPrimed :
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType
        val checkAutDef :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musAutomatonDeclType ->
          'a ->
          MusynthTypes.identifierT *
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
        val initStateDeclChecker :
          MusynthTypes.symTabScope list ref ->
          MusynthTypes.musPropT list -> unit
        val checkProp :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
        val checkSpec :
          MusynthTypes.symTabScope list ref -> MusynthTypes.musSpecT -> unit
        val checkProg :
          MusynthTypes.symTabScope list ref ->
          (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val checkLLProg :
          MusynthTypes.LLDesigSet.elt list * MusynthTypes.llAutomatonT list *
          'a * 'b -> unit
      end
    module AST :
      sig
        val pLoc : Format.formatter -> int * int * int * int -> unit
        val pLocOpt :
          Format.formatter -> (int * int * int * int) option -> unit
        val pIdentifier : Format.formatter -> string * 'a -> unit
        val identToName : 'a * 'b -> 'a
        val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
        val pList :
          string ->
          bool ->
          bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> 'a list -> unit
        val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
        val pSymTypeDecl :
          Format.formatter ->
          (string * 'a) * MusynthTypes.musSymTypeT -> unit
        val musMakeIndentedBox :
          Format.formatter ->
          (Format.formatter -> 'a -> unit) ->
          'a ->
          (Format.formatter -> 'b -> unit) ->
          'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
        val pSymTypeDeclBlock :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
        val pDesignator :
          Format.formatter -> MusynthTypes.musDesignatorT -> unit
        val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
        val pPropOpt :
          Format.formatter -> MusynthTypes.musPropT option -> unit
        val pDecl :
          (Format.formatter -> 'a -> 'b) ->
          (Format.formatter -> 'a -> 'c) ->
          Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
        val noopPrinter : 'a -> 'b -> unit
        val pMsgDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val pMsgDeclBlock :
          string ->
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pMessagesDeclBlock :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pStateAnnot :
          Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
        val pStateDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
          MusynthTypes.musStateAnnotationT -> unit
        val pStateDeclBlock :
          string ->
          Format.formatter ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> unit
        val pTransDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType -> unit
        val pTransDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType list -> unit
        val pInitStateConstraint :
          Format.formatter -> MusynthTypes.musPropT -> unit
        val pInitStateDeclBlock :
          Format.formatter -> MusynthTypes.musPropT list -> unit
        val pChanProp :
          Format.formatter ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
          unit
        val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
        val pLossFairness :
          Format.formatter -> MusynthTypes.musLossFairnessT -> unit
        val pDupFairness :
          Format.formatter -> MusynthTypes.musDupFairnessT -> unit
        val pAutomatonDecl :
          Format.formatter ->
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
        val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
        val pProg :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val pLLDesignator :
          Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLVar :
          Format.formatter ->
          MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
        val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
        val pLLFairness :
          Format.formatter -> MusynthTypes.llFairnessT -> unit
        val pLLDupFairness :
          Format.formatter -> MusynthTypes.llDupFairnessT -> unit
        val pLLLossFairness :
          Format.formatter -> MusynthTypes.llLossFairnessT -> unit
        val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
        val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
        val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
        val pLLAutomaton :
          Format.formatter -> MusynthTypes.llAutomatonT -> unit
        val pLLProg :
          Format.formatter ->
          MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
          MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
      end
    module Utils :
      sig
        module AST :
          sig
            val pLoc : Format.formatter -> int * int * int * int -> unit
            val pLocOpt :
              Format.formatter -> (int * int * int * int) option -> unit
            val pIdentifier : Format.formatter -> string * 'a -> unit
            val identToName : 'a * 'b -> 'a
            val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
            val pList :
              string ->
              bool ->
              bool ->
              (Format.formatter -> 'a -> unit) ->
              Format.formatter -> 'a list -> unit
            val pSymType :
              Format.formatter -> MusynthTypes.musSymTypeT -> unit
            val pSymTypeDecl :
              Format.formatter ->
              (string * 'a) * MusynthTypes.musSymTypeT -> unit
            val musMakeIndentedBox :
              Format.formatter ->
              (Format.formatter -> 'a -> unit) ->
              'a ->
              (Format.formatter -> 'b -> unit) ->
              'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
            val pSymTypeDeclBlock :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
            val pDesignator :
              Format.formatter -> MusynthTypes.musDesignatorT -> unit
            val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
            val pPropOpt :
              Format.formatter -> MusynthTypes.musPropT option -> unit
            val pDecl :
              (Format.formatter -> 'a -> 'b) ->
              (Format.formatter -> 'a -> 'c) ->
              Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
            val noopPrinter : 'a -> 'b -> unit
            val pMsgDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
            val pMsgDeclBlock :
              string ->
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pMessagesDeclBlock :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
              unit
            val pStateAnnot :
              Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
            val pStateDecl :
              Format.formatter ->
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
              MusynthTypes.musStateAnnotationT -> unit
            val pStateDeclBlock :
              string ->
              Format.formatter ->
              (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
               MusynthTypes.musStateAnnotationT)
              list -> unit
            val pTransDecl :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType -> unit
            val pTransDeclBlock :
              Format.formatter ->
              (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
               MusynthTypes.musDesignatorT)
              MusynthTypes.musDeclType list -> unit
            val pInitStateConstraint :
              Format.formatter -> MusynthTypes.musPropT -> unit
            val pInitStateDeclBlock :
              Format.formatter -> MusynthTypes.musPropT list -> unit
            val pChanProp :
              Format.formatter ->
              MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
              MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
              int -> unit
            val pFairness :
              Format.formatter -> MusynthTypes.musFairnessT -> unit
            val pLossFairness :
              Format.formatter -> MusynthTypes.musLossFairnessT -> unit
            val pAutomatonDecl :
              Format.formatter ->
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType ->
              unit
            val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
            val pProg :
              Format.formatter ->
              ((string * 'a) * MusynthTypes.musSymTypeT) list *
              MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
              MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
              MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
              unit
            val pLLDesignator :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLIdent :
              Format.formatter -> MusynthTypes.llDesignatorT -> unit
            val pLLVar :
              Format.formatter ->
              MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
            val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
            val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
            val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
            val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
            val pLLAutomaton :
              Format.formatter -> MusynthTypes.llAutomatonT -> unit
            val pLLProg :
              Format.formatter ->
              MusynthTypes.llDesignatorT list *
              MusynthTypes.llAutomatonT list * MusynthTypes.llPropT *
              MusynthTypes.llSpecT list -> unit
          end
        module MSSet :
          sig
            type elt = int MusynthTypes.LLDesigMap.t
            type t = MusynthUtils.MSSet.t
            val empty : t
            val is_empty : t -> bool
            val mem : elt -> t -> bool
            val add : elt -> t -> t
            val singleton : elt -> t
            val remove : elt -> t -> t
            val union : t -> t -> t
            val inter : t -> t -> t
            val diff : t -> t -> t
            val compare : t -> t -> int
            val equal : t -> t -> bool
            val subset : t -> t -> bool
            val iter : (elt -> unit) -> t -> unit
            val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
            val for_all : (elt -> bool) -> t -> bool
            val exists : (elt -> bool) -> t -> bool
            val filter : (elt -> bool) -> t -> t
            val partition : (elt -> bool) -> t -> t * t
            val cardinal : t -> int
            val elements : t -> elt list
            val min_elt : t -> elt
            val max_elt : t -> elt
            val choose : t -> elt
            val split : elt -> t -> t * bool * t
          end
        val splatList : 'a -> int -> 'a list
        val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
        val msToList :
          int MusynthTypes.LLDesigMap.t -> MusynthTypes.LLDesigMap.key list
        val msToStr :
          (MusynthTypes.LLDesigMap.key -> string) ->
          int MusynthTypes.LLDesigMap.t -> string
        val addToMs :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val delFromMS :
          MusynthTypes.LLDesigMap.key ->
          int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
        val makeEmptyMSWithAlphabet :
          MusynthTypes.LLDesigMap.key list -> int MusynthTypes.LLDesigMap.t
        val enumerateMS :
          MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
        val enumerateLists : 'a list -> int -> 'a list list
        val listToStr : ('a -> string) -> 'a list -> string
        val nextuid : int ref
        val getuid : unit -> int
        val resetuid : unit -> unit
        val crossProduct : 'a list list -> 'a list list
        val identConstPairList2Map :
          (MusynthTypes.IdentMap.key * 'a) list -> 'a MusynthTypes.IdentMap.t
        val sDesigToIdent :
          MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
        val conjoinPropOpts :
          MusynthTypes.musPropT option ->
          MusynthTypes.musPropT option -> MusynthTypes.musPropT option
        val mergeIdentMaps :
          'a MusynthTypes.IdentMap.t ->
          'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
        val evalProp :
          MusynthTypes.musPropT -> ('a * 'b) MusynthTypes.IdentMap.t -> bool
        val getMapsForProp :
          MusynthTypes.IdentMap.key list ->
          MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
          MusynthTypes.musPropT option ->
          MusynthTypes.identifierT MusynthTypes.IdentMap.t list
        val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
        val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesig : unit -> MusynthTypes.llDesignatorT
        val makeLCMesgDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeLCProcDesigPrime : unit -> MusynthTypes.llDesignatorT
        val makeDeadlockDesig : unit -> MusynthTypes.llDesignatorT
        val makeDeferDesig : unit -> MusynthTypes.llDesignatorT
        val getMsgsForAut :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT list * MusynthTypes.llIdentT list
        val getNameForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT
        val getTransitionsForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
        val getStatesForAut :
          MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list
        val getAutomatonByName :
          MusynthTypes.llAutomatonT list ->
          MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT
        val getFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT
        val getLFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llLossFairnessT
        val getDFairnessForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDupFairnessT
        val getSender :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT
        val getReceivers :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT list
        val getStateNameForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getStateNamePForAutomaton :
          MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
        val getCSPredsForMsg :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT -> MusynthTypes.llPropT
        val getCSPredsForMsgAll :
          MusynthTypes.llIdentT ->
          MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT
        val getMsgsToSyncOnFromState :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val getStatesFromWhichMsgSync :
          MusynthTypes.llAutomatonT ->
          MusynthTypes.llIdentT -> MusynthTypes.LLDesigSet.elt list
        val canonicalizeProp : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val canonicalizePropFP : MusynthTypes.llPropT -> MusynthTypes.llPropT
        val makeFormatterOfName : string -> out_channel * Format.formatter
        val makeConjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
        val makeDisjunction :
          MusynthTypes.llPropT list -> MusynthTypes.llPropT
      end
    module Opts :
      sig
        val debugDisabled : bool ref
        val debugOptions : MusynthTypes.StringSet.t ref
        val debugFileName : string ref
        val onlySafety : bool ref
        val conjunctivePart : bool ref
        val inputFileName : string ref
        val numSolsRequested : int ref
        val reorderEnabled : bool ref
        val reorderMethod : Cudd.Man.reorder ref
        val tracePrintMode : string ref
        val jumpStep : int ref
        val reorderMethods : string list
      end
    module Debug :
      sig
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val reorderMethods : string list
          end
        val debugOC : out_channel option ref
        val debugFmt : Format.formatter option ref
        val debugEnabled : unit -> bool
        val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
        val getDebugFmt : unit -> Format.formatter
        val initDebugSubsys : string -> unit
        val shutDownDebugSubsys : unit -> unit
        val dprintf :
          MusynthTypes.StringSet.elt ->
          ('a, Format.formatter, unit) format -> 'a
        val dflush : unit -> unit
      end
    class bddManager :
      object
        val mutable bitNameToBddMap :
          Cudd.Man.d Cudd.Bdd.t MusynthTypes.StringMap.t
        val mutable cachedAllButParamCube : Cudd.Man.d Cudd.Bdd.t option
        val mutable cachedAllVarCube : Cudd.Man.d Cudd.Bdd.t option
        val mutable cachedAllVarPrinter :
          (Format.formatter -> Cudd.Man.tbool array -> unit) option
        val mutable cachedConstraintsOnAllVars : Cudd.Man.d Cudd.Bdd.t option
        val mutable cachedConstraintsOnParams : Cudd.Man.d Cudd.Bdd.t option
        val mutable cachedCubePrinter :
          (Format.formatter -> Cudd.Man.tbool array -> unit) option
        val mutable cachedP2USubstTable : Cudd.Man.d Cudd.Bdd.t array option
        val mutable cachedParamVarCube : Cudd.Man.d Cudd.Bdd.t option
        val mutable cachedParamVarPrinter :
          (Format.formatter -> Cudd.Man.tbool array -> unit) option
        val mutable cachedPrimedVarCube : Cudd.Man.d Cudd.Bdd.t option
        val mutable cachedStateVarPrinter :
          (Format.formatter -> Cudd.Man.tbool array -> unit) option
        val mutable cachedU2PSubstTable : Cudd.Man.d Cudd.Bdd.t array option
        val mutable cachedUnprimedVarCube : Cudd.Man.d Cudd.Bdd.t option
        val mutable cachedVarCubes :
          Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigSetMap.t
        val mutable indexToBitNameMap : string MusynthTypes.IntMap.t
        val mutable internalStateVars : MusynthTypes.LLDesigSet.t
        val mutable manager : Cudd.Man.d Cudd.Man.t
        val mutable numInternalStateBits : int
        val mutable numParamBits : int
        val mutable numStateBits : int
        val mutable numTotalBits : MusynthTypes.IntSet.elt
        val mutable pStateBitSet : MusynthTypes.IntSet.t
        val mutable paramBitSet : MusynthTypes.IntSet.t
        val mutable paramVars : MusynthTypes.LLDesigSet.t
        val mutable stateBitSet : MusynthTypes.IntSet.t
        val mutable stateVars :
          MusynthTypes.LLDesigMap.key MusynthTypes.LLDesigMap.t
        val mutable varMap :
          (MusynthTypes.LLDesigSet.elt list * MusynthTypes.IntSet.elt * 
           int * MusynthTypes.StringMap.key list *
           Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
           MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
           (Cudd.Man.tbool array -> MusynthTypes.LLDesigSet.elt) *
           Cudd.Man.d Cudd.Bdd.t)
          MusynthTypes.LLDesigMap.t
        method private checkVarReregister :
          MusynthTypes.LLDesigMap.key ->
          MusynthTypes.LLDesigSet.elt list ->
          MusynthTypes.LLDesigSet.elt list option
        method cubeOfMinTerm : Cudd.Man.tbool array -> Cudd.Man.d Cudd.Bdd.t
        method private determinizeOnSet :
          MusynthTypes.IntSet.t ->
          Cudd.Man.tbool array -> Cudd.Man.tbool array
        method getAllButParamCube : unit -> Cudd.Man.d Cudd.Bdd.t
        method getAllVarPrinter :
          unit -> Format.formatter -> Cudd.Man.tbool array -> unit
        method getBitPrinter :
          unit -> Format.formatter -> MusynthTypes.IntMap.key -> unit
        method getConstraintsOnAllVars : unit -> Cudd.Man.d Cudd.Bdd.t
        method getConstraintsOnParams : unit -> Cudd.Man.d Cudd.Bdd.t
        method getCubeForAllVars : unit -> Cudd.Man.d Cudd.Bdd.t
        method private getCubeForOneVar :
          MusynthTypes.LLDesigMap.key -> Cudd.Man.d Cudd.Bdd.t
        method getCubeForParamVars : unit -> Cudd.Man.d Cudd.Bdd.t
        method getCubeForPrimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
        method getCubeForUnprimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
        method getCubeForVar :
          MusynthTypes.LLDesigSet.elt -> Cudd.Man.d Cudd.Bdd.t
        method getCubeForVars :
          MusynthTypes.LLDesigSet.elt list -> Cudd.Man.d Cudd.Bdd.t
        method getCubePrinter :
          unit -> Format.formatter -> Cudd.Man.tbool array -> unit
        method getNParamVars :
          int ->
          Cudd.Man.d Cudd.Bdd.t ->
          MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list
        method getNStateVars :
          int ->
          Cudd.Man.d Cudd.Bdd.t ->
          MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list
        method getNumInternalStateBits : unit -> int
        method getNumMinTerms : Cudd.Man.d Cudd.Bdd.t -> float
        method getNumMinTermsParam : Cudd.Man.d Cudd.Bdd.t -> float
        method getNumMinTermsState : Cudd.Man.d Cudd.Bdd.t -> float
        method getNumMinTermsStateNI : Cudd.Man.d Cudd.Bdd.t -> float
        method getNumParamBits : unit -> int
        method getNumParamVars : unit -> int
        method getNumStateBits : unit -> int
        method getNumTotalBits : unit -> MusynthTypes.IntSet.elt
        method getParamVarNames : unit -> MusynthTypes.LLDesigSet.elt list
        method getParamVarPrinter :
          unit -> Format.formatter -> Cudd.Man.tbool array -> unit
        method getParamVars :
          Cudd.Man.d Cudd.Bdd.t ->
          MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t
        method getPeakBDDSize : unit -> int
        method getStateVarNames : unit -> MusynthTypes.LLDesigMap.key list
        method getStateVarNamesNI : unit -> MusynthTypes.LLDesigMap.key list
        method getStateVarPrinter :
          unit -> Format.formatter -> Cudd.Man.tbool array -> unit
        method getStateVars :
          Cudd.Man.d Cudd.Bdd.t ->
          MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t
        method getSubstTableP2U : unit -> Cudd.Man.d Cudd.Bdd.t array
        method getSubstTableU2P : unit -> Cudd.Man.d Cudd.Bdd.t array
        method private invalidateCaches : unit -> unit
        method isFalse : Cudd.Man.d Cudd.Bdd.t -> bool
        method private lg : int -> int
        method lookupVar :
          MusynthTypes.LLDesigMap.key ->
          (MusynthTypes.LLDesigSet.elt list * MusynthTypes.IntSet.elt * 
           int * MusynthTypes.StringMap.key list *
           Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
           MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
           (Cudd.Man.tbool array -> MusynthTypes.LLDesigSet.elt) *
           Cudd.Man.d Cudd.Bdd.t)
          option
        method private makeBDDForRepr :
          MusynthTypes.IntSet.elt ->
          int -> MusynthTypes.IntMap.key -> Cudd.Man.d Cudd.Bdd.t
        method makeFalse : unit -> Cudd.Man.d Cudd.Bdd.t
        method makeTrue : unit -> Cudd.Man.d Cudd.Bdd.t
        method minimize : unit -> unit
        method pickMinTermOnPStates :
          Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
        method pickMinTermOnParams :
          Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
        method pickMinTermOnStates :
          Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
        method printCubes :
          int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
        method printParamVars :
          int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
        method printStateVars :
          int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
        method prop2BDD : MusynthTypes.llPropT -> Cudd.Man.d Cudd.Bdd.t
        method private registerBits :
          MusynthTypes.LLDesigMap.key ->
          int -> MusynthTypes.IntSet.elt * MusynthTypes.StringMap.key list
        method private registerBitsForVar :
          MusynthTypes.IntSet.elt -> int -> MusynthTypes.IntSet.t
        method registerInternalStateVariable :
          MusynthTypes.LLDesigMap.key ->
          MusynthTypes.LLDesigSet.elt list -> unit
        method registerParamVariable :
          MusynthTypes.LLDesigMap.key ->
          MusynthTypes.LLDesigSet.elt list -> unit
        method registerStateVariable :
          MusynthTypes.LLDesigMap.key ->
          MusynthTypes.LLDesigSet.elt list -> unit
        method registerVar :
          MusynthTypes.LLDesigMap.key ->
          MusynthTypes.LLDesigSet.elt list -> unit
        method reset : unit -> unit
        method private substOneVarInTable :
          Cudd.Man.d Cudd.Bdd.t array ->
          MusynthTypes.LLDesigMap.key -> MusynthTypes.LLDesigMap.key -> unit
      end
  end
module Debug :
  sig
    module Opts :
      sig
        val debugDisabled : bool ref
        val debugOptions : MusynthTypes.StringSet.t ref
        val debugFileName : string ref
        val onlySafety : bool ref
        val conjunctivePart : bool ref
        val inputFileName : string ref
        val numSolsRequested : int ref
        val reorderEnabled : bool ref
        val reorderMethod : Cudd.Man.reorder ref
        val reorderMethods : string list
      end
    val debugOC : out_channel option ref
    val debugFmt : Format.formatter option ref
    val debugEnabled : unit -> bool
    val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
    val getDebugFmt : unit -> Format.formatter
    val initDebugSubsys : string -> unit
    val shutDownDebugSubsys : unit -> unit
    val dprintf :
      MusynthTypes.StringSet.elt -> ('a, Format.formatter, unit) format -> 'a
    val dflush : unit -> unit
  end
module Trace :
  sig
    module Opts :
      sig
        val debugDisabled : bool ref
        val debugOptions : MusynthTypes.StringSet.t ref
        val debugFileName : string ref
        val onlySafety : bool ref
        val conjunctivePart : bool ref
        val inputFileName : string ref
        val numSolsRequested : int ref
        val reorderEnabled : bool ref
        val reorderMethod : Cudd.Man.reorder ref
        val tracePrintMode : string ref
        val reorderMethods : string list
      end
    module AST :
      sig
        val pLoc : Format.formatter -> int * int * int * int -> unit
        val pLocOpt :
          Format.formatter -> (int * int * int * int) option -> unit
        val pIdentifier : Format.formatter -> string * 'a -> unit
        val identToName : 'a * 'b -> 'a
        val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
        val pList :
          string ->
          bool ->
          bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> 'a list -> unit
        val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
        val pSymTypeDecl :
          Format.formatter ->
          (string * 'a) * MusynthTypes.musSymTypeT -> unit
        val musMakeIndentedBox :
          Format.formatter ->
          (Format.formatter -> 'a -> unit) ->
          'a ->
          (Format.formatter -> 'b -> unit) ->
          'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
        val pSymTypeDeclBlock :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
        val pDesignator :
          Format.formatter -> MusynthTypes.musDesignatorT -> unit
        val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
        val pPropOpt :
          Format.formatter -> MusynthTypes.musPropT option -> unit
        val pDecl :
          (Format.formatter -> 'a -> 'b) ->
          (Format.formatter -> 'a -> 'c) ->
          Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
        val noopPrinter : 'a -> 'b -> unit
        val pMsgDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val pMsgDeclBlock :
          string ->
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pMessagesDeclBlock :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pStateAnnot :
          Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
        val pStateDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
          MusynthTypes.musStateAnnotationT -> unit
        val pStateDeclBlock :
          string ->
          Format.formatter ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> unit
        val pTransDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType -> unit
        val pTransDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType list -> unit
        val pInitStateConstraint :
          Format.formatter -> MusynthTypes.musPropT -> unit
        val pInitStateDeclBlock :
          Format.formatter -> MusynthTypes.musPropT list -> unit
        val pChanProp :
          Format.formatter ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
          unit
        val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
        val pLossFairness :
          Format.formatter -> MusynthTypes.musLossFairnessT -> unit
        val pAutomatonDecl :
          Format.formatter ->
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
        val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
        val pProg :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> 
          unit
        val pLLDesignator :
          Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLVar :
          Format.formatter ->
          MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
        val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
        val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
        val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
        val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
        val pLLAutomaton :
          Format.formatter -> MusynthTypes.llAutomatonT -> unit
        val pLLProg :
          Format.formatter ->
          MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
          MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
      end
    module Debug :
      sig
        module Opts :
          sig
            val debugDisabled : bool ref
            val debugOptions : MusynthTypes.StringSet.t ref
            val debugFileName : string ref
            val onlySafety : bool ref
            val conjunctivePart : bool ref
            val inputFileName : string ref
            val numSolsRequested : int ref
            val reorderEnabled : bool ref
            val reorderMethod : Cudd.Man.reorder ref
            val reorderMethods : string list
          end
        val debugOC : out_channel option ref
        val debugFmt : Format.formatter option ref
        val debugEnabled : unit -> bool
        val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
        val getDebugFmt : unit -> Format.formatter
        val initDebugSubsys : string -> unit
        val shutDownDebugSubsys : unit -> unit
        val dprintf :
          MusynthTypes.StringSet.elt ->
          ('a, Format.formatter, unit) format -> 'a
        val dflush : unit -> unit
      end
    val printState :
      Format.formatter ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t -> unit
    val printTraceFullSafety :
      Format.formatter ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
    val computeDiff :
      'a MusynthTypes.LLDesigMap.t ->
      'a MusynthTypes.LLDesigMap.t -> 'a MusynthTypes.LLDesigMap.t
    val printTraceDiffSafety :
      Format.formatter ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
    val printTraceDiffLiveness :
      Format.formatter ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
    val printTraceFullLiveness :
      Format.formatter ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
    val printTraceSafety :
      Format.formatter ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
    val printTraceLiveness :
      Format.formatter ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list ->
      MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list -> unit
  end
val musynthProcess : string option -> unit
