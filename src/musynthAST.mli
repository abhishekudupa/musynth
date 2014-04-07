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
  Format.formatter -> ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
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
  Format.formatter ->
  MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT -> unit
val pInitStateDecl :
  Format.formatter ->
  (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
  MusynthTypes.musDeclType -> unit
val pInitStateDeclBlock :
  Format.formatter ->
  (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
  MusynthTypes.musDeclType list -> unit
val pChanProp :
  Format.formatter ->
  MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
  MusynthTypes.musChanDupT * int -> unit
val pAutomatonDecl :
  Format.formatter ->
  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
val pProg :
  Format.formatter ->
  ((string * 'a) * MusynthTypes.musSymTypeT) list *
  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
  (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
  MusynthTypes.musDeclType list * MusynthTypes.musSpecT list -> unit
