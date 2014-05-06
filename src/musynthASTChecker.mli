val resolveSymType : MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
                     MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT

val checkSymTypeDecl : MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
                       MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT -> unit

val destructDesigDecl : MusynthTypes.musDesignatorT ->
                        MusynthTypes.identifierT * MusynthTypes.identifierT list

val getObligationsForIdent : MusynthTypes.symtabEntry 
                               MusynthTypes.IdentMap.t ref list ref ->
                             MusynthTypes.IdentMap.key ->
                             (string * MusynthTypes.musSymTypeT) list * 
                               MusynthTypes.musPropT option

val getTypeObligationsForIdent : MusynthTypes.symtabEntry 
                                   MusynthTypes.IdentMap.t ref list ref ->
                                 MusynthTypes.IdentMap.key -> 
                                 MusynthTypes.musSymTypeT list

val checkTypeLists : MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
                     string * MusynthTypes.sourcelocation option ->
                     MusynthTypes.musSymTypeT list -> 
                     MusynthTypes.IdentMap.key list -> unit

val getDesigType : MusynthTypes.symTabScope list ref ->
                   MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry

val checkTypeCompatibility : MusynthTypes.symTabScope list ref ->
                             MusynthTypes.musDesignatorT ->
                             MusynthTypes.musDesignatorT ->
                             MusynthTypes.sourcelocation option ->
                             MusynthTypes.symtabEntry * MusynthTypes.symtabEntry

val checkPureProp : MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
val checkPureQProp : MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
val convertDesigToPrimed :  MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
val convertDesigDeclToPrimed :  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
                                MusynthTypes.musDesignatorT MusynthTypes.musDeclType

val checkProg : MusynthTypes.symTabScope list ref ->
                (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list *
                  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
                    MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
                      MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> unit

val checkLLProg : MusynthTypes.LLDesigSet.elt list * 
                    MusynthTypes.llAutomatonT list * 'a *
                      'b * 'c -> unit

