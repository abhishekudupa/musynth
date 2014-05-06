val lowerProg : MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
                'a * MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
                  MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
                    MusynthTypes.musPropT list * MusynthTypes.musSpecT list ->
                MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
                  MusynthTypes.llPropT * MusynthTypes.llSpecT list * 
                    MusynthTypes.llPropT
