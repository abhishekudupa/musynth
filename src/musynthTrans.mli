val constructEnabledProp : MusynthTypes.llAutomatonT list ->
                           MusynthTypes.llAutomatonT -> 
                           MusynthTypes.llPropT
val constructDLProps : MusynthTypes.llIdentT list ->
                       MusynthTypes.llAutomatonT list -> 
                       MusynthTypes.llPropT
val makeTransPropOnMsgForAut : MusynthTypes.llIdentT -> 
                               MusynthTypes.llAutomatonT -> 
                               MusynthTypes.llPropT
val makeTransPropForMsg : MusynthTypes.llIdentT ->
                          MusynthTypes.llAutomatonT list -> 
                          MusynthTypes.llPropT
val makeTransMap : MusynthTypes.llIdentT list ->
                    MusynthTypes.llAutomatonT list ->
                    MusynthTypes.llPropT MusynthTypes.LLDesigMap.t

val makeRestrictedTransProp : MusynthTypes.llPropT MusynthTypes.LLDesigMap.t ->
                              MusynthTypes.llIdentT list ->
                              MusynthTypes.llPropT list
