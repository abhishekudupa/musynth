val buildChannelAutomaton : MusynthTypes.LLDesigMap.key list ->
                            MusynthTypes.llIdentT list ->
                            (MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
                               MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * 
                                 int) ->
                            (MusynthTypes.llDesignatorT list * 
                               MusynthTypes.llTransT list)
