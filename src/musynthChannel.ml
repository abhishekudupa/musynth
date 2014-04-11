open MusynthTypes
module AST = MusynthAST
module Utils = MusynthUtils

(* utilities for lowering and creating channel automata *)

let getSimpleDesigForMS ms =
  let str = Utils.msToStr (AST.astToString AST.pDesignator) ms in
  SimpleDesignator (str, None)

let getSimpleDesigForList lst = 
  let str = Utils.listToStr (AST.astToString AST.pDesignator) lst in
  SimpleDesignator (str, None)

let addToList elem lst =
  elem :: lst

let addToMS elem ms =
  Utils.addToMs elem ms

let delFromList elem lst =
  List.tl lst

let delFromMS elem ms =
  Utils.delFromMS elem ms

let listContains elem lst =
  (List.hd lst) = elem

let msContains elem ms =
  (Utils.DesigMap.find elem ms) <> 0

let listLen lst = 
  (List.length lst)

let msLen ms =
  List.length (Utils.msToList ms)

let makeChanTran addFun delFun contFun lenFun desigFun chanprop states linmsgs loutmsgs cclist =
  let intooutmap = 
    List.fold_left2
      (fun acc inmsg outmsg ->
       Utils.DesigMap.add inmsg outmsg acc)
      Utils.DesigMap.empty linmsgs loutmsgs
  in
  let _, l, d, size = chanprop in
  let lossy = match l with | ChanLossy _ -> true | _ -> false in
  let duplicating = match d with | ChanDuplicating _ -> true | _ -> false in
  (* base input transitions *)
  let transitions1 =
    List.concat 
      (List.fold_left
         (fun acc1 cc ->
          if ((lenFun cc) <> size) then
            (List.fold_left 
               (fun acc2 input ->
                (TComplete (desigFun cc, input, desigFun (addFun input cc))) :: acc2) 
               [] linmsgs) :: acc1
          else
            acc1) [] cclist)
  in
  (* lossy input transitions *)
  let transitions2 = 
    List.concat 
      (List.fold_left
         (fun acc1 cc ->
          (List.fold_left
             (fun acc2 input ->
              (TComplete (desigFun cc, input, desigFun cc)) :: acc2) [] linmsgs) :: acc1)
         [] cclist)
  in
  (* base output transitions *)
  let transitions3 = 
    List.concat
      (List.fold_left
         (fun acc1 cc ->
          (List.fold_left
             (fun acc2 input ->
              if (contFun input cc) then
                (TComplete (desigFun cc, Utils.DesigMap.find input intooutmap, 
                            desigFun (delFun input cc))) :: acc2
              else
                acc2) [] linmsgs) :: acc1) [] cclist)
  in
  (* duplicating output transitions *)
  let transitions4 =
    List.concat 
      (List.fold_left
         (fun acc1 cc ->
          if ((lenFun cc)  <> 0) then
            (List.fold_left
               (fun acc2 input ->
                if (contFun input cc) then
                  (TComplete (desigFun cc, Utils.DesigMap.find input intooutmap,
                              desigFun cc)) :: acc2
                else
                  acc2) [] linmsgs) :: acc1
          else
            acc1) [] cclist)
  in
  transitions1 @ transitions2 @ transitions3 @ transitions4
  
  
let buildChannelAutomaton linmsgs loutmsgs chanprops =
  let o, _, _, size = chanprops in
  begin
    match o with
    | ChanOrdered _ ->
       let lists = Utils.enumerateLists linmsgs size in
       let states = List.map getSimpleDesigForList lists in
       (states, makeChanTran addToList delFromList listContains listLen getSimpleDesigForList
                             chanprops states linmsgs loutmsgs lists)
    | ChanUnordered _ ->
       let ms = Utils.enumerateMS linmsgs size in
       let states = List.map getSimpleDesigForMS ms in
       (states, makeChanTran addToMS delFromMS msContains msLen getSimpleDesigForMS
                             chanprops states linmsgs loutmsgs ms)
  end
