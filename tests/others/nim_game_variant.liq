[%%version 0.15]

(* --------------------------------------------------------------------------*)
(* game's data-structures *)
(* --------------------------------------------------------------------------*) 
type params = {
  bet : tez;
  gain : tez;
  max_players : int;
}
            
type player_info = {
  deposit: tez;
  round : nat;
}

type registration = {
  nb_players : int;
  reg_players : (key_hash, player_info) map;
}

type game = {
  remaining : tez;
  blacklist : key_hash set;
  history : (key_hash, player_info) map;
  curr_round : nat;
}
  
type state =
  | Init of params * registration
  | Play of params * game
  | End of  params * game 
  | Fail of  params * game 
              
type action = Join | Take of int
                        
(* --------------------------------------------------------------------------*)
(* initialize the parameters of a game *)
(* --------------------------------------------------------------------------*)
let%init storage (max_p : int) (bet : tez) (gain : tez) =
  if max_p <= 2 then
    Current.failwith "Not enough players (at least 3 are required) !";
  if bet < 17tz then
    Current.failwith "Not enough bet !";
  let params = { bet = bet ; max_players = max_p ; gain = gain } in
  let init = {
    nb_players = 0 ;
    reg_players = (Map : (key_hash, player_info) map) }
  in
  Init (params, init)

let register_player (storage:state) (caller:key_hash) =
  match storage with
  | Play _ -> Current.failwith "The game is in 'play phase' !"
  | End _  -> Current.failwith "The game is in 'end phase' !"
  | Fail _ -> Current.failwith "The game is in 'fail phase' !"
  | Init i ->
      let params = i.(0) in
      let init = i.(1) in
      if init.nb_players >= params.max_players then
       (* invariant: should not happen *)
        Current.failwith "Assert false (1)";
     
      if Map.mem caller init.reg_players then
        Current.failwith "Player already registered !";
      let init = init.nb_players <- init.nb_players + 1 in
      let info = {deposit = params.bet ; round = 0p } in
      let init = init.reg_players <- Map.add caller info init.reg_players in
      if init.nb_players = params.max_players then
        let game = {
          remaining = params.bet;
          blacklist = (Set : key_hash set);
          history = init.reg_players;
          curr_round = 1p
        }
        in
        Play (params,game)
      else
        Init (params, init) 
     
(* returns the address of the current caller *)
let get_caller (_ : unit) =
  let c = (Source : (unit, unit) contract) in
  Contract.manager c
  
let punish (game:game) (caller : key_hash) (punishment : tez) =
  match Map.find caller game.history with
  | None -> 
      Current.failwith "Assert false (2)";
  | Some info ->
      let deposit = info.deposit - punishment in (*overflow well handled for tez ?*)
      if deposit > 0tz then
        game.history <- Map.add caller (info.deposit <- deposit) game.history
      else
        let game = game.blacklist <- Set.add caller game.blacklist in
        game.history <- Map.remove caller game.history
       
let tez_of_move (move : int) =
  if move = 1 then 1tz
  else if move = 2 then 2tz
  else if move = 3 then 3tz
  else 4tz (* bad move -> punishment *)

let play_one_round (storage:state) (caller:key_hash) (move:int) =
  match storage with
  | Init _ -> Current.failwith "The game is in 'init phase' !"
  | End _  -> Current.failwith "The game is in 'end phase' !"
  | Fail _ -> Current.failwith "The game is in 'fail phase' !"
  | Play p ->
      let params = p.(0) in
      let game = p.(1) in
      if Set.mem caller game.blacklist || not (Map.mem caller game.history) then
        storage
          
      else
        let move = tez_of_move move in (* move in {1, 2, 3, 4 tz} *)
        if move = 3tz then 
          let game = punish game caller move in
          Play (params, game)
        else
          match Map.find caller game.history with
          | None -> Current.failwith "Assert false (3)";
          | Some info ->
              if info.round = game.curr_round then
                Play (params, punish game caller 4tz)
              else begin
                if game.remaining < move then
                  Current.failwith "Remaining is smaller than given amount !";
                let game = game.remaining <- game.remaining - move in
                let game = game.curr_round <- game.curr_round + 1p in
                let info = info.round <- game.curr_round in
                let game = game.history <- Map.add caller info game.history in
                Play (params, game)
              end
         
let%entry main
    (parameter : action)
    (storage : state)
  : unit * state =
  let caller = get_caller () in
  match parameter with
  | Join ->
      let storage = register_player storage caller in
      (), storage
  | Take move -> 
      let storage = play_one_round storage caller move in
      match storage with
      | Init _ -> Current.failwith "Assert false (4)"
      | End _  -> Current.failwith "Assert false (5)"
      | Fail _ -> Current.failwith  "Assert false (6)" 
      | Play p ->
          let params = p.(0) in
          let game = p.(1) in
          let sz = Map.size game.history in 
          if sz = 0p then
            (), Fail (params, game)
          else
          if sz = 1p || game.remaining = 0tz then
            (), End (params, game)
          else
            (), storage
