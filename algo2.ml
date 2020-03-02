(* Algorithme de parcours en profondeur du graphe et son inverse *)
let graphe1 = [(1,[6;7;8]) ; (2,[1;4]) ; (3, [2]) ; (4, [3;5]) ; (5, [1]) ; (6, [5;7]) ; (7, []) ; (8, [6;7])];;

(* 2. Retourner la liste des successeurs d’un sommet donné *)
let successeurs sommet graphe = snd (List.find (fun (s, succ) -> s = sommet) graphe);;
successeurs 4 graphe1;;

(* init(graph) = set initial values of index and lowlink *)
let init_graph graph = List.map (fun (s, succ) -> s, -1, -1) graph;;

(* change_index(smts, sommet, index) = self descriptive *)
let change_index sommets sommet index = List.map (fun (s, i, l) -> if sommet = s then (s, index, l) else (s, i, l)) sommets;;

(* change_lowlink(smts, sommet, lowlink) = self descriptive *)
let change_lowlink sommets sommet lowlink = List.map (fun (s, i, l) -> if sommet = s then (s, i, lowlink) else (s, i, l)) sommets;;

(* get_index(smts, sommet) *)
let get_index sommets sommet = let (s, i, l) = List.find (fun (s, i, l) -> s = sommet) sommets in i;;

(* get_lowlink(smts, sommet) *)
let get_lowlink sommets sommet = let (s, i, l) = List.find (fun (s, i, l) -> s = sommet) sommets in l;;

(* min(a, b) *)
let min a b = if a > b then b else a;;

let depiler_jusqua stk v =
    let rec dep stk rst = match rst with
          []     -> (stk, [])
        | x :: r -> if x = v then (x :: stk, r)
                    else dep (x::stk) r
    in dep [] stk;;


let tarjan graph =
    let rec strongconnect smts stk index v =
        let smts  = change_index smts v index in
        let index = index + 1 in
        let stk   = v::stk in
        let (scc, smts, stk, index, lowlink) = List.fold_left
            (fun a e ->
                let (scc, smts, stk, index, lowlink) = a in
                let e_index = get_index smts e in
                if (e_index = -1) then
                    let (nscc, smts, stk, index) = strongconnect smts stk index e
                    in (nscc @ scc, smts, stk, index, (min lowlink (get_lowlink smts e) ))
                else (scc, smts, stk, index, (min lowlink e_index))
            )

            ([], smts, stk, index, index - 1)
            (successeurs v graph)

        in let smts = change_lowlink smts v lowlink in

        if get_index smts v = lowlink then
            let (dep, new_stk) = depiler_jusqua stk v in
             (dep::scc, smts, new_stk, index)
        else (scc, smts, stk, index) in

    let index = 0 in
    let stk = [] in
    let smts = init_graph graph in

    let (scc, _, stk, _) = List.fold_left
        (fun a (s, i, l) ->
            let (scc, smts, stk, index) = a in
            if (get_index smts s) = -1 then
                let (nscc, smts, stk, index) = strongconnect smts stk index s
                in (nscc@scc, smts, stk, index)
            else a
        )

        ([], smts, stk, index)
        smts

    in stk::scc;;

tarjan graphe1;;
