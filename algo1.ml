(* Algorithme 01 *)
let graphe1 = [(1,[6;7;8]) ; (2,[1;4]) ; (3, [2]) ; (4, [3;5]) ; (5, [1]) ; (6, [5;7]) ; (7, []) ; (8, [6;7])];;

(* 1. Retourne la liste de tous les sommets *)
let sommets = List.map fst;;
sommets graphe1;;

(* 2. Retourner la liste des successeurs d’un sommet donné *)
let successeurs sommet graphe = snd (List.find (fun (s, succ) -> s = sommet) graphe);;
successeurs 4 graphe1;;

(* 3. Inverser un graphe *)
(* TODO: Amelioer l'algorithme *)
let predecesseurs sommet graphe = List.fold_left (
        fun a (s, succ) -> if List.mem sommet succ then s::a
                           else a
    ) [] graphe;;

let inverse_graphe graphe = List.map (fun (s, succ) -> s, (predecesseurs s graphe)) graphe;;
inverse_graphe graphe1;;

(* 4. Efectuer le parcours en profondeur d’un graphe et retourner la liste des sommets parcourus en ordre sufxe (inversé). *)
let parcours_prof graphe =
    let rec parcours x visited =
        let (l, v) = List.fold_left (
                         fun (l, v) e -> if (List.mem e v) then (l, v)
                                         else let (rl, rv) = parcours e v
                                              in (rl @ l, rv)
                     )

                     ([], x::visited)
                     (successeurs x graphe)
        in (x::l, v)

    in fst (List.fold_left (
            fun (l, v) s -> if List.mem s v then (l, v)
                            else let (rl, rv) = parcours s v
                            in (rl @ l, rv)
        )
        ([], [])
        (sommets graphe));;

parcours_prof graphe1;;
