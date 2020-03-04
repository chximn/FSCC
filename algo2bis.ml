
let graphe1 = [(1,[6;7;8]) ; (2,[1;4]) ; (3, [2]) ; (4, [3;5]) ; (5, [1]) ; (6, [5;7]) ; (7, []) ; (8, [6;7])];;
let sommets = List.map fst;;

let successeurs sommet graphe = snd (List.find (fun (s, succ) -> s = sommet) graphe);;
successeurs 4 graphe1;;

let rec parcours_sommet x visited graphe =
    let (a, b) = List.fold_left (
         (* on ignore les sommets déjà visités *)
         fun (l, v) e -> if (List.mem e v) then (l, v)

                         (* on parcours le graphe à partir du successeurs `e` *)
                         else let (rl, rv) = parcours_sommet e v graphe

                              (* on ajoute la liste des sommets parcourus à partir de `e` à la fin *)
                              (* on mets à jour la liste des sommets visités                       *)
                              in  (l @ rl, rv)
     )

     (* on commence par ajouter x à la liste des sommet visités *)
     ([x], x::visited)

     (* on parcours les successeurs du sommet x dans le graphe *)
     (successeurs x graphe)

     in (a @ [-1], b);;

let parcours_prof graphe =
	(* pour chaque sommet du graphe *)
    fst (List.fold_left (
                        (* si le sommet n'est pas visité,
                           on parcours le graphe à partir de ce sommet *)
        fun (l, v) s -> if List.mem s v then (l, v)
                        else let (rl, rv) = parcours_sommet s v graphe
                        in       (l @ rl, rv)
    )
    ([], [])
    (sommets graphe));;

parcours_prof graphe1;;
