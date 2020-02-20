(* Algorithme 01 *)
let graphe1 = [(1,[6;7;8]) ; (2,[1;4]) ; (3, [2]) ; (4, [3;5]) ; (5, [1]) ; (6, [5;7]) ; (7, []) ; (8, [6;7])];;

(* 1. Retourne la liste de tous les sommets *)
let sommets = List.map fst;;
sommets graphe1;;

(* 2. Retourner la liste des successeurs d’un sommet donné *)
let successeurs sommet graphe = snd (List.find (fun (s, succ) -> s = sommet) graphe);;
successeurs 4 graphe1;;

(* 3. Inverser un graphe *)
let predecesseurs sommet graphe = List.fold_left (
        fun a (s, succ) -> if List.mem sommet succ then s::a
                           else a
    ) [] graphe;;

let inverse_graphe graphe = List.fold_left (fun a (s, succ) -> (s, (predecesseurs s graphe)) :: a ) [] graphe;;
inverse_graphe graphe1;;

(* 4. Efectuer le parcours en profondeur d’un graphe et retourner la liste des sommets parcourus en ordre sufxe (inversé). *)
let parcours_prof graphe =
	(* on parcours le graphe à partir du sommet x *)
	(* visited est la liste des sommets déja visités *)
    let rec parcours x visited =
        let (l, v) = List.fold_left (
                                         (* on ignore les sommets déjà visités *)
                         fun (l, v) e -> if (List.mem e v) then (l, v)

                                         (* on parcours le graphe à partir du successeurs `e` *)
                                         else let (rl, rv) = parcours e v

                                              (* on ajoute la liste des sommets parcourus à partir de `e` à la fin *)
                                              (* on mets à jour la liste des sommets visités *)
                                              in  (rl @ l, rv)
                     )

                     (* on commence par ajouter x à la liste des sommet visités *)
                     ([], x::visited)

					 (* on parcours les successeurs du sommet x dans le graphe *)
                     (successeurs x graphe)

		(* on ajoute le sommet x au début de la liste des sommets parcourus *)
        in (x::l, v)

	(* pour chaque sommet du graphe *)
    in fst (List.fold_left (
                            (* si le sommet n'est pas visité,
                               on parcours le graphe à partir de ce sommet *)
            fun (l, v) s -> if List.mem s v then (l, v)
                            else let (rl, rv) = parcours s v
                            in       (rl @ l, rv)
        )
        ([], [])
        (sommets graphe));;

parcours_prof graphe1;;
parcours_prof (inverse_graphe graphe1);;
(* 5. Rechercher les composantes fortement connexes par parcours du graphe et de son inverse. *)










(*




	
	

	L = parcours_prof(graphe)
	
	k = 0
	h = 0

	marque = [false, false, ..., false]
	comp   = [0, 0, ..., 0]
	


	foreach (let sommet in L.inverse()) {
		if (!marque[sommet]) {
			k++
			parcours(sommet, graphe, marque, comp, k)
		}	
	}


	graphe  -> [2; 4; 3; 1; 8; 6; 7; 5]
	inverse -> [7; 8; 1; 5; 4; 2; 3; 6]

	
*)










