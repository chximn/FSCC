(* Algorithme de tarjan *)
let graphe1 = [(1,[6;7;8]) ; (2,[1;4]) ; (3, [2]) ; (4, [3;5]) ; (5, [1]) ; (6, [5;7]) ; (7, []) ; (8, [6;7])];;

(* Retourne la liste de tous les sommets *)
let sommets = List.map fst;;
sommets graphe1;;

(* Retourner la liste des successeurs d’un sommet donné *)
let successeurs sommet graphe = snd (List.find (fun (s, succ) -> s = sommet) graphe);;

(* Donner pour chaque sommet un indice = -1 et le plus petit numéro préfixe d’un sommet accessible (lowlink) = -1*)
let init_graph graph = List.map (fun (s, succ) -> s, -1, -1) graph;;

(*
 * Par la suite, on va utiliser un tuple pour sauvgarder les valuers des variables de l'algorithme (l'etat de l'algorithme)
 * le tuple est defini comme suit:
 *      etat = (sommets, pile, indice) où
 *      sommets : une liste de tuple de sommet, indice, lowlink (initialiser par la fonction init_graph)
 *      pile    : une pile des sommets
 *      indice  : un compteur
 *
 * On définera aussi des fonctions permettant de récuperer et changer {sommets, pile, indice}
 *)

 (* les fonctions suivantes ont été commentées car on utilise que acces_pile et changer_pile *)
 (* let etat_acces_sommets   (sommets, _   , _     ) = sommets;; *)
    let etat_acces_pile      (_      , pile, _     ) = pile;;
 (* let etat_acces_indice    (_      , _   , indice) = indice;; *)

 (* let etat_changer_sommets (_      , pile, indice) sommets = (sommets, pile, indice);; *)
    let etat_changer_pile    (sommets, _   , indice) pile    = (sommets, pile, indice);;
 (* let etat_changer_indice  (sommets, pile, _     ) indice  = (sommets, pile, indice);; *)


(* Changer l'indice d'un sommet et retourner le nouvel etat de l'algorithme *)
let sommet_changer_indice (sommets, pile, indice) sommet indice =
    (
        List.map (fun (s, i, l) -> if sommet = s then (s, indice, l) else (s, i, l)) sommets,
        pile,
        indice
    );;


(* Changer la valeur du lowlink d'un sommet et retourner le nouvel etat de l'algorithme *)
let sommet_changer_lowlink (sommets, pile, indice) sommet lowlink =
    (
        List.map (fun (s, i, l) -> if sommet = s then (s, i, lowlink) else (s, i, l)) sommets,
        pile,
        indice
    );;

(* Récuperer l'indice d'un sommet *)
let sommet_acces_indice (sommets, _, _) sommet = let (s, i, l) = List.find (fun (s, i, l) -> s = sommet) sommets in i;;

(* Récuperer la valeur du lowlink d'un sommet *)
let sommet_acces_lowlink (sommets, _, _) sommet = let (s, i, l) = List.find (fun (s, i, l) -> s = sommet) sommets in l;;

(* Calculer le minimum entre deux entiers *)
let min a b = if a > b then b else a;;

(* Dépiler les valeurs d'une pile jusqu'a un sommet et retourner la nouvelle pile et les sommets dépilée *)
let depiler_jusqua pile v =
    let rec dep pile rst = match rst with
          []     -> (pile, [])
        | x :: r -> if x = v then (x :: pile, r)
                    else dep (x::pile) r
    in dep [] pile;;



(* L'algoritme de tarjan pour trouver les composants fortement connexes *)
let tarjan graph =

    (* Parcours en profondeur *)
    let rec parcours etat v =

        (* Donner au sommet v le plus petit indice non utilisé  *)
        (* empiler le sommet v dans la pile                     *)
        (* incrémenter la valeur du compteur indice             *)

        let (_, pile, old_indice) = etat
        in let (sommets, _, _) = sommet_changer_indice etat v old_indice (* v.indice = indice *)
        in let etat = (
            sommets,
            v::pile, (* pile.empiler(v) *)
            old_indice + 1 (* indice += 1*)
        )

        (* Pour chaque successeurs du sommet v *)
        in let (composants, lowlink, etat) = List.fold_left
            (fun (composants, lowlink, etat) e ->

                (* Récuperer l'indice du sommet e *)
                let e_indice = sommet_acces_indice etat e in

                (* Si le sommet e n'est pas encore visité *)
                if (e_indice = -1) then

                    (* On fait un parcours en profondeur à partir du sommet e *)
                    let (ncomposants, etat) = parcours etat e

                    (* Calculer la nouvelle valeur du lowlink du sommet v *)
                    in let lowlink = (min lowlink (sommet_acces_lowlink etat e) )

                    (* Ajouter les nouveau composants et mettre à jour la valeur du lowlink du  sommet v*)
                    in (ncomposants @ composants, lowlink, etat)

                (* Si le sommet est visité et il est dans la pile *)
                else if (List.mem e pile)
                    (* Calculer la nouvelle valeur du lowlink du sommet v *)
                    then (composants, (min lowlink e_indice), etat)

                    (* Sinon, ignorer le sommet *)
                    else (composants, lowlink, etat)
            )

            (* l'accumulateur dans ce cas est un tuple (composants_fortement_connexes, lowlink, etat) *)
            ([], old_indice, etat)
            (successeurs v graph)

        (* Changer la valeur du lowlink du sommet v par la valeur retourner par `fold_left`*)
        in let etat = sommet_changer_lowlink etat v lowlink in

        (* if (v.indice = v.lowlink) *)
        if sommet_acces_indice etat v = lowlink then
            (* Alors, on a trouvé un nouveau composant fortement connexe *)
            (* Depiler tous les sommets dans la pile jusqu'au sommet v   *)
            let (dep, new_pile) = depiler_jusqua (etat_acces_pile etat) v

            (* Ajouter le nouveau composant fortement connexe à la liste et mettre à jour la pile *)
            in (dep::composants, etat_changer_pile etat new_pile)

        (* Sinon, retourner sans rien ajouter *)
        else (composants, etat)

    in


    (* Initialiser l'état de l'algoritme *)
    (*     sommets = init_graph graph    *)
    (*     pile    = [] (pile vide)      *)
    (*     indice  = 0  (compteur)       *)
    let etat = (init_graph graph, [], 0) in

    (* Pour chaque sommet du graphe *)
    let (composants, _) = List.fold_left (
            fun (composants, etat) sommet ->

                (* Si le sommet n'est pas encore visité *)
                if (sommet_acces_indice etat sommet) = -1 then
                    (* On fait un parcours en profondeur à partir du sommet e *)
                    let (new_composants, etat) = parcours etat sommet

                    (* Ajouter les nouveaux composants fortement connexes à la liste et mettre à jour l'état *)
                    in  (new_composants@composants, etat)

                (* Sinon, ignorer le sommet (n'ajouter rien) *)
                else (composants, etat)
        )

        (* l'accumulateur dans ce cas est un couple des composants fortement connexes et l'etat de l'algoritme *)
        ([], etat)
        (sommets graph)

    in composants;;

tarjan graphe1;;
