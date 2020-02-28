%:- lib(listut).      % Placer cette directive en commentaire si vous utilisez Swi-Prolog 
   
                      % Sinon ne pas modifier si vous utilisez ECLiPSe Prolog :
                      % -> permet de disposer du predicat nth1(N, List, E)
                      % -> permet de disposer du predicat sumlist(List, S)
                      % (qui sont predefinis en Swi-Prolog)
:-consult(avl).

                      
%***************************
%DESCRIPTION DU JEU DU TAKIN
%***************************

   %********************
   % ETAT INITIAL DU JEU
   %********************   
   % format :  initial_state(+State) ou State est une matrice (liste de listes)
   

initial_state([ [b, h, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [a, f, d],       % 
                [g,vide,e] ]).   % h1=4,   h2=5,   f*=5



% AUTRES EXEMPLES POUR LES TESTS DE  A*

/*
initial_state([ [ a, b, c],        
                [ g, h, d],
                [vide,f, e] ]). % h2=2, f*=2

initial_state([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h2=10 f*=10
			
initial_state([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h2=16, f*=20
			
initial_state([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h2=24, f*=30 

initial_state([ [a, b, c],
                [g,vide,d],
                [h, f, e]]). % etat non connexe avec l'etat final (PAS DE SOLUTION)
*/  


   %******************
   % ETAT FINAL DU JEU
   %******************
   % format :  final_state(+State) ou State est une matrice (liste de listes)
   
final_state([[a, b,  c],
             [h,vide, d],
             [g, f,  e]]).

			 
   %********************
   % AFFICHAGE D'UN ETAT
   %********************
   % format :  write_state(?State) ou State est une liste de lignes a afficher

write_state([]).
write_state([Line|Rest]) :-
   writeln(Line),
   write_state(Rest).
   

%**********************************************
% REGLES DE DEPLACEMENT (up, down, left, right)             
%**********************************************
   % format :   rule(+Rule_Name, ?Rule_Cost, +Current_State, ?Next_State)
   
rule(up,   1, S1, S2) :-
   vertical_permutation(_X,vide,S1,S2).

rule(down, 1, S1, S2) :-
   vertical_permutation(vide,_X,S1,S2).

rule(left, 1, S1, S2) :-
   horizontal_permutation(_X,vide,S1,S2).

rule(right,1, S1, S2) :-
   horizontal_permutation(vide,_X,S1,S2).

   %***********************
   % Deplacement horizontal            
   %***********************
    % format :   horizontal_permutation(?Piece1,?Piece2,+Current_State, ?Next_State)
	
horizontal_permutation(X,Y,S1,S2) :-
   append(Above,[Line1|Rest], S1),
   exchange(X,Y,Line1,Line2),
   append(Above,[Line2|Rest], S2).

   %***********************************************
   % Echange de 2 objets consecutifs dans une liste             
   %***********************************************
   
exchange(X,Y,[X,Y|List], [Y,X|List]).
exchange(X,Y,[Z|List1],  [Z|List2] ):-
   exchange(X,Y,List1,List2).

   %*********************
   % Deplacement vertical            
   %*********************
   
vertical_permutation(X,Y,S1,S2) :-
   append(Above, [Line1,Line2|Below], S1), % decompose S1
   delete(N,X,Line1,Rest1),    % enleve X en position N a Line1,   donne Rest1
   delete(N,Y,Line2,Rest2),    % enleve Y en position N a Line2,   donne Rest2
   delete(N,Y,Line3,Rest1),    % insere Y en position N dans Rest1 donne Line3
   delete(N,X,Line4,Rest2),    % insere X en position N dans Rest2 donne Line4
   append(Above, [Line3,Line4|Below], S2). % recompose S2 

   %***********************************************************************
   % Retrait d'une occurrence X en position N dans une liste L (resultat R) 
   %***********************************************************************
   % use case 1 :   delete(?N,?X,+L,?R)
   % use case 2 :   delete(?N,?X,?L,+R)   
   
delete(1,X,[X|L], L).
delete(N,X,[Y|L], [Y|R]) :-
   delete(N1,X,L,R),
   N is N1 + 1.


   
   
   %*******************
   % PARTIE A COMPLETER
   %*******************
   
   %*******************************************************************
   % Coordonnees X(colonne),Y(Ligne) d'une piece P dans une situation U
   %*******************************************************************
	% format : coordonnees(?Coord, +Matrice, ?Element)
	% Définit la relation entre des coordonnees [Ligne, Colonne] et un element de la matrice
	/*
	Exemples
	
	?- coordonnees(Coord, [[a,b,c],[d,e,f]],  e).        % quelles sont les coordonnees de e ?
	Coord = [2,2]
	yes
	
	?- coordonnees([2,3], [[a,b,c],[d,e,f]],  P).        % qui a les coordonnees [2,3] ?
	P=f
	yes
	*/

	
   coordonnees([L,C], Mat, Elt) :-
      nth1(L,Mat,Ligne),
      nth1(C,Ligne,Elt).
											 
   %*************
   % HEURISTIQUES
   %*************
   
heuristique(U,H) :-
    heuristique1(U, H).  % au debut on utilise l'heuristique 1 
%   heuristique2(U, H).  % ensuite utilisez plutot l'heuristique 2  
   
   
   %****************
   %HEURISTIQUE no 1
   %****************
   % Nombre de pieces mal placees dans l'etat courant U
   % par rapport a l'etat final F
diff(X,Y,1):-
   X\=Y,
   X\=vide.
diff(X,Y,0):-
   X=Y,
   X\=vide.
diff(vide,_,0).

diffLigne([],[],0).
diffLigne([X|R1],[Y|R2],N):-
   diff(X,Y,N1),
   diffLigne(R1,R2,N2),
   N is N1+N2.

diffMat([],[],0).
diffMat([L1|R1],[L2|R2],N):-
   diffLigne(L1,L2,N1),
   diffMat(R1,R2,N2),
   N is N1 + N2.

heuristique1(U, H) :-
   final_state(Final),
   diffMat(U,Final,H).

mal_place(U,P1):-
   final_state(Fin),
   coordonnees([L,C],U,P1),
   coordonnees([L,C],Fin,P2),
   diff(P1,P2,1).
liste_Mal(U,L):-
   findall(X, mal_place(U,X), L).
   

   
   %****************
   %HEURISTIQUE no 2
   %****************
   
   % Somme des distances de Manhattan à parcourir par chaque piece
   % entre sa position courante et sa positon dans l'etat final

distance([L1,C1],[L2,C2],D):-
   R1 is abs(L1-L2),
   R2 is abs(C2-C1),
   D is R1 + R2.
heuristique2(U, H) :-
   final_state(Final),
   findall(Dist,
      (coordonnees(Coord1, U,P),
       P \= vide,
       coordonnees(Coord2,Final,P),
       distance(Coord1,Coord2,Dist),
       Dist>0
      ),
      Liste_des_Dist
   ),
   sum_list(Liste_des_Dist, H).
									
									
%a 
final_state4([[1,2,3,4],
             [5,6,7,8],
             [9,10,11,12],
             [13,14,15,vide]]).
bien_place(P):-
   initial_state(Init),
   final_state(Fin),
   coordonnees([L,C],Init,P),
   coordonnees([L,C],Fin,P).

couple([A,S]):-
   initial_state(Init),
   rule(A,1,Init,S).
main():-
   initial_state(S0),
   heuristique(S0,H0),
   empty(Pue),
   empty(Pfe),
   empty(Q),
   insert([ [H0,H0,0], S0 ],Pfe,Pf),
   insert([S0, [H0,H0,0], nil, nil],Pue,Pu),
   aetoile(Pf,Pu,Q).
aetoile(Pf,Pu,_):-
   empty(Pf),
   empty(Pu),
   display("PAS de SOLUTION : L’ETAT FINAL N’EST PAS ACCESSIBLE !").

aetoile(_,Pu,Q):-
   final_state(Final),
   suppress_min([Final, _],Pu, _),
   affiche_solution(Q,Final).

aetoile(Pf,Pu,Q):-
   suppress_min([[Fu,Hu,Gu],U],Pf,Pf2),
   suppress([U,[Fu,Hu,Gu],_,_],Pu,Pu2),
   %not(final_state(U)),
   findall(S,expand(U,Gu,S),Ls),
   loop_successors(Ls,Pu2,Pf2,Q,Puu,Pff),
   insert(S,Q,Qf),
   aetoile(Pff,Puu,Qf).

expand(S,G,Suiv):-
   rule(Action,1,S,S2),
   heuristique(S2,H2),
   G2 is G+1,
   F2 is G2+H2,
   Suiv=[S2,[F2,H2,G2],S,Action].
loop_successors([],_,_,_,_,_,_).
loop_successors(Ls,Pu,Q,Pf,PU,PF):-
   Ls=[S|Lsuiv],
   S=[U,[F,H,G],_,_],
   not(belongs(U,Q)),
   ( belongs([Vec,U],Pu) ->
   (
      Vec @> [F,H,G] ->
      suppress([U,_,_,_],Pu,Pu2),
      suppress([Vec,U],Pf,Pf2),
      insert(S,Pu2,Puu),
      insert([[F,H,G],U],Pf2,Pff)
      ;
      true
   )
   ;
   insert(S,Pu,Puu),
   insert([[F,H,G],U],Pf,Pff)
   ),
   loop_successors(Lsuiv,Puu,Pff,Q,PU,PF).
affiche_solution(_,Init):-
   initial_state(Init).
affiche_solution(Q,Final):-
   belongs([Final,_,Pere,_],Q),
   affiche_solution(Q,Pere),
   write(Final).