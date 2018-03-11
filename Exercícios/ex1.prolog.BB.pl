%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao sobre um universo de discurso na área da prestação de cuidados de saúde.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:-set_prolog_flag( discontiguous_warnings,off ).
:-set_prolog_flag( single_var_warnings,off ).
:-set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição de invariante

:-op(900,xfy,'::').

%-------------------------------------------------------------------------------------------
% BASE DE CONHECIMENTO
%-------------------------------------------------------------------------------------------
% Base de conhecimento com informação dos utentes, prestadores e cuidados

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/5.

%-------------------------------------------------------------------------------------------
%Extensão do predicado utente: IdUT, Nome, Idade, Morada -> {V,F}

utente(1,Pascoal,38,Rua Limpa).

% -------------------------------------------------------------------------------------------
%Extensão do predicado prestador: IdPrest, Nome, Especialidade, Instituição -> {V,F}

prestador(1,'Zeca Matos','Ortopedia','Hospital Privado de Braga'). 

% -------------------------------------------------------------------------------------------
%Extensão do predicado cuidado: Data, IdUt, IdPrest, Descrição, Custo  -> {V,F}

cuidado('2018-1-1',1,1,'Ortopedia',10).

% -------------------------------------------------------------------------------------------
%Extensão do predicado comprimento: Lista, Resultado -> {V,F}
comprimento([X|Y],R):- comprimento(Y,Z), 
						 R is Z+1.


%Extensão do predicado remove
remove(T):- retract(T).


%Extensão do predicado insere: 
insere(T):- assert(T).
insere(T):-retract(T),!,fail.


%Extensão do predicado evolução: Termo -> {V,F}
evolucao(T):- solucoes(Inv,+T::Inv,Lista),
			  inserir(T),
			  teste(Lista).


%Extensão do predicado involucao: Termo -> {V,F}
involucao(T):- solucoes(I,+T::I,Lista),
				teste(Lista),
				remove(T).


%Extensão do predicado teste: Lista -> {V,F}
teste([X|Y]):- X, teste(Y).


%Extensão do predicado soluções: Q,T, Lista de termos -> {V,F}
solucoes(Q,T,S):- findall(Q,T,S).

% -------------------------------------------------------------------------------------------


