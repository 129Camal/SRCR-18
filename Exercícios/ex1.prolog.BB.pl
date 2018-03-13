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

utente(1,'Pascoal',38,'Rua Limpa').

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
% Invariantes
% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o utente

+utente(id,nome,idade,morada)::((solucoes(id, utente(idU,n,idd,mor), U), comprimento(U,N), N==1)).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o prestador

+prestador(idPrest,nome,esp,inst)::((solucoes(id, prestador(id,n,e,itt), P), comprimento(P,N), N==1)).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o cuidado

+cuidado(data,idUt,idPrest,desc,custo) :: (solucoes((d,iU,iP,d,c), cuidado(d,iU,iP,d,c), C), comprimento(C,N), N ==1).

% -------------------------------------------------------------
% Identificar os utentes por critérios de seleção 

utenteID(ID,R) :- solucoes((ID,N,I,M), utente(ID,N,I,M), R).
utenteNome(Nome,R) :- solucoes((ID,Nome,I,M), utente(ID,Nome,I,M), R).
utenteIdade(Idade,R) :- solucoes((ID,N,Idade,M),utente(ID,N,Idade,M),R).
utenteMor(M,R) :- solucoes((ID,N,I,M),utente(ID,N,I,M),R).

% -------------------------------------------------------------
% Identificar as instituições prestadoras de cuidados de saúde
% inst_cuidados: I -> {V,F}

inst_cuidados(R1) :- solucoes(I, prestador(_,_,_,I), R),
					apagaRep(R,R1).

apaga1(X,[],[]).
apaga1(X,[X|Y],Y).
apaga1(X,[H|Y],[H|R]) :- apaga1(X,Y,R).

apagaRep([],[]).
apagaRep([X|Y],R) :- apaga1(X,Y,L), apagaRep(L,L1), R = [X|L1].

% -------------------------------------------------------------
% Identificar cuidados de saúde prestados por instituição
% cuidadosI: I,L -> {V,F}

% cuidadosI(I,[]) :- solucoes((d,, cuidado(_,)) % 2 maneiras q pensei: 1. ao usar cuidado, organizar por prestador, buscar inst, tirar rep
																	 % 2. usar prestador(id e inst) p buscar lista depois predicado p dar cuidados a partir do id do prestador

% Identificar cuidados de saúde prestados por cidade
% cuidados_ICD()

% Identificar cuidados de saúde prestados por data
% cuidados_ICD()