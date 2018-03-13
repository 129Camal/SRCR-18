%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao sobre um universo de discurso na área da prestação de cuidados de saúde.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:-set_prolog_flag(discontiguous_warnings,off).
:-set_prolog_flag(single_var_warnings,off).
:-set_prolog_flag(unknown,fail).

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
utente(2,'Zeca',20,'Rua da Caça').
utente(3,'Anibal',59,'Rua do Gota').
utente(4,'Maria',42,'Rua dos Peões').
utente(5,'Carlota',22,'Rua do Speedy').
% -------------------------------------------------------------------------------------------
%Extensão do predicado prestador: IdPrest, Nome, Especialidade, Instituição -> {V,F}

prestador(1,'Ze','Otorrino',1). 
prestador(2,'Andreia','Dentaria',1).
prestador(3,'Guilherme','Dermatologia',1).
prestador(4,'Manuel','Oncologia',2).
prestador(5,'Elso','Ortopedia',3).

% -------------------------------------------------------------------------------------------
%Extensão do predicado cuidado: Data, IdUt, IdPrest, Descrição, Custo  -> {V,F}

cuidado('2018-1-1',1,1,'perna',10).
cuidado('2018-1-1',2,2,'Carie',26).
cuidado('2018-1-1',3,3,'Acne',15).
cuidado('2018-1-2',4,4,'Cancro',32).
cuidado('2018-1-2',5,5,'Fratura do pulso',19).

% -------------------------------------------------------------------------------------------
%Extensão do predicado instituição: IdInst, Nome, Cidade -> {V,F}
inst(1,'Hospital Privado de Braga', 'Braga').
inst(2,'IPO','Porto').
inst(3,'Hospital S.Joao','Porto').


% -------------------------------------------------------------------------------------------
%Extensão do predicado comprimento: Lista, Resultado -> {V,F}
comprimento([],[]).
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

% -----------------------------------------------------------------------
% Identificar instituições prestadoras de cuidados de saúde
% inst_cuidados: I -> {V,F}

inst_cuidados(R1) :- solucoes(inst(Id,N,C), (inst(Id,N,C), prestador(Idp,_,_,Id), cuidado(_,Idp,_,_,_)), R),
					apagaRep(R,R1).

apaga1(X,[],[]).
apaga1(X,[X|Y],T):- apaga1(X,Y,T).
apaga1(X,[H|Y],[H|R]) :- apaga1(X,Y,R).

apagaRep([],[]).
apagaRep([X|Y],[X|L1]) :- apaga1(X,Y,L), apagaRep(L,L1).

% ------------------------------------------------------------------------
% Identificar cuidados de saúde prestados por instituição
% cuidadosI: I,L -> {V,F}

cuidados_I(N,R) :- solucoes(cuidado(D,IDU,IDP,Desc,Custo), (inst(Id,N,_), prestador(Idp,_,_,Id), cuidado(D,IDU,Idp,Desc,Custo)),R).

% Identificar cuidados de saúde prestados por cidade
% cuidados_C: 

cuidados_C(C,R) :- solucoes(cuidado(D,IDU,IDP,Desc,Custo), (inst(ID,_,C), prestador(IDP,_,_,ID), cuidado(D,IDU,IDP,Desc,Custo)),R). 
				   

% Identificar cuidados de saúde prestados por data
% cuidados_D: Data, LResultado ->{V,F}

cuidados_D(D,R1) :- solucoes((D,Idu,Idp,Desc,C), cuidado(D,Idu,Idp,Desc,C),R1).


% ------------------------------------------------------------------------
% Identificar os utentes de um prestador/especialidade/instituição





