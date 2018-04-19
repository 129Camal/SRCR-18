%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento com informacao sobre um universo de discurso na área da prestação de cuidados de saúde.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:-style_check(-discontiguous).
:-set_prolog_flag(discontiguous_warnings,off).
:-set_prolog_flag(single_var_warnings,off).
%:-set_prolog_flag(unknown,fail).
:- discontiguous (::)/2.
:- discontiguous excecao/1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Definição de invariante

:-op(900,xfy,'::').
:-op(400,yfx,'$$'). %operador da conjunção 
:-op(400,yfx,'//'). %operador da disjunção ---- VERIFICAR TODOS
:-op(700,xfx,'equals'). %operador de igualdade

%-------------------------------------------------------------------------------------------
% BASE DE CONHECIMENTO
%-------------------------------------------------------------------------------------------
% Meta predicados.

:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/6.
:- dynamic inst/3.
:- dynamic data/3.
:- dynamic (-)/1.
:- dynamic excecao/1.

%-------------------------------------------------------------------------------------------
%Extensão do predicado utente: IdUT, Nome, Idade, Morada -> {V,F}

utente(1,"Pascoal",38,"Rua Limpa").
utente(2,"Zeca",20,"Rua da Capa").
utente(3,"Anibal",59,"Rua do Gota").
utente(4,"Maria",42,"Rua dos Peoes").
utente(5,"Carlota",22,"Rua do Speedy").
utente(6,"Brito",65,"Rua do Colombo").
utente(7,"Micaela",8,"Rua dos Olivais").
utente(8,"Julio",36,"Rua do Campo").
utente(9,"Dinis",48,"Rua da Cruz").
utente(10,"Rita",88,"Rua das Flores").
utente(11,"Mariana",6,"Rua do Carmo").
utente(12,"Sergio",26,"Rua dos Limoes").
utente(13,"Lucifer",14,"Rua do Palacio").
utente(14,"Miguel",49,"Rua do Pinheiro").
utente(15,"Joana",70,"Rua da Maria").

-utente(Idu,N,Idd,M) :- nao(utente(Idu,N,Idd,M)), nao(excecao(utente(Idu,N,Idd,M))).
% -------------------------------------------------------------------------------------------
%Extensão do predicado prestador: IdPrest, Nome, Especialidade, IdInstituição -> {V,F}

prestador(1,"Ze","Otorrino",1). 
prestador(2,"Andreia","Dentaria",1).
prestador(3,"Guilherme","Dermatologia",1).
prestador(4,"Manuel","Oncologia",2).
prestador(5,"Elso","Ortopedia",3).
prestador(6,"Bino","Ginecologia", 3).
prestador(7,"Telmo","Radiologia",2).
prestador(8,"Miquelina","Cardiologia",4).
prestador(9,"Armando","Neurologia",4).
prestador(10,"Firmino","Endocrinologia",5).
prestador(11,"Horacio","Otorrino",5).
prestador(12,"Carlos","Dentaria",1).
prestador(13,"Maria","Dermatologia",3).
prestador(14,"Narcisa","Ginecologia",2).
prestador(15,"Adelaide","Psiquiatria",4).
prestador(16,"Teresa","Nutricao",1).
prestador(17,"Ambrosio","Podologia",5).
prestador(18,"Nuno","Radiologia",2).
prestador(19,"Marta","Cardiologia",4).
prestador(20,"David","Oncologia",5).

-prestador(Idp,Nome,Esp,Inst) :- nao(prestador(Idp,Nome,Esp,Inst)), nao(excecao(prestador(Idp,Nome,Esp,Inst))).

% -------------------------------------------------------------------------------------------
%Extensão do predicado cuidado: IdData, IdUt, IdPrest, Prioridade, Descrição, Custo -> {V,F}

cuidado(1,1,1,"Media","Amigdalite",10).
cuidado(2,2,1,"Alta","Carie",26).
cuidado(3,3,3,"Baixa","Acne",15).
cuidado(4,4,4,"Alta","Cancro",32).
cuidado(5,5,5,"Media","Fratura do pulso",19).
cuidado(6,4,6,"Baixa","Papa Nicolau", 100).
cuidado(7,7,20,"Alta","Cancro da Mama",20).
cuidado(8,8,19,"Alta","Enfarte",198).
cuidado(9,9,18,"Baixa","Tirar Raio-X",3).
cuidado(10,10,17,"Media","Unha encravada",37).
cuidado(11,11,16,"Baixa","Plano Alimentar",12).
cuidado(12,12,15,"Media","Consulta rotina",90).
cuidado(13,13,14,"Media","Ecografia aos ovarios",58).
cuidado(14,14,13,"Alta","Urticaria",5).
cuidado(15,15,12,"Baixa","Por aparelho",2000).
cuidado(16,5,5,"Alta","Fratura exposta na perna",18).
cuidado(17,8,18,"Raio-X ao coração",400).

-cuidado(Data,Idu,Idp,Prio,Desc,Custo) :- nao(cuidado(Data,Idu,Idp,Prio,Desc,Custo)), nao(excecao(cuidado(Data,Idu,Idp,Prio,Desc,Custo))).

% -------------------------------------------------------------------------------------------
%Extensão do predicado instituição: IdInst, Nome, Cidade -> {V,F}
inst(1,"Hospital Privado de Braga", "Braga").
inst(2,"IPO","Porto").
inst(3,"Hospital S.Joao","Porto").
inst(4,"Hospital de Felgueiras","Felgueiras").
inst(5,"Hospital dos Bonecos","Lisboa").

-inst(Idi,Nome,Cidade) :- nao(inst(Idi,Nome,Cidade)), nao(excecao(inst(Idi,Nome,Cidade))).

% -------------------------------------------------------------------------------------------
%Extensão do predicado data: IdData, Ano, Mês, Dia -> {V,F}
data(1,2018,1,1).
data(2,2018,1,1).
data(3,2018,1,1).
data(4,2018,1,2).
data(5,2018,1,2).
data(6,2018,1,3).
data(7,2018,1,3).
data(8,2018,1,3).
data(9,2018,1,4).
data(10,2018,1,4).
data(11,2018,1,5).
data(12,2018,2,5).
data(13,2018,2,6).
data(14,2018,2,6).
data(15,2018,2,7).
data(16,2018,1,2).
data(17,2018,3,5).

-data(IdD,Ano,Mes,Dia) :- nao(data(IdD,Ano,Mes,Dia)), nao(excecao(data(IdD,Ano,Mes,Dia))).

% ------------------------------------------------------------------------------------------------
%   Sistema de Inferência
% ------------------------------------------------------------------------------------------------


equals(verdadeiro,$$(verdadeiro,verdadeiro)).
equals(falso,$$(falso,verdadeiro)).
equals(falso,$$(verdadeiro,falso)).
equals(falso,$$(falso,falso)).
equals(desconhecido,$$(desconhecido,desconhecido)).
equals(desconhecido,$$(desconhecido,verdadeiro)).
equals(desconhecido,$$(verdadeiro,desconhecido)).
equals(desconhecido,$$(desconhecido,falso)).
equals(desconhecido,$$(falso,desconhecido)).

equals(verdadeiro,//(verdadeiro,verdadeiro)).
equals(verdadeiro,//(verdadeiro,falso)).
equals(verdadeiro,//(falso,verdadeiro)).
equals(falso,//(falso,falso)).
equals(desconhecido,//(desconhecido,desconhecido)).
equals(desconhecido,//(desconhecido,verdadeiro)).
equals(desconhecido,//(verdadeiro,desconhecido)).
equals(desconhecido,//(desconhecido,falso)).
equals(desconhecido,//(falso,desconhecido)).


% ------------------------------------------------------------------------------------------------
%   Conhecimento Incerto
% ------------------------------------------------------------------------------------------------
%um valor nulo do tipo desconhecido e não, necessariamente, de um conjunto determinado de valores.

%Desconhecimento da morada do utente e respetiva excecao
utente(16,"Joaquina",57,morada_desconhecida).
utente(17,"Quim",38,morada_desconhecida).

excecao(utente(Idu,N,Idd,_)) :- utente(Idu,N,Idd,morada_desconhecida).


%Desconhecimento da idade de um utente mas sabendo que não é 20 anos
utente(18,"Jonas",idade_desconhecida,"Rua de Cristal").
-utente(18,"Jonas",20,"Rua de Cristal").

excecao(utente(Idu,N,_,M)) :- utente(Idu,N,idade_desconhecida,M).	


%O senhor X entrou de urgência no Hospital S.Joao com suspeita de ter sido envenenado.
%Após investigação, sabe-se que X é um espião russo, então os seus dados são interditos para preservar a sua identidade.
%No entanto, após contacto, apareceu o agente especial K para este preencher os dados do espião 
%e tornar o conhecimento desconhecido em conhecimento perfeito
utente(22,nome_interdito,idade_interdita,morada_interdita).
+utente(Idu,Nome,Idd,M) :: (solucoes((Idu,Nome,Idd,M), (utente(Idu,nome_interdito,idade_interdita,morada_interdita),
												 nao(nulo(nome_interdito)),nao(nulo(idade_interdita)),nao(nulo(morada_interdita))),R),
												 comprimento(R,N), N == 0).

%preenche_utente_todos(Id,Nome,Idade,Morada) :- preencher_utente_nome(Id,Nome), %---- VERIFICAAAAAAAAAAR ------
%											   preencher_utente_idade(Id,Idade), 
%											   preencher_utente_morada(Id,Morada).

% -------------------------------------------------------------------------------------------------
%   Conhecimento Impreciso
% -------------------------------------------------------------------------------------------------
%valor nulo que pertence a um conjunto de valores bem determinados mas 
%ao qual nao se sabe qual o valor concreto

%O prestador Amaral trabalha em três instituições diferentes
%O cuidado do deste prestador pode ter sido efetuado em qualquer uma das instituições.

excecao(prestador(21,"Amaral","Optometria",1)).
excecao(prestador(21,"Amaral","Optometria",3)).
excecao(prestador(21,"Amaral","Optometria",5)).

%Devido a problemas na base de conhecimento perderam-se alguns parâmetros 
%das datas dos cuidados de saúde

%Dia perdido
data(18,2018,4,dia_desconhecido).
excecao(cuidado(data(18,2018,4,D),6,14,"Alta","Ecografia",30)) :- D >= 1, D =< 30.

%Ano perdido
data(19,ano_desconhecido,4,5).
excecao(cuidado(data(19,Ano,4,5),3,12,"Baixa","Branqueamento",190)) :- Ano >= 2013, Ano =< 2015.

%Imprecisão na idade de um utente, mas sabendo que é aproximadamente 50
excecao(utente(19,"Carmo",Idd,"Rua dos Pinheiros")) :- aproximadamente(Idd,50).

aproximadamente(X,Y) :- W is 0.85 * Y, Z is 1.15 * Y, X >= W, X =< Z.


% -------------------------------------------------------------------------------------------------
%   Conhecimento Interdito
% -------------------------------------------------------------------------------------------------
%valor nulo desconhecido e não é permitida a especificação do mesmo

%Impossibilidade de saber uma certa prioridade

nulo(prioridade_interdita).
nulo(idade_interdita).
nulo(morada_interdita).
nulo(nome_interdito).

cuidado(17,10,9,prioridade_interdita,"AVC",20).
cuidado(17,2,8,prioridade_interdita,"Arritmias",11).
+cuidado(Data,Idu,Idp,Prio,Desc,Custo) :: (solucoes((Data,Idu,Idp,Prio,Desc,Custo), (cuidado(18,10,9,prioridade_interdita,"AVC",20), 
																					nao(nulo(prioridade_interdita))), R), comprimento(R,N), N==0).

+cuidado(Data,Idu,Idp,Prio,Desc,Custo) :: (solucoes((Data,Idu,Idp,Prio,Desc,Custo), (cuidado(18,2,8,prioridade_interdita,"Arritmias",11), 
																					nao(nulo(prioridade_interdita))), R), comprimento(R,N), N==0).

excecao(cuidado(D,Idu,Idp,_,Desc,C)) :- cuidado(D,Idu,Idp,prioridade_interdita,Desc,C).


%Impossibilidade de saber uma certa idade
utente(20,"Tina",idade_interdita,"Rua do Chiado").
+utente(Idu,Nome,Idd,M) :: (solucoes((Idu,Nome,Idd,M), (utente(Idu,Nome,idade_interdita,"Rua do Chiado"), 
												 nao(nulo(idade_interdita))),R), comprimento(R,N),N==0).

%Impossibilidade de saber uma certa morada
utente(21,"Mino",37,morada_interdita).
+utente(Idu,Nome,Idd,M) :: (solucoes((Idu,Nome,Idd,M), (utente(Idu,Nome,Idd,morada_interdita), 
												 nao(nulo(morada_interdita))),R), comprimento(R,N),N==0).



%-------Transformar conhecimento desconhecido em conhecimento perfeito-------------

preencher_utente_idade(Id, Idade):- utenteID(Id, utente(Id, N, S, M)),
	     			 					   atom(S), nao( atom(Idade) ),
    								       troca(utente(Id, N, S, M), utente(Id, N, Idade, M)).

preencher_utente_morada(Idu,M) :- utenteID(Idu,utente(I,N,Id,Mo)), 
										   atom(Mo), nao(atom(M)), 
										   troca(utente(I,N,Idd,Mo), utente(I,N,Idd,M)).

%preencher_prestador_inst(Idp,Inst) :-
% -------------------------------------------------------------------------------------------------
%Extensão do predicado comprimento: Lista, Resultado -> {V,F}
comprimento([],0).
comprimento([X|Y],R):- comprimento(Y,Z), 
						 R is Z+1.


%Extensão do predicado remove
remove(T):- retract(T).


%Extensão do predicado insere: 
insere(T):- assert(T).
insere(T):-retract(T),!,fail.


%Extensão do predicado evolução: Termo -> {V,F}
evolucao(T):- solucoes(Inv,+T::Inv,Lista),
			  insere(T),
			  teste(Lista).


%Extensão do predicado involucao: Termo -> {V,F}
involucao(T):- solucoes(I,-T::I,Lista),
				teste(Lista),
				remove(T).


%Extensão do predicado teste: Lista -> {V,F}
teste([]).
teste([X|Y]):- X, teste(Y).


%Extensão do predicado soluções: Q,T, Lista de termos -> {V,F}
solucoes(Q,T,S):- findall(Q,T,S).

% Extensao do predicado concat: lista, lista, resultado, concat -> {V,F}
concat([], Y, Y).
concat([X|Y], Z, [X|L]) :- concat(Y,Z,L).

%Extensão do predicado nao: Questao -> {V,F}
nao(Q) :- Q,!,fail.
nao(Q).

%Extensão do predicado demo: Questao, Resposta -> {V,F}
demo(Q,verdadeiro) :- Q.
demo(Q,falso) :- -Q.
demo(Q,desconhecido) :- nao(Q), nao(-Q).

%Extensão do predicado demoConj: ListaQuestoes, Resposta({V,F,D}) -> {V,F}
demoConj([],verdadeiro).
demoConj([X],R) :- demo(X,R).
demoConj([Q1|Q2],R) :- demo(Q1,R1), demoConj(Q2,R2), conjuncao(R1,R2,R).

%Extensão do predicado demoConj: ListaQuestoes, Resposta({V,F,D}) -> {V,F}
demoDisj([],falso).
demoDisj([X],R) :- demo(X,R).
demoDisj([Q1|Q2],R) :- demo(Q1,R1), demoDisj(Q2,R2), disjuncao(R1,R2,R).

%Extensão do predicado conjunção: VV1, VV2, Resultado -> {V,F,D}
conjuncao(X,Y,R) :- R equals X$$Y.

disjuncao(X,Y,R) :- R equals X//Y.

%Extensão do predicado troca: Antigo, Recente -> {V,F}
troca(A,R) :- remove(A), evolucao(R).
troca(A,_) :- assert(A), !, fail.


% -------------------------------------------------------------------------------------------
% -------------------------------------------------------------------------------------------
%                            INVARIANTES
% -------------------------------------------------------------------------------------------
% -------------------------------------------------------------------------------------------


% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o utente

+utente(Id,_,_,_)::((solucoes(Id, utente(Id,_,_,_), U), comprimento(U,N), N==1)).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o prestador

+prestador(IdPrest,Nome,Esp,Inst)::((solucoes(IdPrest, prestador(IdPrest,Nome,Esp,Inst), P), comprimento(P,N), N==1)).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o cuidado

+cuidado(Data,IdU,IdPrest,Prio,Desc,Custo) :: (solucoes((Data,IdU,IdPrest,Prio,Desc,Custo), cuidado(Data,IdU,IdPrest,Prio,Desc,Custo), C), comprimento(C,N), N ==1).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para a instituição

+inst(Id,Nome,Cid) :: (solucoes(Id, inst(Id,Nome,Cid),S), comprimento(S,N), N ==1).

%Invariante estrutural: nao permite inserção de conhecimento repetido para a data. 

+data(IdD,_,_,_) :: (solucoes(data(IdD,Ano,Mes,Dia), data(IdD,_,_,_), R), comprimento(R,N), N ==1).

%Invariante estrutural: nao permite a inserção de um cuidado cujo id de utente e 
%						de prestador não façam parte da base de conhecimento

+cuidado(Data,IdU,IdPrest,Prio,Desc,Custo) :: (utentes_de_esp(IdU,_,_,_), prestador(IdPrest,_,_,_)).


%%Invariante estrutural que garante que duas datas com Id diferentes não têm os mesmo dados

+data(_,Ano,Mes,Dia) :: (solucoes((Ano,Mes,Dia), data(_,Ano,Mes,Dia), R), comprimento(R,N), N==1).


%Invariante estrutural para controlo de remoção de utente, prestador, cuidado e instituição

-utente(Idu,N,Idd,M):: (solucoes(Idu, utente(Idu,N,Idd,M), L), comprimento(L,R), R==1).

-prestador(Idp,N,Esp,Inst) :: (solucoes(Idp, prestador(Idp,N,Esp,Inst), L), comprimento(L,R), R==1).

-cuidado(Data,IdU,IdPrest,Prio,Desc,Custo) :: (solucoes((Data,IdU,IdPrest,Prio,Desc,Custo), cuidado(Data,IdU,IdPrest,Prio,Desc,Custo), L), comprimento(L,R), R==1).

-inst(Id,N,C) :: (solucoes(Id, inst(Id,N,C), L), comprimento(L,R), R ==1).


% -------------------------------------------------------------------------------------------
% -------------------------------------------------------------------------------------------
%       	REQUISITOS
% -------------------------------------------------------------------------------------------
% -------------------------------------------------------------------------------------------


% 1-------------------------------------------------------------
% Registar utentes, prestadores, cuidados e instituições
% Extensao do predicado registaUtente: T -> {V,F}

registaUtente(Id,N,Idd,M) :- evolucao(utente(Id,N,Idd,M)).

% Extensão do predicado registaPrest: T -> {V,F}

registaPrest(Idp,N,E,I) :- evolucao(prestador(Idp,N,E,I)).

% Extensão do predicado registaCuidado: T -> {V,F}

registaCuidado(D,Idu,Idp,P,Desc,C) :- evolucao(cuidado(D,Idu,Idp,P,Desc,C)).

% Extensao do predicado registaInst: T -> {V,F}

registaInst(Id,N,C) :- evolucao(inst(Id,N,C)).

% 2-------------------------------------------------------------
% Remover utentes, prestadores, cuidados de saúde e instituição;

% Extensao do predicado removeUtentes : L -> {V,F}

removeUtente(ID) :- involucao(utente(ID,_,_,_)).


% Extensao do predicado removePrestador : L -> {V,F}

removePrestador(ID) :- involucao(prestador(ID,_,_,_)).

% Extensao do predicado removeCuidado: L -> {V,F}

removeCuidado(Dt,IdU,IdP,P,Desc,C) :- involucao(cuidado(Dt,IdU,IdP,P,Desc,C)).


% Extensao do predicado removeInst: L -> {V,F}

removeInst(IdInst) :- involucao(inst(IdInst,_,_)).


% 3-------------------------------------------------------------
% Identificar os utentes por critérios de seleção 

utenteID(ID,R) :- solucoes(utente(ID,N,I,M), utente(ID,N,I,M), [R|_]).
utenteNome(Nome,R) :- solucoes((ID,Nome,I,M), utente(ID,Nome,I,M), R).
utenteIdade(Idade,R) :- solucoes((ID,N,Idade,M),utente(ID,N,Idade,M),R).
utenteMor(M,R) :- solucoes((ID,N,I,M),utente(ID,N,I,M),R).

% 4-----------------------------------------------------------------------
% Identificar instituições prestadoras de cuidados de saúde
% inst_cuidados: ListaResultado -> {V,F}

inst_cuidados(R1) :- solucoes(inst(Id,N,C), (inst(Id,N,C), prestador(Idp,_,_,Id), cuidado(_,Idp,_,_,_,_)), R),
					apagaRep(R,R1).

% Extensao do predicado que apaga todas ocorrencias de 1 elemento numa lista
% apaga1: Elemento, Lista, ListaResultado -> {V,F}

apaga1(_,[],[]).
apaga1(X,[X|Y],T):- apaga1(X,Y,T).
apaga1(X,[H|Y],[H|R]) :- apaga1(X,Y,R).

% Extensao do predicado que apaga todos os elementos repetidos de uma lista
% apagaRep: Lista, ListaResultado -> {V,F}

apagaRep([],[]).
apagaRep([X|Y],[X|L1]) :- apaga1(X,Y,L), apagaRep(L,L1).

% 5------------------------------------------------------------------------
% Identificar cuidados de saúde prestados por instituição
% cuidadosI: I,L -> {V,F}

cuidados_I(N,R) :- solucoes(cuidado(D,Idu,Idp,P,Desc,Custo), (inst(Id,N,_), prestador(Idp,_,_,Id), cuidado(D,Idu,Idp,P,Desc,Custo)),R).

% Identificar cuidados de saúde prestados por cidade
% cuidados_C: 

cuidados_C(C,R) :- solucoes(cuidado(D,Idu,Idp,P,Desc,Custo), (inst(ID,_,C), prestador(Idp,_,_,ID), cuidado(D,Idu,Idp,P,Desc,Custo)),R). 
				   

% Identificar cuidados de saúde prestados por data
% cuidados_D: Data, LResultado ->{V,F}

cuidados_D(D,R1) :- solucoes((D,Idu,Idp,P,Desc,C), cuidado(D,Idu,Idp,P,Desc,C),R1).


% 6-----------------------------------------------------------------------------------------
% Identificar os utentes de um prestador
% utentes_de_prest: IdPrest, Resultado -> {V,F}

utentes_de_prest(Idp,R) :- solucoes(utente(Idu,N,Idd,M), (cuidado(_,Idu,Idp,_,_,_), prestador(Idp,_,_,_), utente(Idu,N,Idd,M)),R1),
						   apagaRep(R1,R).



% Identificar os utentes de uma especialidade
% utentes_de_esp: Especialidade, Resultado -> {V,F}

utentes_de_esp(Esp,R) :- solucoes(utente(Id,N,Idd,M), (cuidado(_,Id,Idp,_,_,_), prestador(Idp,_,Esp,_), utente(Id,N,Idd,M)), R1),
						 apagaRep(R1,R).

% Identificar os utentes de uma instituição dando o nome da instituição
% utentes_de_inst: IdPrest, Resultado -> {V,F}

utentes_de_instNome(NomeI,R) :- solucoes(utente(Id,N,Idd,M), (cuidado(_,Id,Idp,_,_,_), prestador(Idp,_,_,Idinst), inst(Idinst,NomeI,_), utente(Id,N,Idd,M)), R).


% 7------------------------------------------------------------------------------------------
% Identificar cuidados de saúde realizados por utente
% cuidados_por_utente: IdUt, ListaResultado -> {V,F}

cuidados_por_utente(Idu,R) :- solucoes(cuidado(D,Idu,Idp,P,Desc,Custo), cuidado(D,Idu,Idp,P,Desc,Custo), R).


% Identificar cuidados de saúde realizados por prestador
% cuidados_por_prest: IdPrest, ListaResultado -> {V,F}

cuidados_por_prest(Idp,R) :- solucoes(cuidado(D,Idu,Idp,P,Desc,Custo), (cuidado(D,Idu,Idp,P,Desc,Custo), prestador(Idp,_,_,_)), R).


% 8-----------------------------------------------------------------------------------------
% Determinar todas instituições a que um utente já recorreu
% todas_inst: Idu, ListaResultado -> {V,F}

todas_inst(Idu,R) :- solucoes(inst(Idi,N,C), (inst(Idi,N,C), cuidado(_,Idu,Idp,_,_,_), prestador(Idp,_,_,Idi), utente(Idu,_,_,_)), R1),
					 apagaRep(R1,R).


% Determinar todas os prestadores a que um utente já recorreu
% todos_prest: Idu, ListaResultado -> {V,F}

todos_prest(Idu,R) :- solucoes(prestador(Idp,N,Esp,Idi), (prestador(Idp,N,Esp,Idi), cuidado(_,Idu,Idp,_,_,_)), R1),
					  apagaRep(R1,R).


% 9-----------------------------------------------------------------------------------------
% Calcular o custo total dos cuidados de saúde por utente/especialidade/prestador/datas
% custo_utente: Idu, Resultado -> {V,f}

custo_utente(Idu,R) :- solucoes(Custo, cuidado(_,Idu,_,_,_,Custo), R1),
					   custo_total(R1,R).


% custo_esp: Especialidade, Resultado -> {V,f}

custo_esp(Esp,R) :- solucoes(Custo, (cuidado(_,_,Idp,_,_,Custo), prestador(Idp,_,Esp,_)), R1),
					custo_total(R1,R).


% custo_prest: IdPrest, Resultado -> {V,f}

custo_prest(Idp,R) :- solucoes(Custo, (cuidado(_,_,Idp,_,_,Custo), prestador(Idp,_,_,_)), R1),
					  custo_total(R1,R).


% custo_data: Data, Resultado -> {V,f} 

custo_data(D,R) :- solucoes(Custo, cuidado(D,_,_,_,_,Custo),R1),
				   custo_total(R1,R).


% Extensão do predicado para o calculo do custo total de uma lista de custos
% custo_total: Lista, Resultado -> {V,F}

custo_total([X],X).
custo_total([X,Y|Z], R) :- custo_total([X+Y|Z], R1), R is R1.

%----------------------------------------------------------------------
%						EXTRAS
%----------------------------------------------------------------------

% predicado que devolve o número total de utentes
% total_utentes: Resultado -> {V,F}

total_utentes(R) :- solucoes(Id, utente(Id,_,_,_), L), comprimento(L,R).



% predicado que devolve o número total de prestadores
% total_prest : R -> {V,F}

total_prestadores(R) :- solucoes(Idp, prestador(Idp,_,_,_), L), comprimento(L,R).



% predicado que devolve o número total de cuidados
total_cuidados(R) :- solucoes(Desc, cuidado(_,_,_,_,Desc,_), L), comprimento(L,R).



%Extensão do predicado que devolve o número de utentes de uma especialidade
%nr_ut_esp: Especialidade, Resultado -> {V,F}

nr_ut_esp(Esp,R) :- solucoes(Idu, (prestador(Idp,_,Esp,_), cuidado(_,Idu,Idp,_,_,_)), L), comprimento(L,R).

%Extensão do predicado que devolve a percentagem de utentes de uma especialidade
%percent_utentes_esp: Especialidade, Resultado -> {V,F}

percent_utentes_esp(Esp,R) :- nr_ut_esp(Esp,Res), total_utentes(X), R is (Res*100)/X.



%Extensão do predicado que devolve uma lista com os prestadores que não têm nenhum cuidado associado
%prest_sem_cuidados: Resultado -> {V,F}
prest_sem_cuidados(R) :- solucoes(Idp, prestador(Idp,_,_,_),L), solucoes(Idp, (prestador(Idp,_,_,_), cuidado(_,_,Idp,_,_,_)), L1),
				  pertence(L,L1,R1), prest(R1,R).

%Extensão do predicado que a partir de uma lista de id de prestadores devolve uma lista dos respetivos prestadores
%prest: Lista, ListaResultado -> {V,F}

prest([Id],R) :- solucoes(prestador(Id,N,E,Inst), prestador(Id,N,E,Inst), R).
prest([Id|Y],R) :- solucoes(prestador(Id,N,E,Inst), prestador(Id,N,E,Inst), L), prest(Y,R1), concat(L,R1,R).

%Extensão do predicado que verifca se um elemento pertence a uma lista
%pertence1: Elemento -> {V,F}

pertence1(X,[X|Y]).
pertence1(X,[Y|Z]) :- pertence1(X,Z).

% retorna lista dos elementos da primeira lista que nao existem na segunda lista
%pertence: Lista1, Lista2, ListaResultado -> {V,F}

pertence([],_,[]).
pertence([X|Y],Z,[X|R]) :- not(pertence1(X,Z)), pertence(Y,Z,R).
pertence([X|Y],Z,R) :- pertence1(X,Z), pertence(Y,Z,R).



% Identificar os utentes de uma instituição dando o id da instituição
% utentes_de_inst: IdPrest, Resultado -> {V,F}

utentes_de_instID(Idi,R) :- solucoes(utente(Id,N,Idd,M), (cuidado(_,Id,Idp,_,_,_), prestador(Idp,_,_,Idi), inst(Idi,_,_), utente(Id,N,Idd,M)), R).



%Extensão do predicado que devolve lista com o número de utentes de cada instituição
%nr_ut_inst: ListaResultado -> {V,F}
nr_ut_todas_inst(R) :- solucoes(Idi, inst(Idi,_,_), L), nr_ut_inst(L,R).

%Extensão do predicado que a partir de uma lista de ids de instituições devolve a lista com o número de utentes de cada uma
%nr_ut_inst: Lista, ListaResultado -> {V,F}
nr_ut_inst([],[]).
nr_ut_inst([Idi],[R]) :- utentes_de_instID(Idi,R1), comprimento(R1,R).
nr_ut_inst([Idi|Y],[R|L]) :- utentes_de_instID(Idi,R1), comprimento(R1,R), nr_ut_inst(Y,L).



%Extensão do predicado que verifica o cuidado mais caro da base de conhecimento
% cuidado_mais_caro: Resultado -> {V,F}

cuidado_mais_caro(R) :- solucoes(C, cuidado(_,_,_,_,_,C), L), maxLista(L,R1), solucoes((D,Idu,Idp,P,Desc,R1), cuidado(D,Idu,Idp,P,Desc,R1), R).

%Extensão do predicado que calcula o máximo de uma lista
% maxLista: Lista, Resultado -> {V,F}

maxLista([H],R):- R is H.
maxLista([X|L],R) :- maxLista(L,N), X>N, R is X.
maxLista([X|L],R) :- maxLista(L,N), X=<N, R is N.


%Extensão do predicado que ordena cuidados de uma instituição por preço
% cuidados_por_preco: ListaResultado -> {V,F}

cuidados_por_preco(Idi,R) :- solucoes((D,Idu,Idp,P,Desc,R1), (inst(Idi,_,_), prestador(Idp,_,_,Idi),cuidado(D,Idu,Idp,P,Desc,R1)), L), 
							 ordena(L,R).

%Extensão do predicado que ordena uma lista de cuidados
%ordena: Lista, ListaResultado -> {V,F}

ordena([],[]).
ordena([(A,B,C,D,E,X)|Y],R) :- ordena(Y,R1), iSort((A,B,C,D,E,X),R1,R).

%Extensão do predicado que insere um cuidado num lista ordenadamente
%iSort: Cuidado, Lista, ListaResultado -> {V,F}

iSort((A,B,C,D,E,X),[],[(A,B,C,D,E,X)]).
iSort((A,B,C,D,E,X),[(F,G,H,I,J,Y)|Z],[(A,B,C,D,E,X),(F,G,H,I,J,Y)|Z]) :- X=<Y.
iSort((A,B,C,D,E,X),[(F,G,H,I,J,Y)|Z],[(F,G,H,I,J,Y)|R]) :- X>Y, iSort((A,B,C,D,E,X),Z,R).



%Extensão do predicado que identifica cuidados de saúde por prioridade
% cuidados_prio: Prioridade, ListaResultado -> {V,F}

cuidados_prio(Prio,R) :- solucoes(cuidado(D,Idu,Idp,Prio,Desc,C), cuidado(D,Idu,Idp,Prio,Desc,C), R).



%Extensão do predicado que da o numero de cuidados com um certa prioridade
% nr_cuidado_prio: Prioridade, ListaResultado -> {V,F}

nr_cuidado_prio(Prio,R) :- solucoes(cuidado(D,Idu,Idp,Prio,Desc,C), cuidado(D,Idu,Idp,Prio,Desc,C), R1), comprimento(R1,R).


