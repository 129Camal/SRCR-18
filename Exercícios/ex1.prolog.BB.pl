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
:- dynamic cuidado/6.

%-------------------------------------------------------------------------------------------
%Extensão do predicado utente: IdUT, Nome, Idade, Morada -> {V,F}

utente(1,'Pascoal',38,'Rua Limpa').
utente(2,'Zeca',20,'Rua da Capa').
utente(3,'Anibal',59,'Rua do Gota').
utente(4,'Maria',42,'Rua dos Peoes').
utente(5,'Carlota',22,'Rua do Speedy').
utente(6,'Brito',65,'Rua do Colombo').
utente(7,'Micaela',8,'Rua dos Olivais').
utente(8,'Julio',36,'Rua do Campo').
utente(9,'Dinis',48,'Rua da Cruz').
utente(10,'Rita',88,'Rua das Flores').
utente(11,'Mariana',6,'Rua do Carmo').
utente(12,'Sergio',26,'Rua dos Limoes').
utente(13,'Lucifer',14,'Rua do Palacio').
utente(14,'Miguel',49,'Rua do Pinheiro').
utente(15,'Joana',70,'Rua da Maria').
% -------------------------------------------------------------------------------------------
%Extensão do predicado prestador: IdPrest, Nome, Especialidade, IdInstituição -> {V,F}

prestador(1,'Ze','Otorrino',1). 
prestador(2,'Andreia','Dentaria',1).
prestador(3,'Guilherme','Dermatologia',1).
prestador(4,'Manuel','Oncologia',2).
prestador(5,'Elso','Ortopedia',3).
prestador(6,'Bino','Ginecologia', 3).
prestador(7,'Telmo','Radiologia',2).
prestador(8,'Miquelina','Cardiologia',4).
prestador(9,'Armando','Neurologia',4).
prestador(10,'Firmino','Endocrinologia',5).
prestador(11,'Horacio','Otorrino',5).
prestador(12,'Carlos','Dentaria',1).
prestador(13,'Maria','Dermatologia',3).
prestador(14,'Narcisa','Ginecologia',2).
prestador(15,'Adelaide','Psiquiatria',4).
prestador(16,'Teresa','Nutricao',1).
prestador(17,'Ambrosio','Podologia',5).
prestador(18,'Nuno','Radiologia',2).
prestador(19,'Marta','Cardiologia',4).
prestador(20,'David','Oncologia',5).


% -------------------------------------------------------------------------------------------
%Extensão do predicado cuidado: Data, IdUt, IdPrest, Prioridade, Descrição, Custo -> {V,F}

cuidado('2018-1-1',1,1,'Media','Amigdalite',10).
cuidado('2018-1-1',2,2,'Alta','Carie',26).
cuidado('2018-1-1',3,3,'Baixa','Acne',15).
cuidado('2018-1-2',4,4,'Alta','Cancro',32).
cuidado('2018-1-2',5,5,'Media','Fratura do pulso',19).
cuidado('2018-1-3',4,6,'Baixa','Papa Nicolau', 100).
cuidado('2018-1-3',7,20,'Alta','Cancro da Mama',20).
cuidado('2018-1-3',8,19,'Alta','Enfarte',198).
cuidado('2018-1-4',9,18,'Baixa','Tirar Raio-X',3).
cuidado('2018-1-4',10,17,'Media','Unha encravada',37).
cuidado('2018-1-5',11,16,'Baixa','Plano Alimentar',12).
cuidado('2018-2-5',12,15,'Media','Consulta rotina',90).
cuidado('2018-2-6',13,14,'Media','Ecografia aos ovarios',58).
cuidado('2018-2-6',14,13,'Alta','Urticaria',5).
cuidado('2018-2-7',15,12,'Baixa','Por aparelho',2000).

% -------------------------------------------------------------------------------------------
%Extensão do predicado instituição: IdInst, Nome, Cidade -> {V,F}
inst(1,'Hospital Privado de Braga', 'Braga').
inst(2,'IPO','Porto').
inst(3,'Hospital S.Joao','Porto').
inst(4,'Hospital de Felgueiras','Felgueiras').
inst(5,'Hospital dos Bonecos','Lisboa').


% -------------------------------------------------------------------------------------------
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
			  inserir(T),
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



% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para a instituição

+inst(Id,Nome,Cid) :: (solucoes(Id, inst(Id,Nome,Cid),S), comprimento(S,N), N ==1).

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
% TESTAR A VER SE SE USA EVOLUÇAO AQUI

utenteID(ID,R) :- solucoes((ID,N,I,M), utente(ID,N,I,M), R).
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

% Identificar os utentes de uma instituição
% utentes_de_inst: IdPrest, Resultado -> {V,F}

utentes_de_inst(NomeI,R) :- solucoes(utente(Id,N,Idd,M), (cuidado(_,Id,Idp,_,_,_), prestador(Idp,_,_,Idinst), inst(Idinst,NomeI,_), utente(Id,N,Idd,M)), R).


% 7------------------------------------------------------------------------------------------
% Identificar cuidados de saúde realizados por utente
% cuidados_por_utente: IdUt, ListaResultado -> {V,F}

cuidados_por_utente(Idu,R) :- solucoes(cuidado(D,Idu,Idp,P,Desc,Custo), cuidado(D,Idu,Idp,P,Desc,Custo), R).


% Identificar cuidados de saúde realizados por instituição
% cuidados_por_utente: IdUt, ListaResultado -> {V,F}


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

% predicado que dá o número total de utentes
% total_utentes: Resultado -> {V,F}

total_utentes(R) :- solucoes(Id, utente(Id,_,_,_), R).



% predicado que dá o número total de prestadores
% total_prest : R -> {V,F}

total_prestadores(R) :- solucoes(Idp, prestador(Idp,_,_,_), R).



% predicado que dá o número total de cuidados
% total_cuidados(R) :- solucoes(cuidado(D, cuidado(D,_,_,_,_,_), R).


% VER ISTO, SE NAO SE CONSEGUIR TIRA-SE 

% predicado que verifica os prestadores saqueadores -- INSERIR PRESTADORES MAS NAO LHE ATRIBUIR CUIDADOS
% prestador_sem_cuidados : ListaResultado -> {V,F}

% prestador_sem_cuidados(R) :- comprimento(cuidados_por_prest(Idp,R1), R2), R2 == 0, solucoes(prestador(Idp,N,E,I), prestador(Idp,N,E,I), R).


% VER QUAL É O PROBLEMA
% predicado que verifica a especialidade com mais utentes
% esp_mais_utentes: Resultado -> {V,F}

% esp_mais_utentes(R) TUPLOS. 

%Extensão do predicado que a partir de uma lista de especialidades cria uma lista de listas de utentes de cada especialidade
% coisa: Lista, ListaResultado -> {V,F}
coisa([],[]).
coisa([Esp],R) :- coisa(utentes_de_esp(Esp,R),R).
coisa([Esp|Y],[L|R1]) :- utentes_de_esp(Esp,L), coisa(Y,R1).

%Extensão do predicado que a partir de uma lista de listas da outra lista com o comprimento de cada uma das listas 
% comp : Lista, ListaResultado -> {V,F}
comp([],[R]) :- comprimento([],R).
comp([X],[R]) :- comprimento(X,R).
comp([X|Y],[L|R]) :- comprimento(X,L), comp(Y,R).

%Extensão do predicado que calcula o máximo de uma lista
% maxLista: Lista, Resultado -> {V,F}

maxLista([H],R):- R is H.
maxLista([X|L],R) :- maxLista(L,N), X>N, R is X.
maxLista([X|L],R) :- maxLista(L,N), X=<N, R is N.


%Extensão do predicado que verifica a instituição com mais utentes 
% fazer : predicado extra findall(utentes de 1 inst) e comprimento e retornar tuplo
%		: + outro predicado recursivo que chama o anterior p todas inst(ja existe este em cima)

%Extensão do predicado que verifica o cuidado mais caro da base de conhecimento
% cuidado_mais_caro: Resultado -> {V,F}

cuidado_mais_caro(R) :- solucoes(C, cuidado(_,_,_,_,_,C), L), maxLista(L,R1), solucoes((D,Idu,Idp,P,Desc,R1), cuidado(D,Idu,Idp,P,Desc,R1), R).


% QUASE QUASE 
%Extensão do predicado que ordena cuidados de uma instituição por preço
% cuidados_por_preco: ListaResultado -> {V,F}

cuidados_por_preco(Idi,R) :- solucoes(((D,Idu,Idp,P,Desc), R1), 
									  (inst(Idi,_,_), prestador(Idp,_,_,Idi),cuidado(D,Idu,Idp,P,Desc,R1)), L), 
							 ordena(L,R).

ordena([],[]).
ordena([(A,X)],[(B,X)]).
ordena([(A,X)|Y],R) :- ordena(Y,L), inSort((A,X),L,R).

inSort((A,X),[],[(A,X)]).
inSort((A,X),[(B,Y)|Z],[(A,X),(B,Y)|Z]) :- X=<Y.
inSort((A,X),[(B,Y)|Z],[(B,Y)|R]) :- X>Y, inSort((A,X),Z,R).

%Extensão do predicado que identifica cuidados de saúde por prioridade 

%Extensão do predicado que da o numero de cuidados com um certa prioridade 







