%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Trabalho Parte 1 
% Declaracoes iniciais:
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- op(900,xfy,'::').

% - Definicoes iniciais:

 :- dynamic utente/4.
 :- dynamic medico/3.
 :- dynamic ato_medico/8.
 :- dynamic cuidado_prestado/4.
 %-------------------

%utente: id_ut, Nome, idade, morada -> {V,F}

%cuidado_prestado: id_serv, descricao, instituicao, cidade -> {V,F}

%ato_medico: id_medico,  dia, mes, hora, minutos, id_ut, id_serv, custo -> {V,F}

%medico: id_medico, nome, idade-> {V,F}


utente(12345,joaocarlos,20,rua-rosa). 
utente(67810,martaferreira,20,rua_barros).
utente(11123,carlalopes,20,ruateixeira).
utente(14156,pedrolopes,20,ruadocarmo).
utente(17189,joaomaria,20,avenidacentral).
utente(20212,ricardoabreu,20,ruasantaclara).
utente(23245,joanamateus,20,ruadapraia).
utente(26278,mariaana,20,ruamalmequer).
utente(28293,margaridaleao,20,avenidadapraca).
utente(31323,fredericasantos,20,rualuiscarlos).
utente(33345,paulogomes,20,ruadmario).
utente(51523,adrianosilva,20,ruaprimeiro).
utente(52534,sarafilipa,60,ruasantos).
utente(61626,carloslima,47,ruasegundo).
utente(71727,gabrielbarbosa,33,ruaterceiro).
utente(81828,ritasilva,56,ruaquarto).

medico(2345,josebeleza,50).
medico(1234,carlarodrigues,39).
medico(5678,pedroferreira,30).
medico(1001,dinisabreu,60).
medico(2002,carlossantos,45).
medico(3003,joanaazevedo,44).
medico(4004,martagomes,33).
medico(5005,saraabrantes,29).
medico(6006,afonsogomes,56).
medico(7007,filipesantos,60).
medico(8008,yarabraga,59).

cuidado_prestado(00001,cardiologia,hospital,braga).
cuidado_prestado(00002,enfermaria,hospital,braga).
cuidado_prestado(00003,psiquiatria,hospital,guimaraes).
cuidado_prestado(00004,podologia,centrodesaude,barcelos).
cuidado_prestado(00005,neurologia,hospital,viladoconde).
cuidado_prestado(00006,cardiologia,hospital,barcelos).
cuidado_prestado(00007,dermatologia,clinicaprivada,porto).
cuidado_prestado(00008,enfermaria,centrodesaude,barcelos).
cuidado_prestado(00009,oftalmologia,hospital,guimaraes).
cuidado_prestado(00010,fisioterapia,hospital,viladoconde).
cuidado_prestado(00011,ortopedia,clinicaprivada,barcelos).
cuidado_prestado(00012,ortopedia,hospital,porto).
cuidado_prestado(00013,enfermaria,centrodesaude,guimaraes).
cuidado_prestado(00014,clinicageral,hospital,braga).



ato_medico(4004,2,4,16,40,51523,00011,50).
ato_medico(2345,16,11,3,50,23245,00001,20).                      
ato_medico(5005,20,12,6,47,11123,00005,40).
ato_medico(3003,4,9,18,20,20212,00010,35).
ato_medico(3003,27,6,5,18,23245,00009,55).
ato_medico(2002,8,10,13,55,14156,00011,60).
ato_medico(2345,2,4,19,34,51523,00003,40).
ato_medico(3003,2,5,21,26,51523,00009,45).
ato_medico(1234,5,7,21,26,23245,00001,100).
ato_medico(5005,3,10,12,13,14156,00002,10).
ato_medico(1001,12,1,14,28,17189,00004,30).
ato_medico(2002,23,3,9,30,31323,00010,44).
ato_medico(2002,16,8,10,40,28293,00002,30).
ato_medico(1001,31,8,16,34,33345,00001,22).
ato_medico(1234,17,2,17,20,67810,00014,30).
ato_medico(8008,2,5,21,10,52534,00003,50).
ato_medico(7007,2,5,20,30,61626,00009,60).
ato_medico(2002,2,5,23,00,81828,00003,60).
ato_medico(6006,2,5,21,00,11123,00009,64).
ato_medico(4004,2,5,21,30,51523,00009,35).
ato_medico(4004,2,5,21,45,20212,00009,35).
ato_medico(1001,2,5,21,45,20212,00003,35).
ato_medico(4004,2,5,22,45,23245,00003,45).
ato_medico(2002,2,5,23,00,81828,00003,60).
ato_medico(3003,2,5,14,45,71727,00010,59).
ato_medico(2345,2,5,14,45,67810,00011,50).
ato_medico(4004,2,5,16,30,28293,00009,75).
ato_medico(3003,2,5,17,45,26278,00014,55).
ato_medico(5678,2,5,20,00,14156,00003,90).
ato_medico(3003,3,8,14,12,23245,00006,60).
ato_medico(5005,4,12,12,50,67810,00008,30).
ato_medico(6006,3,10,13,20,71727,00013,20).
ato_medico(2002,18,2,15,37,26278,00012,25).
ato_medico(8008,5,7,20,50,26278,00003,30).
ato_medico(1001,5,7,21,50,71727,00004,30).

%-----------------------------------6. Predicados Auxiliares----------------------------------------------------------

%--------6.1.Predicado nao--------------

%Predicado nao: Questao -> {V,F}
nao(Q):-
    Q,!,fail.
    nao(Q).
    
%--------6.2.Predicado naocomum--------------

%Predicado naocomum:  Lista1, Lista2, Resultado -> {V,F}
naocomum([],L2,[]).

naocomum([A|B],L2,NL) :- 
  pertence(A,L2), 
  naocomum(B,L2,NL).

naocomum([A|B],L2,[A|NL]) :-
 nao(pertence(A,L2)), 
 naocomum(B,L2,NL).


%--------6.3.Predicado somatorio-----------

%Predicado somatorio: Lista, Resultado -> {V,F}

somatorio([],0).
somatorio([X|L],R):-
        somatorio(L,R1), R is X+R1.

%-------6.4.Predicado comprimento-------------

%Predicado Comprimento: Lista, Comprimento -> {V,F}

comprimento( [],0 ).
comprimento( [X|L],N ) :-
        comprimento( L,N1 ),
                    N is N1+1.

%------6.5.Predicado eliminarelemento----------

%Predicado eliminarelemento: Elemento, Lista, Resultado -> {V,F}

eliminarelemento(E,[],[]).
eliminarelemento(E,[E|L],L ).
eliminarelemento(E,[Y|L],[Y|NL]) :- 
    E \== Y, 
    eliminarelemento(E,L,NL).

%--------6.7.Predicado eliminarrepetidos---------

%Predicado eliminarrepetidos: Lista, Resultado -> {V,F}

eliminarrepetidos([],[]).
eliminarrepetidos([X|Y],[X|L]) :- 
    eliminarelemento(X,Y,NL),
    eliminarrepetidos(NL,L).



%--------6.8.Predicado Soluções-----------------
%Predicado solucoes: Arquétipo, Teorema, Solucao -> {V,F}

    solucoes(T,Q,S):- 
        findall(T,Q,S).

%--------6.9.Predicado Pertence-----------------	
%Predicado pertence: Elemento, Lista -> {V,F}

pertence( E,[E|L] ).
pertence( E,[Y|L] ) :- E \== Y, pertence( E,L ).

%--------6.10.Predicado int_tempo-----------------

%Predicado intervalo de tempo: Mês1, Dia1, Mês2, Dia2, Mês3, Dia3  ->{V,F}

 int_tempo(M1,_,M2,_,M3,_) :-
    M1 < M3,
    M3 < M2.

 int_tempo(M1,D1,M2,_,M3,D3) :-
    M1 = M3,
    M3 < M2,
    D1 =< D3.

 int_tempo(M1,_,M2,D2,M3,D3) :-
    M1 < M3,
    M3 = M2,
    D3 =< D2.

 int_tempo(M1,D1,M2,D2,M3,D3) :-
    M1 = M3,
    M3 = M2,
    D1 =< D3,
    D3 =< D2.

%--------6.11.Predicado int_horas----------

%Predicado int_horas: Hora1, Minutos1, Hora2, Minutos2, Hora3, Minutos3 -> {V,F}

int_horas(H1,_,H2,_,H3,_):-
    H1 < H3,
    H3 < H2.

int_horas(H1,Mi1,H2,_,H3,Mi3):-
    H1 = H3,
    H3 < H2,
    Mi1 =< Mi3.

int_horas(H1,_,H2,Mi2,H3,Mi3):-
    H1 < H3,
    H3 = H2,
    Mi3 =< Mi2.

int_horas(H1,Mi1,H2,Mi2,H3,Mi3):-
    H1 = H3,
    H3 = H2,
    Mi1 =< Mi3,
    Mi3 =< Mi2.

%------------------7.Extensão de Predicado-----------------------


%---------7.1.Extensao do Predicado evolucao| Predicados testar e inserir-----------

%Extensao do predicado evolucao: Teorema -> {V,F}

evolucao(T):-solucoes(I,+T::I,Li),
            inserir(T),
            testar(Li).

%Predicado inserir: Teorema -> {V,F}

inserir(T):-assert(T).
inserir(T):-retract(T),!,fail.


%Predicado testar: Lista ->{V,F}
testar([]).
testar([R|LR]):-
        R,testar(LR).

%-------7.2.Extensao do Predicado remoção | Predicado remover----

%Extensao do predicado remocao: T -> {V,F}

remocao(T):- solucoes(I,-T::I,Li),
            remover(T),
            testar(Li).

%Extensao predicado remover: T -> {V,F}

remover(T):-retract(T).
remover(T):-assert(T),!,fail.


%-------------7.3.Extensão de Predicados lista------------

%------7.3.1.Extensão do Predicado lista_ute_int-------- 
%Extensão do predicado lista_ute_int: Mês1, Dia1, Mês2, Dia2, Resultado  -> {V,F}

    lista_ute_int(M1,D1,M2,D2,R):-
        solucoes((IU,NU),(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),utente(IU,NU,ID,M),int_tempo(M1,D1,M2,D2,M3,D3)),Y),eliminarrepetidos(Y,R).

%-------7.3.2.Extensão do Predicado lista_medico_int-------
%Extensao do predicado lista_medico_int: Mês1, Dia1, Mês2, Dia2, Resultado  ->{V,F} 

    lista_medico_int(M1,D1,M2,D2,R):-
        solucoes((IM,NM),(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),medico(IM,NM,ID), int_tempo(M1,D1,M2,D2,M3,D3)),Y),eliminarrepetidos(Y,R).


%--------7.3.3.Extensao do Predicado lista_servico_int-------
%Extensão do predicado lista_servico_int: Mês1, Dia1, Mês2, Dia2, Identificação Utente, Resultado ->{V,F}
  lista_servico_int(M1,D1,M2,D2,IU,R):-
solucoes((IS,D),(ato_medico(M,D3,M3,H3,Mi3,IU,IS,_),
cuidado_prestado(IS,D,_,_), int_tempo(M1,D1,M2,D2,M3,D3)),Y), eliminarrepetidos(Y,R).


%------7.3.4.Extensão do Predicado lista_ute_servico--------------
%Extensao do predicado lista_ute_servico: Mês1, Dia1, Mês2, Dia2, Descrição do serviço, Resultado  ->{V,F}

    lista_ute_servico(M1,D1,M2,D2,D,R):-
        solucoes((IU,NU),(cuidado_prestado(IS,D,I,CI),ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),int_tempo(M1,D1,M2,D2,M3,D3), utente(IU,NU,ID,M)),Y),eliminarrepetidos(Y,R).


%---------7.3.5.Extensão do Predicado lista_inst_cuidados------
%Extensao do predicado lista_inst_cuidados: D, R  ->{V,F}
    lista_inst_cuidados(D,R):-
        solucoes((I,C),cuidado_prestado(IS,D,I,C),Y), eliminarrepetidos(Y,R).



%-------7.3.6.Extensão do Predicado lista_cuidados_inst_cidade -----------

%Extensao do predicado lista_cuidados_inst_cidade: Instituição, Cidade, Resultado  ->{V,F} 

    lista_cuidados_inst_cidade(I,C,R):- 
        solucoes((IS,D),cuidado_prestado(IS,D,I,C),Y), eliminarrepetidos(Y,R).


%---------7.3.7.Extensao do Predicado l_atos_med_utente_inst_servico------

%Extensao do predicado l_atos_med_utente_inst_servico: Identificação do Utente, Descrição do Serviço, Intituição, Resultado  ->{V,F}

l_atos_med_utente_inst_servico(IU,D,I,R):-
    solucoes((D3,M3,H3,Mi3,C,NM),(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,CU),
    cuidado_prestado(IS,D,I,C),medico(IM,NM,IDA)),Y), eliminarrepetidos(Y,R).


%-----------7.3.8.Extensão do Predicado l_inst_servicos_utente------- 
%Extensao do predicado l_inst_servicos_utente: Identificação do Utente, Resultado  ->{V,F}

   l_inst_servicos_utente(IU,R):- 
        solucoes((D,I,C),(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,CU), cuidado_prestado(IS,D,I,C)),Y), eliminarrepetidos(Y,R).


%-----------7.3.9.Extensao do Predicado l_custo_utente--------------

%Extensao do predicado l_custo_utente: Identificação do utente, Resultado -> {V,F} 
   l_custo_utente(IU,R):-
        solucoes(C,ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),Y), somatorio(Y,R).


%-----------7.3.10.Extensão do Predicado l_custo_servico------------

%Extensao do predicado l_custo_servico: Descrição do serviço, Resposta -> {V,F} 
    l_custo_servico(D,R):-
        solucoes(C,(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),cuidado_prestado(IS,D,I,CI)),Y), somatorio(Y,R).

%----------7.3.11.Extensão do Predicado l_custo_instituicao------

%Extensao do predicado l_custo_instituicao: Instituicao, Resposta -> {V,F} 
    l_custo_instituicao(I,CI,R):-
        solucoes(C,(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),cuidado_prestado(IS,D,I,CI)),Y), somatorio(Y,R).


%-----------7.3.12.Extensão do Predicado l_custo_data-------------

%Extensao do predicado l_custo_data: Mês1, Dia 1, Mês2, Dia 2, Resposta -> {V,F} 
    l_custo_data(M1,D1,M2,D2,R):-
        solucoes(C,(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),int_tempo(M1,D1,M2,D2,M3,D3)),Y), somatorio(Y,R).


%----------7.3.13.Extensão do Predicado l_custo__d_i_s_u----------- 

%Extensao do predicado l_custo_d_i_s_u: Mês1, Dia 1, Mês2, Dia 2, Instituição, Descrição do Serviço, Identificação do Utente, Resposta -> {V,F}

l_custo_d_i_s_u(M1,D1,M2,D2,I,CI,D,IU,R):- 
        solucoes(C,(ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),cuidado_prestado(IS,D,I,CI),int_tempo(M1,D1,M2,D2,M3,D3)),Y), somatorio(Y,R).


%-----------7.3.14.Extensão do Predicado l_ute_serv_data_inthora--------

%Extensao predicado l_ute_serv_data_inthora:Mês1, Dia1, Mês2, Dia2,Hora1,Minutos1,Hora2,Minutos2,Descrição Cuidado,Resultado ->{V,F}  


  l_ute_serv_data_inthoras(M1,D1,M2,D2,H1,Mi1,H2,Mi2,D,I,CI,R):-
        solucoes((IU,NU),(cuidado_prestado(IS,D,I,CI),ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),int_tempo(M1,D1,M2,D2,M3,D3), int_horas(H1,Mi1,H2,Mi2,H3,Mi3), utente(IU,NU,ID,M)),Y),eliminarrepetidos(Y,R).

%-----------7.3.15.Extensao do Predicado l_med_serv_data_inthora--------
%Extensão predicado l_med_serv_data_inthora:Mês1, Dia1, Mês2, Dia2,Hora1,Minutos1,Hora2,Minutos2,Descrição Cuidado,Resultado ->{V,F}  


  l_med_serv_data_inthoras(M1,D1,M2,D2,H1,Mi1,H2,Mi2,D,I,CI,R):-
        solucoes((IM,NM),(cuidado_prestado(IS,D,I,CI),ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),int_tempo(M1,D1,M2,D2,M3,D3), int_horas(H1,Mi1,H2,Mi2,H3,Mi3), medico(IM,NM,ID)),Y),eliminarrepetidos(Y,R).



%----------7.3.16.Extensão do predicado l_med_inst_cidade-----------------

%Extensão do predicado l_med_inst_cidade: Instituicao, Cidade,Resultado ->{V,F}

l_med_inst_cidade(I,C,R):-
    solucoes((IM,NM), (ato_medico(IM,D3,M3,H3,Mi3,IU,IS,CU),medico(IM,NM,ID),cuidado_prestado(IS,D,I,C)),Y), eliminarrepetidos(Y,R).


%--------7.3.17.Extensão do Predicado l_med_nao_int----------- 

%Extensão do predicado l_med_nao_int: Mês1, Dia1, Mês2, Dia2, Descrição, Instituição, Cidade, Resultado -> {V,F}
    l_med_nao_int(M1,D1,M2,D2,D,I,CI,R):-
        (solucoes((IM,NM),(ato_medico(IM,_,_,_,_,IU,IS,C),medico(IM,NM,ID),cuidado_prestado(IS,D,I,CI)),Y)),
        eliminarrepetidos(Y,T),
        solucoes((IM,NM),(ato_medico(IM,D3,M3,_,_,IU,IS,C),medico(IM,NM,ID),int_tempo(M1,D1,M2,D2,M3,D3),cuidado_prestado(IS,D,I,CI)),P),
        naocomum(T,P,R).

	


%------------- 8. Invariantes----------------------------------
%------------- 8.1 Invariantes de Inserção --------------------


%------------- 8.1.1.Invariante para inserir um utente---------- 
+utente(IU,_,_,_) :: (solucoes((IU,NU,I,M),utente(IU,NU,I,M),S),
                  comprimento( S,C ), C == 1
                  ).


%--------------8.1.2.Invariante inserir um medico--------- 

+medico(IM,_,_) :: (solucoes((IM,NM,I),(medico(IM,NM,I)),S ),
                  comprimento( S,C ), C == 1
                  ).


%-------------8.1.3.Invariante para inserir um ato_medico-----

+ato_medico(IM,D3,M3,H3,Mi3,_,_,_) :: (solucoes((IM,IU,M3,D3,H3,Mi3),ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),S ),
                  comprimento(S,C), C ==1).

+ato_medico(_,D3,M3,H3,Mi3,IU,_,_) :: (solucoes((IM,IU,M3,D3,H3,Mi3),ato_medico(IM,D3,M3,H3,Mi3,IU,IS,C),S ),
                  comprimento(S,C), C ==1).


%------------8.1.4.Invariante para inserir cuidado_prestado-------
+cuidado_prestado(_,D,I,C) :: (solucoes((IS,D),cuidado_prestado(IS,D,I,C),S ),
                  comprimento(S,C), C ==1).


+cuidado_prestado(IS,_,_,_) :: (solucoes((IS,D),cuidado_prestado(IS,D,I,C),S ),
                  comprimento(S,C), C ==1).


%------------- 8.2 Invariantes de Remoção --------------------

%------------8.2.1.Invariante para remover utente-------- 
-utente(IU,NU,I,M) :: (solucoes((IU),utente(IU,NU,I,M),S),
                  comprimento( S,C ), C == 0
                  ).


%-----------8.2.2.Invariante para remover um medico--------- 
-medico(IM,NM,I) :: (solucoes((IM),(medico(IM,NM,I)),S ),
                  comprimento( S,C ), C == 0
                  ).


		  
%------------8.2.3.Invariante para remover um ato medico----------

-ato_medico(IM,D3,M3,H3,Mi3,IU,IS,CU) :: (solucoes((IM,IU,M3,D3,H3,Mi3),ato_medico(IM,D3,M3,H3,Mi3,IU,IS,CU),S ),
                  comprimento(S,C), C ==0
                  ).



%------------8.2.4.Invariante para remover cuidado_prestado------------
		  
-cuidado_prestado(IS,D,I,CI) :: (solucoes((IS,D),cuidado_prestado(IM,IU,IS,CI),S ),
                  comprimento(S,C), C == 0
                  ).





