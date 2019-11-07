:- dynamic hora/1.
:- dynamic matricula/1.
:- dynamic vuelo/1.
:- dynamic solicitud/1.
:- dynamic aeronave/2.
:- dynamic direccion/1.


split(L,Result) :-
    split_string(L,"\s","\s",Result),
    write(Result),
    nl.

del(X, [X|Xs], Xs).
del(X, [Y|Ys], [Y|Zs]):-
    del(X, Ys, Zs).

miembro(X,[X|_]).
miembro(X,[_|Y]):-
    miembro(X,Y).

direccion_auxiliar("Este-Oeste").
direccion_auxiliar("Oeste-Este").
direccion_aux(A,L):-
    miembro(A,L),
    direccion(A).



%PENDIENTE REVISAR SI DEBEN GUARDAR SOLO EL NUMERO
vuelo_aux(L,L3):-
   (miembro("Vuelo",L)->
   (   nextto("Vuelo",Y,L)->
   string_concat("Vuelo ",Y,Result),
       write(Result),
       nl,
       asserta(vuelo(Result)),
       del("Vuelo",L,L2),
       del(Y,L2,L3)
   );
   false
   ).

matricula_aux(L,L3):-
   (miembro("Matricula",L)->
   (   nextto("Matricula",Y,L)->
   string_concat("Matricula ",Y,Result),
       write(Result),
       nl,
       asserta(matricula(Result)),
       del("Matricula",L,L2),
       del(Y,L2,L3)
   );
   false
   ).

saludo("Hola, soy MayCEy. ¿En que puedo ayudarle?").
despedida("Es un gusto ayudar. Que tenga un buen día.").
despegue("Puede despegar").
aterrizaje("Puede aterrizar").

aeronave_aux("Cessna", pequena).
aeronave_aux("Beechcraft", pequena).
aeronave_aux("Embraer-Phenom", pequena).
aeronave_aux("Boeing-717", mediana).
aeronave_aux("Embraer-190", mediana).
aeronave_aux("Airbus-A220", mediana).
aeronave_aux("Boeing-747", grande).
aeronave_aux("Airbus-A340", grande).
aeronave_aux("Airbus-A380", grande).
aeronave_auxiliar(L,L3):-
     (miembro("Aeronave",L)->
   (   nextto("Aeronave",Y,L)->
   string_concat("Aeronave ",Y,Result),
       write(Result),
       nl,
       asserta(aeronave(Result,mediana)),
       del("Aeronave",L,L2),
       del(Y,L2,L3)
   );
   false
   ).

aeronave_auxiliar2(A,B,L):-
    miembro(A,L),
    aeronave_aux(A,B).

emergencia("Mayday").
emergencia("inmediato").
emergencia("Emergencia").
emergencia("emergencia").
emergencia("SOS").
emergencia_aux(A,L):-
    miembro(A,L),
    emergencia(A).

emergencia_auxiliar("perdida de motor","llamaremos a los bomberos de inmediato\n").
emergencia_auxiliar("parto en medio vuelo","llamaremos a un médico de inmediato\n").
emergencia_auxiliar("paro cardiaco de pasajero","llamaremos a un médico de inmediato\n").
emergencia_auxiliar("secuestro","llamaremos al OIJ de inmediato\n").
emergencia_auxiliar(_,"lo pondremos en contacto con la línea de emergencia_auxiliar\n").

saludo_aux("Hola").
saludo_aux("hola").
saludo_aux("MayCEy").
saludo_aux("Buenas").
saludo_aux("Buenos").
saludo_aux("Saludos").
saludo_aux(Sal,L):-
    miembro(Sal,L),
    saludo_aux(Sal).

despedida_aux("Adios").
despedida_aux("adios").
despedida_aux("Gracias").
despedida_aux("gracias").
despedida_aux("agradezco").
despedida_aux(Des,L):-
    miembro(Des,L),
    despedida_aux(Des).

despegue_aux("despegar").
despegue_aux("despegue").
despegue_aux(A,L):-
    miembro(A,L),
    despegue_aux(A).

aterrizaje_aux("aterrizar").
aterrizaje_aux("aterrizaje").
aterrizaje_aux(A,L):-
    miembro(A,L),
    aterrizaje_aux(A).

hora_aux(A,L):-
    miembro(A,L),
    hora_(A).

hora_(X):-
    atom_chars(X,Chars),
    miembro(:,Chars).

analizar(L):-
    (saludo_aux(X,L)->
    write(X),
    nl,
    saludo(Y),
    write(Y),
    nl;
    del(X,L,A),
    false,
    analizar(A)
    ).

% HAY QUE HACER RETRACT DE RESERVACION??? (Creo que no, para guardar las
% reservaciones)
analizar(L):-
    (despedida_aux(X,L)->
    write(X),
    nl,
    despedida(Y),
    write(Y),
    nl,
    retractall(vuelo(_)),
    retractall(matricula(_)),
    retractall(solicitud(_)),
    retractall(aeronave(_,_)),
    retractall(direccion(_)),
    retractall(hora(_)),
    write("-----Corte-----\n");
    false
    ).

analizar(L):-
    (despegue_aux(X,L)->
    write(X),
    nl,
    despegue(Y),
    write(Y),
    nl,
    asserta(solicitud(despegue))
    ),
    !,
    del(X,L,A),
    write(A),
    nl,
    analizar(A).


analizar(L):-
    (aterrizaje_aux(X,L)->
    write(X),
    nl,
    aterrizaje(Y),
    write(Y),
    nl,
    asserta(solicitud(aterrizaje))
    ),
    !,
    del(X,L,A),
    analizar(A).

analizar(L):-
    (hora_aux(X,L)->
    write(X),
    nl,
    asserta(hora(X))
    ),
    !,
    del(X,L,A),
    analizar(A).

analizar(L):-
    (direccion_aux(X,L)->
    write(X),
    nl,
    asserta(direccion(X))
    ),
    !,
    del(X,L,A),
    analizar(A).

analizar(L):-
    (emergencia_aux(X,L)->
    write(X),
    nl,
    casoEmergencia
    ),
    !,
    del(X,L,A),
    analizar(A).

analizar(L):-
    (   vuelo_aux(L,Lx)->
    analizar(Lx)
    ),
    !,
    analizar(L).

analizar(L):-
    (   matricula_aux(L,Lx)->
    analizar(Lx)
    ),
    !,
    analizar(L).

analizar(L):-
    (aeronave_auxiliar2(A,B,L)->
    write(A),
    nl,
    asserta(aeronave(A,B))
    ),
    !,
    del(A,L,L2),
    write(L2),
    nl,
    analizar(L2).



analizar(L):-
    (   aeronave_auxiliar(L,Lx)->
    analizar(Lx)
    ),
    !,
    analizar(L).



analizar(L):-
    write("No le he comprendido, repita por favor"),
    nl.


io:-
    read(L),
    nl,
    split(L,Result),
    analizar(Result),
    read(L),
    nl,
    split(L,Result),
    analizar(Result),
    cicloPrograma.

cicloPrograma:-
    (   datos()->
    read(L),
    nl,
    split(L,Result),
    analizar(Result),
    cicloPrograma;
    asignar()
    ).



% Hechos de reserva, usan una string con la pista y un int con l hora en
% formato militar
reservado("P1",700).
reservado("P2-1",700).
reservado("P2-2",700).
reservado("P3",705).

% Regla para asginar una pista en caso de emergencia, se busca una
% disponible lo más cercana posible y si no se encuentra se asigna la P1
asignarEmergencia("P1",Hora):-not(reservado("P1",Hora)),!.
asignarEmergencia("P2-1",Hora):-not(reservado("P2-1",Hora)),!.
asignarEmergencia("P2-2",Hora):-not(reservado("P2-2",Hora)),!.
asignarEmergencia("P3",Hora):-not(reservado("P3",Hora)),!.
asignarEmergencia("P1",_).


%Regla para ejecutar el ciclo de una emergencia
casoEmergencia:-
writeln("Por favor especifique de forma puntual su emergencia"),
read(Emergencia),
emergencia_auxiliar(Emergencia,Medida),
writeln("Por favor identifiquese"),
read(ID),
asignarEmergencia(Pista,700),
write("Su pista asignada es "),
write(Pista),
write(" y "),
write(Medida),
writeln("").

% General
datos() :- not(solicitud(_)), write("Quiere despegar o aterrizar?\n"), read(Y), assertz(solicitud(Y)), datos().
datos() :- not(aeronave(_,_)), write("Cual es su aeronave?\n"), read(Y), aeronave_aux(Y, Peso), assertz(aeronave(Y, Peso)), datos().
% Si se da una aeronave inexistente, se asume que es mediana
datos() :- not(aeronave(_,_)), write("Cual es su aeronave?\n"), read(Y), not(aeronave_aux(Y, _)), assertz(aeronave(Y, mediana)), datos().
datos() :- not(identificacion(_)), write("Cual es su matricula?\n"), read(Y), assertz(identificacion(Y)), datos().
datos() :- not(direccion(_)), write("En que direccion se encuentra?\n"), read(Y), assertz(direccion(Y)), datos().

% Despegar
datos() :- not(vuelo(_)), solicitud(despegar), write("Cual es su numero de vuelo?\n"), read(Y), assertz(vuelo(Y)), datos().
datos() :- not(hora(_)), solicitud(despegar), write("A que hora planea realizar el aterrizaje?\n"), read(Y), assertz(hora(Y)), datos().

datos().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Determinacion de pista y hora %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Para aeronaves pequenas
/* la pista P1 no discrimina en direccion */
asignar() :- solicitud(despegar), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p1, Hora)),
            assertz(reservacion(X, p1, Hora)), write("Se le a asignado la pista P1 para despegar en la hora "), write(Hora), write(".\n").

asignar() :- solicitud(aterrizar), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p1, Hora)),
            assertz(reservacion(X, p1, Hora)), write("Puede despegar en la pista P1, tiene 5 minutos desde las  "), write(Hora), write(" tiempo actual.\n").

asignar() :- solicitud(despegar), direccion("Este-Oeste"), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("P1 esta ocupada. Se le a asignado la pista P2-1 para despegar en la hora "), write(Hora), write(".\n").

asignar() :- solicitud(aterrizar), direccion("Este-Oeste"), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("P1 esta ocupada. Puede despegar en la pista P2-1, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n").

asignar() :- solicitud(despegar), direccion("Oeste-Este"), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("P1 esta ocupada. Se le a asignado la pista P2-2 para despegar en la hora "), write(Hora), write(".\n").

asignar() :- solicitud(aterrizar), direccion("Oeste-Este"), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("P2-2 esta ocupada. Puede despegar en la pista P2-2, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n").

asignar() :- solicitud(despegar), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P1 esta ocupada. Se le a asignado la pista P3 para despegar en la hora "), write(Hora), write(".\n").


asignar() :- solicitud(aterrizar), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P1 esta ocupada. Puede despegar en la pista P3, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n").

% Para aeronavas medianas
asignar() :- solicitud(despegar), direccion("Este-Oeste"), aeronave(X, mediana),
            hora(Hora), not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("Se le a asignado la pista P2-1 para despegar en la hora "), write(Hora), write(".\n").

asignar() :- solicitud(aterrizar), direccion("Este-Oeste"), aeronave(X, mediana),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("Puede despegar en la pista P2-1, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n").

asignar() :- solicitud(despegar), direccion("Oeste-Este"), aeronave(X, mediana),
            hora(Hora), not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("Se le a asignado la pista P2-2 para despegar en la hora "), write(Hora), write(".\n").

asignar() :- solicitud(aterrizar), direccion("Oeste-Este"), aeronave(X, mediana),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("Puede despegar en la pista P2-2, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n").

asignar() :- solicitud(despegar), aeronave(X, mediana),
            hora(Hora), not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P2 esta ocupada. Se le a asignado la pista P3 para despegar en la hora "), write(Hora), write(".\n").

asignar() :- solicitud(aterrizar), aeronave(X, mediana),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P2 esta ocupada. Puede despegar en la pista P3, tiene 5 minutos desde las  "), write(Hora), write(" tiempo actual.\n").

% Para aeronavas grandes
asignar() :- solicitud(despegar), aeronave(X, grande),
            info(hora, Hora), not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("Se le a asignado la pista P3 para despegar en la hora "), write(Hora), write(".\n").

asignar() :- solicitud(aterrizar), aeronave(X, grande),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("Puede despegar en la pista P3, tiene 5 minutos desde las  "), write(Hora), write(" tiempo actual.\n").


asignar() :- write("Lo sentimos ninguna pista se encuentra disponible en este momento, por favor intente en otro momento.\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Asignacion de reservacion %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic reservacion/3.

/*
reservacion(aeronave, pista, hora).
*/
