:- dynamic hora/1.
:- dynamic matricula/1.
:- dynamic vuelo/1.
:- dynamic solicitud/1.
:- dynamic aeronave/2.
:- dynamic direccion/1.


% General
datos() :- not(solicitud(_)), write("Quiere despegar o aterrizar?\n").
datos() :- not(aeronave(_,_)), write("Cual es su aeronave?\n").
datos() :- not(matricula(_)), write("Cual es su matricula?\n").
datos() :- not(direccion(_)), write("En que direccion se encuentra?\n").

% Despegar
datos() :- not(vuelo(_)), solicitud(despegar), write("Cual es su numero de vuelo?\n").
datos() :- not(hora(_)), solicitud(despegar), write("A que hora planea realizar el aterrizaje?\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Determinacion de pista y hora %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Para aeronaves pequenas
/* la pista P1 no discrimina en direccion */
asignar() :- solicitud(despegar), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p1, Hora)),
            assertz(reservacion(X, p1, Hora)), write("Se le ha asignado la pista P1 para despegar en la hora "), write(Hora), write(".\n"),!.

asignar() :- solicitud(aterrizar), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p1, Hora)),
            assertz(reservacion(X, p1, Hora)), write("Puede despegar en la pista P1, tiene 5 minutos desde las  "), write(Hora), write(" tiempo actual.\n"),!.

asignar() :- solicitud(despegar), direccion("Este-Oeste"), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("P1 esta ocupada. Se le a asignado la pista P2-1 para despegar en la hora "), write(Hora), write(".\n"),!.

asignar() :- solicitud(aterrizar), direccion("Este-Oeste"), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("P1 esta ocupada. Puede despegar en la pista P2-1, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n"),!.

asignar() :- solicitud(despegar), direccion("Oeste-Este"), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("P1 esta ocupada. Se le a asignado la pista P2-2 para despegar en la hora "), write(Hora), write(".\n"),!.

asignar() :- solicitud(aterrizar), direccion("Oeste-Este"), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("P2-2 esta ocupada. Puede despegar en la pista P2-2, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n").!.

asignar() :- solicitud(despegar), aeronave(X, pequena),
            hora(Hora), not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P1 esta ocupada. Se le a asignado la pista P3 para despegar en la hora "), write(Hora), write(".\n"),!.


asignar() :- solicitud(aterrizar), aeronave(X, pequena),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P1 esta ocupada. Puede despegar en la pista P3, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n"),!.

% Para aeronavas medianas
asignar() :- solicitud(despegar), direccion("Este-Oeste"), aeronave(X, mediana),
            hora(Hora), not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("Se le a asignado la pista P2-1 para despegar en la hora "), write(Hora), write(".\n"),!.

asignar() :- solicitud(aterrizar), direccion("Este-Oeste"), aeronave(X, mediana),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-1, Hora)),
            assertz(reservacion(X, p2-1, Hora)), write("Puede despegar en la pista P2-1, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n"),!.

asignar() :- solicitud(despegar), direccion("Oeste-Este"), aeronave(X, mediana),
            hora(Hora), not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("Se le a asignado la pista P2-2 para despegar en la hora "), write(Hora), write(".\n"),!.

asignar() :- solicitud(aterrizar), direccion("Oeste-Este"), aeronave(X, mediana),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p2-2, Hora)),
            assertz(reservacion(X, p2-2, Hora)), write("Puede despegar en la pista P2-2, tiene 5 minutos desde las  "), write(Hora),  write(" tiempo actual.\n"),!.

asignar() :- solicitud(despegar), aeronave(X, mediana),
            hora(Hora), not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P2 esta ocupada. Se le a asignado la pista P3 para despegar en la hora "), write(Hora), write(".\n"),!.

asignar() :- solicitud(aterrizar), aeronave(X, mediana),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("P2 esta ocupada. Puede despegar en la pista P3, tiene 5 minutos desde las  "), write(Hora), write(" tiempo actual.\n"),!.

% Para aeronavas grandes
asignar() :- solicitud(despegar), aeronave(X, grande),
            hora(Hora), not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("Se le a asignado la pista P3 para despegar en la hora "), write(Hora), write(".\n"),!.

asignar() :- solicitud(aterrizar), aeronave(X, grande),
            get_time(T), stamp_date_time(T, date(_,_,_,H,M,_,_,_,_), local), atom_concat(H, M, Hora),
            not(reservacion(_, p3, Hora)),
            assertz(reservacion(X, p3, Hora)), write("Puede despegar en la pista P3, tiene 5 minutos desde las  "), write(Hora), write(" tiempo actual.\n"),!.


asignar() :- write("Lo sentimos ninguna pista se encuentra disponible en este momento, por favor intente en otro momento.\n"),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Asignacion de reservacion %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic reservacion/3.

/*
reservacion(aeronave, pista, hora).
*/


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

despegue_aux("despegue").
despegue_aux("despegar").

aterrizaje_aux("aterrizaje").
aterrizaje_aux("aterrizar").



%PENDIENTE REVISAR SI DEBEN GUARDAR SOLO EL NUMERO

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

emergencia("Mayday").
emergencia("inmediato").
emergencia("Emergencia").
emergencia("emergencia").
emergencia("SOS").

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

despedida_aux("Adios").
despedida_aux("adios").
despedida_aux("Gracias").
despedida_aux("gracias").
despedida_aux("agradezco").

hora_(X):-
    atom_chars(X,Chars),
    miembro(:,Chars).

% HAY QUE HACER RETRACT DE RESERVACION??? (Creo que no, para guardar las
% reservaciones)
reset:-
    retractall(vuelo(_)),
    retractall(matricula(_)),
    retractall(solicitud(_)),
    retractall(aeronave(_,_)),
    retractall(direccion(_)),
    retractall(hora(_)).
%retractall(reservacion(_,_,_)).


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


casoSolicitud:-datos(),!,
    read(Input),
    split(Input,Result),
    clasificar2(Result).
casoSolicitud:-asignar(),!.

clasificar2(L):-equal(L,[]),!,casoSolicitud.
clasificar2([X|[Y|Z]]):-equal(X,"Vuelo"),!,
    assert(vuelo(Y)),
    clasificar2(Z).
clasificar2([X|[Y|Z]]):-equal(X,"Matricula"),!,
    assert(matricula(Y)),
    clasificar2(Z).
clasificar2([X|[Y|Z]]):-equal(X,"Aeronave"),!,
    aeronave_aux(Y,T),
    assert(aeronave(Y,T)),
    clasificar2(Z).
clasificar2([X|[Y|Z]]):-equal(X,"Direccion"),!,
    assert(direccion(Y)),
    clasificar2(Z).
clasificar2([X|[Y|Z]]):-equal(X,"Hora"),!,
    assert(hora(Y)),
    clasificar2(Z).
clasificar2([X|Z]):-direccion_auxiliar(X),!,
    assert(direccion(X)),
    clasificar2(Z).
clasificar2([X|Z]):-hora_(X),!,
    assert(hora(X)),
    clasificar2(Z).
clasificar2([X|Z]):-aeronave_aux(X,Y),!,
    assert(aeronave(X,Y)),
    clasificar2(Z).

clasificar2([_|Y]):-clasificar2(Y).


equal(A,A).
clasificar(L):-equal(L,[]),!,writeln("No le entendi").
clasificar([X|_]):-saludo_aux(X),!,
    writeln("Hola, soy MayCEy. ¿En que puedo ayudarle?").
clasificar([X|_]):-despedida_aux(X),!,
    writeln("Es un gusto ayudar. Que tenga un buen día."),
    reset.
clasificar([X|_]):-emergencia(X),!,casoEmergencia.
clasificar([X|_]):-despegue_aux(X),!,
    assert(solicitud(despegar)),
    casoSolicitud.
clasificar([X|_]):-aterrizaje_aux(X),!,
    assert(solicitud(aterrizar)),
    casoSolicitud.
clasificar([_|Y]):-clasificar(Y).

cicloMain:-
    read(Input),
    split(Input,Splited),
    clasificar(Splited),
    cicloMain.





















