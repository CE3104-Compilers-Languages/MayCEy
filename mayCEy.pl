/*
saludo("Hola, soy MayCEy. ¿En que puedo ayudarle?").
despedida("Es un gusto ayudar. Que tenga un buen día.").

emergencia("perdida de motor","llamaremos a los bomberos de inmediato\n").
emergencia("parto en medio vuelo","llamaremos a un médico de inmediato\n").
emergencia("paro cardiaco de pasajero","llamaremos a un médico de inmediato\n").
emergencia("secuestro","llamaremos al OIJ de inmediato\n").
emergencia(_,"lo pondremos en contacto con la línea de emergencia\n").
:- dynamic
        vuelo/1.
vuelo("").
equal(A,A).


io:-
saludo(S),
writeln(S),
read(Input),
%retract(vuelo("")),
%assertz(vuelo("hola")),
(equal(Input,"no") ->
despedida(D),
writeln(D);
emergencia(Input,Output),
writeln(Output),
io
)
.
*/

aeronave_aux("Cesna", pequena).
aeronave_aux("Beechcraft", pequena).
aeronave_aux("Embraer-Phenom", pequena).
aeronave_aux("Boeing-717", mediana).
aeronave_aux("Embraer-190", mediana).
aeronave_aux("Airbus-A220", mediana).
aeronave_aux("Boeing747", grande).
aeronave_aux("Airbus-A340", grande).
aeronave_aux("Airbus-A380", grande).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Determinacion de datos faltantes %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic hora/1.
:- dynamic identificacion/1.
:- dynamic solicitud/1.
:- dynamic aeronave/2.
:- dynamic direccion/1.
:- dynamic vuelo/1.

:- retractall(hora(_)).
:- retractall(identificacion(_, _)).
:- retractall(solicitud(_)).
:- retractall(aeronave(_,_)).
:- retractall(direccion(_)).
:- retractall(vuelo(_)).

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

asignar() :- solicitud(despegar), direccion("Oeste-Eeste"), aeronave(X, pequena),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Asignacion de reservacion %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic reservacion/3.

/*
reservacion(aeronave, pista, hora).
*/
