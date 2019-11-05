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
%assert(vuelo("hola")),
(equal(Input,"no") ->
despedida(D),
writeln(D);
emergencia(Input,Output),
writeln(Output),
io
)
.

aeronave(cesna).
aeronave(beechcraft).
aeronave(embraerPhenom).
aeronave(boing717).
aeronave(embraer190).
aeronave(airbusA220).
aeronave(boing747).
aeronave(airbusA340).
aeronave(airbusA380).

pista(p1, esteaoeste).
pista(p2, oesteaeste).
pista(p3, esteaoeste).
pista(p4, oesteaeste).

emergenia(pedidamotor).
emergencia(parto).
emergencia(parocardiaco).
emergencia(secuestro).

atencion(bomberos).
atencion(medico).
atencion(oij).
atencion(policia).

:- dynamic condicion/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Determinacion de datos faltantes %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic info/2.

:- retractall(info(_, _)).

/*
info(accion, str) --> despegue o aterrizaje
info(vuelo, num).
info(aerolinea, num).
info(matricula, str).
info(velocidad, num).
info(distancia, num).
info(aeronave, str).
info(hora, num). --> se da en hora militar
info(direccion, str).
*/

% General
datos() :- not(info(accion, _)), write("Quiere despegar o aterrizar?\n"), read(Y), assert(info(accion, Y)), datos().
datos() :- not(info(aeronave, _)), write("Cual es su aeronave?\n"), read(Y), assert(info(aeronave, Y)), datos().
datos() :- not(info(matricula, _)), write("Cual es su matricula?\n"), read(Y), assert(info(matricula, Y)), datos().
datos() :- not(info(direccion, _)), info(accion, despegar), write("En que direccion se encuentra?\n"), read(Y), assert(info(direccion, Y)), datos().

% Aterrizar
datos() :- not(info(velocidad, _)), info(accion, aterrizar), write("Cual es su velocidad?\n"), read(Y), assert(info(velocidad, Y)), datos().
datos() :- not(info(distancia, _)), info(accion, aterrizar), write("Cual es su distancia a la pista?\n"), read(Y), assert(info(distancia, Y)), datos().

% Despegar
datos() :- not(info(vuelo, _)), write("Cual es su numero de vuelo?\n"), read(Y), assert(info(vuelo, Y)), datos().
datos() :- not(info(hora, _)), info(accion, despegar), write("A que hora planea realizar el aterrizaje?\n"), read(Y), assert(info(hora, Y)), datos().

datos().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Determinacion de pista y hora %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

asignar() :- info(accion, despegar), info(direccion, esteaoeste), info(aeronave, X),
            info(hora, Y), not(reservacion(_, p1, Y)),
            assert(reservacion(X, p1, Y)), write("Se le a asignado la pista p1 por los siguientes 5min"),
            retractall(info(_, _)).

asignar() :- info(accion, despegar), info(direccion, esteaoeste), info(aeronave, X),
            info(hora, Y), not(reservacion(_, p3, Y)),
            assert(reservacion(X, p3, Y)), write("Se le a asignado la pista p3 por los siguientes 5min"),
            retractall(info(_, _)).

asignar() :- info(accion, despegar), info(direccion, oesteaeste), info(aeronave, X),
             info(hora, Y), not(reservacion(_, p2, Y)),
              assert(reservacion(X, p2, Y)), write("Se le a asignado la pista p2 por los siguientes 5min"),
              retractall(info(_, _)).

asignar() :- info(accion, despegar), info(direccion, oesteaeste), info(aeronave, X),
             info(hora, Y), not(reservacion(_, p4, Y)),
              assert(reservacion(X, p4, Y)), write("Se le a asignado la pista p4 por los siguientes 5min"),
              retractall(info(_, _)).

asignar() :- write("No hay pistas disponibles en este momento, por favor intente de nuevo en otro momento.")
              ,retractall(info(_, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Asignacion de reservacion %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic reservacion/3.

/*
reservacion(aeronave, pista, hora).
*/
