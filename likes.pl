
clasificar2(A) :-
    equal(A, []),
    !,
    casoSolicitud.
clasificar2([A, B|C]) :-
    equal(A, "Vuelo"),
    !,
    assert(vuelo(B)),
    clasificar2(C).
clasificar2([A, B|C]) :-
    equal(A, "Matricula"),
    !,
    assert(matricula(B)),
    clasificar2(C).
clasificar2([A, B|D]) :-
    equal(A, "Aeronave"),
    !,
    aeronave_aux(B, C),
    assert(aeronave(B, C)),
    clasificar2(D).
clasificar2([A, B|C]) :-
    equal(A, "Direccion"),
    !,
    assert(direccion(B)),
    clasificar2(C).
clasificar2([A, B|C]) :-
    equal(A, "Hora"),
    !,
    assert(hora(B)),
    clasificar2(C).
clasificar2([A|B]) :-
    direccion_auxiliar(A),
    !,
    assert(direccion(A)),
    clasificar2(B).
clasificar2([A|B]) :-
    hora_(A),
    !,
    assert(hora(A)),
    clasificar2(B).
clasificar2([A|C]) :-
    aeronave_aux(A, B),
    !,
    assert(aeronave(A, B)),
    clasificar2(C).
clasificar2([_|A]) :-
    clasificar2(A).

:- dynamic vuelo/1.

vuelo("404").

:- multifile prolog_clause_name/2.


aeronave_aux("Cessna", pequena).
aeronave_aux("Beechcraft", pequena).
aeronave_aux("Embraer-Phenom", pequena).
aeronave_aux("Boeing-717", mediana).
aeronave_aux("Embraer-190", mediana).
aeronave_aux("Airbus-A220", mediana).
aeronave_aux("Boeing-747", grande).
aeronave_aux("Airbus-A340", grande).
aeronave_aux("Airbus-A380", grande).

:- dynamic solicitud/1.

solicitud(despegar).
solicitud(despegar).
solicitud(despegar).
solicitud(despegar).

equal(A, A).

reset :-
    retractall(vuelo(_)),
    retractall(matricula(_)),
    retractall(solicitud(_)),
    retractall(aeronave(_, _)),
    retractall(direccion(_)),
    retractall(hora(_)).

datos :-
    not(solicitud(_)),
    write("Quiere despegar o aterrizar?\n").
datos :-
    not(aeronave(_, _)),
    write("Cual es su aeronave?\n").
datos :-
    not(matricula(_)),
    write("Cual es su matricula?\n").
datos :-
    not(direccion(_)),
    write("En que direccion se encuentra?\n").
datos :-
    not(vuelo(_)),
    solicitud(despegar),
    write("Cual es su numero de vuelo?\n").
datos :-
    not(hora(_)),
    solicitud(despegar),
    write("A que hora planea realizar el aterrizaje?\n").

condicion(viento).
condicion(peso).
condicion(largoPista).
condicion(velocidad).

emergencia("Mayday").
emergencia("inmediato").
emergencia("Emergencia").
emergencia("emergencia").
emergencia("SOS").

aterrizaje_aux("aterrizaje").
aterrizaje_aux("aterrizar").

asignar :-
    solicitud(despegar),
    aeronave(B, pequena),
    hora(A),
    not(reservacion(_, p1, A)),
    assertz(reservacion(B, p1, A)),
    write("Se le ha asignado la pista P1 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    aeronave(E, pequena),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p1, D)),
    assertz(reservacion(E, p1, D)),
    write("Puede despegar en la pista P1, tiene 5 minutos desde las  "),
    write(D),
    write(" tiempo actual.\n"),
    !.
asignar :-
    solicitud(despegar),
    direccion("Este-Oeste"),
    aeronave(B, pequena),
    hora(A),
    not(reservacion(_, p2-1, A)),
    assertz(reservacion(B, p2-1, A)),
    write("P1 esta ocupada. Se le a asignado la pista P2-1 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    direccion("Este-Oeste"),
    aeronave(E, pequena),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p2-1, D)),
    assertz(reservacion(E, p2-1, D)),
    write("P1 esta ocupada. Puede despegar en la pista P2-1, tiene 5 minutos desde las  "),
    write(D),
    write(" tiempo actual.\n"),
    !.
asignar :-
    solicitud(despegar),
    direccion("Oeste-Este"),
    aeronave(B, pequena),
    hora(A),
    not(reservacion(_, p2-2, A)),
    assertz(reservacion(B, p2-2, A)),
    write("P1 esta ocupada. Se le a asignado la pista P2-2 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    direccion("Oeste-Este"),
    aeronave(E, pequena),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p2-2, D)),
    assertz(reservacion(E, p2-2, D)),
    write("P2-2 esta ocupada. Puede despegar en la pista P2-2, tiene 5 minutos desde las  "),
    write(D),
    '.'(write(" tiempo actual.\n"), !, F),
    call(F).
asignar :-
    solicitud(despegar),
    aeronave(B, pequena),
    hora(A),
    not(reservacion(_, p3, A)),
    assertz(reservacion(B, p3, A)),
    write("P1 esta ocupada. Se le a asignado la pista P3 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    aeronave(E, pequena),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p3, D)),
    assertz(reservacion(E, p3, D)),
    write("P1 esta ocupada. Puede despegar en la pista P3, tiene 5 minutos desde las  "),
    write(D),
    write(" tiempo actual.\n"),
    !.
asignar :-
    solicitud(despegar),
    direccion("Este-Oeste"),
    aeronave(B, mediana),
    hora(A),
    not(reservacion(_, p2-1, A)),
    assertz(reservacion(B, p2-1, A)),
    write("Se le a asignado la pista P2-1 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    direccion("Este-Oeste"),
    aeronave(E, mediana),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p2-1, D)),
    assertz(reservacion(E, p2-1, D)),
    write("Puede despegar en la pista P2-1, tiene 5 minutos desde las  "),
    write(D),
    write(" tiempo actual.\n"),
    !.
asignar :-
    solicitud(despegar),
    direccion("Oeste-Este"),
    aeronave(B, mediana),
    hora(A),
    not(reservacion(_, p2-2, A)),
    assertz(reservacion(B, p2-2, A)),
    write("Se le a asignado la pista P2-2 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    direccion("Oeste-Este"),
    aeronave(E, mediana),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p2-2, D)),
    assertz(reservacion(E, p2-2, D)),
    write("Puede despegar en la pista P2-2, tiene 5 minutos desde las  "),
    write(D),
    write(" tiempo actual.\n"),
    !.
asignar :-
    solicitud(despegar),
    aeronave(B, mediana),
    hora(A),
    not(reservacion(_, p3, A)),
    assertz(reservacion(B, p3, A)),
    write("P2 esta ocupada. Se le a asignado la pista P3 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    aeronave(E, mediana),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p3, D)),
    assertz(reservacion(E, p3, D)),
    write("P2 esta ocupada. Puede despegar en la pista P3, tiene 5 minutos desde las  "),
    write(D),
    write(" tiempo actual.\n"),
    !.
asignar :-
    solicitud(despegar),
    aeronave(B, grande),
    hora(A),
    not(reservacion(_, p3, A)),
    assertz(reservacion(B, p3, A)),
    write("Se le a asignado la pista P3 para despegar en la hora "),
    write(A),
    write(".\n"),
    !.
asignar :-
    solicitud(aterrizar),
    aeronave(E, grande),
    get_time(A),
    stamp_date_time(A,
                    date(_,
                         _,
                         _,
                         B,
                         C,
                         _,
                         _,
                         _,
                         _),
                    local),
    atom_concat(B, C, D),
    not(reservacion(_, p3, D)),
    assertz(reservacion(E, p3, D)),
    write("Puede despegar en la pista P3, tiene 5 minutos desde las  "),
    write(D),
    write(" tiempo actual.\n"),
    !.
asignar :-
    write("Lo sentimos ninguna pista se encuentra disponible en este momento, por favor intente en otro momento.\n"),
    !.

:- dynamic aeronave/2.

aeronave("Cessna", pequena).

:- multifile prolog_predicate_name/2.


despegue_aux("despegue").
despegue_aux("despegar").

emergencia_auxiliar("perdida de motor", "llamaremos a los bomberos de inmediato\n").
emergencia_auxiliar("parto en medio vuelo", "llamaremos a un médico de inmediato\n").
emergencia_auxiliar("paro cardiaco de pasajero", "llamaremos a un médico de inmediato\n").
emergencia_auxiliar("secuestro", "llamaremos al OIJ de inmediato\n").
emergencia_auxiliar(_, "lo pondremos en contacto con la línea de emergencia\n").

pista(p1).
pista(p21).
pista(p22).
pista(p3).

:- dynamic hora/1.

hora("4:3").

despedida_aux("Adios").
despedida_aux("adios").
despedida_aux("Gracias").
despedida_aux("gracias").
despedida_aux("agradezco").

casoEmergencia :-
    writeln("Por favor especifique de forma puntual su emergencia"),
    read(A),
    emergencia_auxiliar(A, B),
    writeln("Por favor identifiquese"),
    read(_),
    write("Su pista asignada es "),
    write("P3"),
    write(" y "),
    write(B),
    writeln("").

hora_(A) :-
    atom_chars(A, B),
    miembro(:, B).

:- thread_local thread_message_hook/3.
:- dynamic thread_message_hook/3.
:- volatile thread_message_hook/3.


casoSolicitud :-
    datos,
    !,
    read(A),
    split(A, B),
    clasificar2(B).
casoSolicitud :-
    asignar,
    !.

:- dynamic matricula/1.

matricula("dasd").

cicloMain :-
    read(A),
    split(A, B),
    clasificar(B),
    open('likes.pl', write, C),
    set_output(C),
    listing,
    close(C),
    cicloMain.

miembro(A, [A|_]).
miembro(A, [_|B]) :-
    miembro(A, B).

:- dynamic reservacion/3.

reservacion("Cessna", p1, "~:~~").
reservacion("Cessna", p1, "3:3").
reservacion("Boeing-747", p3, '2351').
reservacion("Cessna", p1, "4:3").
reservacion("Cessna", p2-1, "4:3").
reservacion("Cessna", p3, "4:3").

direccion_auxiliar("Este-Oeste").
direccion_auxiliar("Oeste-Este").

clasificar(A) :-
    equal(A, []),
    !,
    writeln("No le entendí.").
clasificar([A|_]) :-
    saludo_aux(A),
    !,
    writeln("Hola, soy MayCEy. ¿En que puedo ayudarle?").
clasificar([A|_]) :-
    despedida_aux(A),
    !,
    writeln("Es un gusto ayudar. Que tenga un buen día."),
    reset.
clasificar([A|_]) :-
    emergencia(A),
    !,
    casoEmergencia.
clasificar([A|_]) :-
    despegue_aux(A),
    !,
    assert(solicitud(despegar)),
    casoSolicitud.
clasificar([A|_]) :-
    aterrizaje_aux(A),
    !,
    assert(solicitud(aterrizar)),
    casoSolicitud.
clasificar([_|A]) :-
    clasificar(A).

:- dynamic prolog_exception_hook/4.
:- multifile prolog_exception_hook/4.

prolog_exception_hook(error(E, context(Ctx0, Msg)), error(E, context(prolog_stack(Stack), Msg)), Fr, GuardSpec) :-
    prolog_stack:
    (   current_prolog_flag(backtrace, true),
        \+ is_stack(Ctx0, _Frames),
        (   atom(GuardSpec)
        ->  debug(backtrace,
                  'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)',
                  [GuardSpec, E, Ctx0]),
            stack_guard(GuardSpec),
            Guard=GuardSpec
        ;   prolog_frame_attribute(GuardSpec,
                                   predicate_indicator,
                                   Guard),
            debug(backtrace,
                  'Got exception ~p (Ctx0=~p, Catcher=~p)',
                  [E, Ctx0, Guard]),
            stack_guard(Guard)
        ),
        (   current_prolog_flag(backtrace_depth, Depth)
        ->  Depth>0
        ;   Depth=20
        ),
        get_prolog_backtrace(Depth,
                             Stack0,
                             [frame(Fr), guard(Guard)]),
        debug(backtrace, 'Stack = ~p', [Stack0]),
        clean_stack(Stack0, Stack1),
        join_stacks(Ctx0, Stack1, Stack)
    ).
prolog_exception_hook(error(A, context(C, B)), error(A, context(prolog_stack(J), B)), G, D) :-
    user:
    (   prolog_stack:current_prolog_flag(backtrace, true),
        \+ prolog_stack:is_stack(C, _),
        (   atom(D)
        ->  prolog_stack:debug(backtrace, 'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)', [D, A, C]),
            prolog_stack:stack_guard(D),
            E=D
        ;   prolog_stack:prolog_frame_attribute(D, predicate_indicator, E),
            prolog_stack:debug(backtrace, 'Got exception ~p (Ctx0=~p, Catcher=~p)', [A, C, E]),
            prolog_stack:stack_guard(E)
        ),
        (   prolog_stack:current_prolog_flag(backtrace_depth, F)
        ->  prolog_stack:(F>0)
        ;   F=20
        ),
        prolog_stack:get_prolog_backtrace(F, H, [frame(G), guard(E)]),
        prolog_stack:debug(backtrace, 'Stack = ~p', [H]),
        prolog_stack:clean_stack(H, I),
        prolog_stack:join_stacks(C, I, J)
    ).
prolog_exception_hook(error(A, context(C, B)), error(A, context(prolog_stack(J), B)), G, D) :-
    user:
    (   prolog_stack:current_prolog_flag(backtrace, true),
        \+ prolog_stack:is_stack(C, _),
        (   atom(D)
        ->  prolog_stack:debug(backtrace, 'Got uncaught (guard = ~q) exception ~p (Ctx0=~p)', [D, A, C]),
            prolog_stack:stack_guard(D),
            E=D
        ;   prolog_stack:prolog_frame_attribute(D, predicate_indicator, E),
            prolog_stack:debug(backtrace, 'Got exception ~p (Ctx0=~p, Catcher=~p)', [A, C, E]),
            prolog_stack:stack_guard(E)
        ),
        (   prolog_stack:current_prolog_flag(backtrace_depth, F)
        ->  prolog_stack:(F>0)
        ;   F=20
        ),
        prolog_stack:get_prolog_backtrace(F, H, [frame(G), guard(E)]),
        prolog_stack:debug(backtrace, 'Stack = ~p', [H]),
        prolog_stack:clean_stack(H, I),
        prolog_stack:join_stacks(C, I, J)
    ).

split(A, B) :-
    split_string(A, " ", " ", B),
    nl.

:- dynamic direccion/1.

direccion("Este-Oeste").

saludo_aux("Hola").
saludo_aux("hola").
saludo_aux("MayCEy").
saludo_aux("Buenas").
saludo_aux("Buenos").
saludo_aux("Saludos").
