%Saludo estándar para iniciar una conversación
saludo("Hola, soy MayCEy. ¿En que puedo ayudarle?").

%Despedida estándar para cerrar una conversación
despedida("Es un gusto ayudar. Que tenga un buen día.").

%Tabla de emergencias con su respectiva acción, en caso de una emergencia no registrada se usa la última acción
emergencia("perdida de motor","llamaremos a los bomberos de inmediato\n").
emergencia("parto en medio vuelo","llamaremos a un médico de inmediato\n").
emergencia("paro cardiaco de pasajero","llamaremos a un médico de inmediato\n").
emergencia("secuestro","llamaremos al OIJ de inmediato\n").
emergencia(_,"lo pondremos en contacto con la línea de emergencia\n").

%Regla para probar inputs y outputs de strings
cicloInput:-
saludo(S),
writeln(S),
read(Input),
(equal(Input,"no") ->
despedida(D),
writeln(D);
emergencia(Input,Output),
writeln(Output),
cicloInput
)
.

%Hechos de reserva, usan una tring con la pista y un int con l hora en formato militar
reservado("P1",700).
reservado("P2-1",700).
reservado("P2-2",700).
reservado("P3",705).

%Regla para asginar una pista en caso de mergencia, se busca una disponible lo más cercana posible y si no se encuentra se asigna la P1
asginarEmergencia("P1",Hora):-not(reservado("P1",Hora)),!.
asginarEmergencia("P2-1",Hora):-not(reservado("P2-1",Hora)),!.
asginarEmergencia("P2-2",Hora):-not(reservado("P2-2",Hora)),!.
asginarEmergencia("P3",Hora):-not(reservado("P3",Hora)),!.
asginarEmergencia("P1",_).


%Regla para ejecutar el ciclo de una emergencia
casoEmergencia:-
writeln("Por favor especifique de forma puntual su emergencia"),
read(Emergencia),
emergencia(Emergencia,Medida),
writeln("Por favor identifiquese"),
read(ID),
asginarEmergencia(Pista,700),
write("Su pista asiganada es "),
write(Pista),
write(" y "),
write(Medida),
writeln("").

