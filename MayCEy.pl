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

