/* PUNTO 1 */
esPesada(Materia) :-
  materia(Materia, Horas),
  Horas>100.

esPesada(Materia) :-
  materia(Materia,_),
  menosDe15Letras(Materia),
  not(esPromocionable(Materia)).

menosDe15Letras(Materia) :-
  atom_length(Materia,CantLetras),
  CantLetras =< 15.


/* PUNTO 2) */
/* a) */
esInicial(Materia) :-
  materia(Materia,_),
  not(esCorrelativaDe(Materia,_)).

/* b) */
% materias necesarias para cursar una materia
necesariaParaCursar(Materia,Correlativa) :-
  esCorrelativaDe(Materia,OtraMateria),
  necesariaParaCursar(OtraMateria,Correlativa).

%Caso Base
necesariaParaCursar(Materia,Correlativa) :- esCorrelativaDe(Materia,Correlativa).

/* c) */
% materias que habilita una materia
% Materia = materia habilitada
habilita(Correlativa,Materia) :- esCorrelativaDe(Materia,Correlativa).


/* PUNTO 3 */
/* a) */
% materias que cursó un estudiante.
curso(Estudiante,Materia) :- aproboCursada(Estudiante,Materia).
curso(Estudiante,Materia) :- rindioLibre(Estudiante,Materia).

aproboCursada(Estudiante,Materia) :-
  cursada(Estudiante,Materia,Nota),
  notaAprobacion(Nota).

% rindio el final de una materia sin haberla cursado.
rindioLibre(Estudiante,Materia) :-
  materia(Materia,_), %Predicado Generador
  not(cursada(Estudiante,Materia,_)),
  examenFinal(Estudiante,Materia,_).

notaAprobacion(Nota) :- Nota>=4.

/* b) */
% materias aprobadas por un estudiante.
aprobo(Estudiante,Materia) :- aproboFinal(Estudiante,Materia).
aprobo(Estudiante,Materia) :- promociono(Estudiante,Materia).

aproboFinal(Estudiante,Materia) :-
  % curso(Estudiante,Materia),
  examenFinal(Estudiante,Materia,Nota),
  notaAprobacion(Nota).

promociono(Estudiante,Materia) :-
  esPromocionable(Materia),
  cursada(Estudiante,Materia,Nota),
  notaPromocion(Nota).

notaPromocion(Nota) :- Nota>=7.

/* PUNTO 4 */
/* a) */
adeudaFinal(Estudiante, Materia):-
  curso(Estudiante,Materia),
  not(aprobo(Estudiante,Materia)).

/* b) */
% una materia bloquea a otra
bloquea(Correlativa,Materia,Estudiante) :-
  necesariaParaCursar(Materia,Correlativa), %la primera es correlativa con la segunda
  curso(Estudiante,Materia),
  adeudaFinal(Estudiante,Correlativa). % adeuda el final de la primera, esta ya verifica que la curso


/* c) */
/*PROBAR*/
perdioPromocion(Estudiante,Materia):-
  promociono(Estudiante,Materia), % obtuvo la nota necesaria para promocionar
  necesariaParaCursar(Materia,Correlativa),
  adeudaFinal(Estudiante,Correlativa). % pero adeuda finales de materias correlativas anteriores


/* d) */
% no adeuda ningún final
estaAlDia(Estudiante) :-
  estudiante(Estudiante),
	forall(curso(Estudiante,Materia), not(adeudaFinal(Estudiante,Materia))).


estudiante(Estudiante) :- cursada(Estudiante,_,_).

puedeRendirFinal(Estudiante,Materia) :-
  curso(Estudiante, Materia),
  forall(necesariaParaCursar(Materia, Correlativa), aprobo(Estudiante,Correlativa)).


/***************************CASOS DE PRUEBA*********************************/

/* PUNTO 2 */
% CORRELATIVAS PARA CURSAR
% esCorrelativaDe(materia, correlativa).
esCorrelativaDe(laboratorioDeComputacionII,laboratorioDeComputacionI).

esCorrelativaDe(sistemasDeProcesamientoDeDatos,laboratorioDeComputacionI).

esCorrelativaDe(matematicaII,laboratorioDeComputacionI).
esCorrelativaDe(matematicaII,matematicaI).

esCorrelativaDe(algoritmosI,laboratorioDeComputacionII).
esCorrelativaDe(algoritmosI,matematicaII).
esCorrelativaDe(algoritmosI,sistemasDeProcesamientoDeDatos).

esCorrelativaDe(sistemasOperativos,sistemasDeProcesamientoDeDatos).
esCorrelativaDe(sistemasOperativos,laboratorioDeComputacionII).

esCorrelativaDe(matematicaIII,laboratorioDeComputacionII).
esCorrelativaDe(matematicaIII,matematicaII).

esCorrelativaDe(algoritmosII,algoritmosI).
esCorrelativaDe(algoritmosII,matematicaIII).

esCorrelativaDe(redesLocales,sistemasOperativos).

esCorrelativaDe(metodosNumericos,algoritmosI).

esCorrelativaDe(algoritmosIII,algoritmosII).
esCorrelativaDe(algoritmosIII,redesLocales).

esCorrelativaDe(basesDeDatos,algoritmosII).

esCorrelativaDe(seminarioDeProgramacion,algoritmosII).
esCorrelativaDe(seminarioDeProgramacion,redesLocales).
esCorrelativaDe(seminarioDeProgramacion,metodosNumericos).

esCorrelativaDe(programacionHerramientasModernas,algoritmosIII).

esCorrelativaDe(proyectosDeSoftware,algoritmosIII).
esCorrelativaDe(proyectosDeSoftware,basesDeDatos).

esCorrelativaDe(paradigmasDeProgramacion,algoritmosIII).

/* PUNTO 5 */
% materia(nombre, cantidadTotalHoras).
materia(matematicaI,96).
materia(laboratorioDeComputacionI,128).
materia(electricidadYMagnetismo,128).
materia(laboratorioDeComputacionII,128).
materia(sistemasDeProcesamientoDeDatos,128).
materia(matematicaII,96).
materia(algoritmosI,160).
materia(matematicaIII,96).
materia(sistemasOperativos,96).
materia(algoritmosII,144).
materia(redesLocales,128).
materia(metodosNumericos,80).
materia(algoritmosIII,160).
materia(basesDeDatos,128).
materia(seminarioDeProgramacion,64).
materia(programacionHerramientasModernas,160).
materia(proyectosDeSoftware,128).
materia(paradigmasDeProgramacion,64).

% esPromocionable(materia).
esPromocionable(laboratorioDeComputacionI).
esPromocionable(laboratorioDeComputacionII).
esPromocionable(matematicaI).
esPromocionable(matematicaII).
esPromocionable(electricidadYMagnetismo).
esPromocionable(sistemasDeProcesamientoDeDatos).
esPromocionable(sistemasOperativos).
esPromocionable(paradigmasDeProgramacion).


/* PUNTO 7 */
% cursada(estudiante, materia, nota).
cursada(pepo,matematicaI,8).
cursada(pepo,laboratorioDeComputacionI,8).
cursada(pepo,electricidadYMagnetismo,8).
cursada(pepo,laboratorioDeComputacionII,5).
cursada(pepo,matematicaII,6).
cursada(pepo,matematicaIII,4).

% examenFinal(estudiante,materia,nota).
examenFinal(pepo,matematicaII,4).
examenFinal(pepo,laboratorioDeComputacionII,2).
examenFinal(pepo,sistemasDeProcesamientoDeDatos,6).  % libre


% ******************************TESTS**********************************

:- begin_tests(cursada_universitaria).
/* PUNTO 6 */
test(algoritmosI_es_materia_pesada,nondet) :- esPesada(algoritmosI).
test(basesDeDatos_es_materia_pesada,nondet) :- esPesada(basesDeDatos).
test(metodosNumericos_no_es_materia_pesada,fail) :- esPesada(metodosNumericos).

test(materias_iniciales, set(Materias==[matematicaI,laboratorioDeComputacionI,electricidadYMagnetismo])) :-
  esInicial(Materias).

test(materias_necesarias_para_algoritmosI,
 set(Materias==[matematicaI,matematicaII,laboratorioDeComputacionI,laboratorioDeComputacionII,sistemasDeProcesamientoDeDatos])) :-
   necesariaParaCursar(algoritmosI,Materias).

/* PUNTO 8 */
test(materias_aprobadas_de_pepo, set(Materias==[laboratorioDeComputacionI,matematicaI,matematicaII,electricidadYMagnetismo,sistemasDeProcesamientoDeDatos])) :-
  aprobo(pepo,Materias).

test(pepo_no_esta_al_dia_pero_no_se_perdio_ninguna_promocion,fail) :-
  estaAlDia(pepo), perdioPromocion(pepo,_).

test(matematicaIII_es_bloqueada_por_laboratorioDeComputacionII,nondet) :-
  bloquea(laboratorioDeComputacionII,matematicaIII,pepo).

:- end_tests(cursada_universitaria).
