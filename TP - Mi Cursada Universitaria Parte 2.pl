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
habilitaA(MateriaHabilitada,Materia) :-
  necesariaParaCursar(Materia,MateriaHabilitada).


/* PUNTO 3 */
/* a) */
% materias que cursó un estudiante.
curso(Estudiante,Materia) :- aproboCursada(Estudiante,Materia).
curso(Estudiante,Materia) :- rindioLibre(Estudiante,Materia).

aproboCursada(Estudiante,Materia) :-
  cursada(Estudiante,Materia,Nota,_),
  notaAprobacion(Nota).

% rindio el final de una materia sin haberla cursado.
rindioLibre(Estudiante,Materia) :-
  materia(Materia,_), %Predicado Generador
  not(cursada(Estudiante,Materia,_,_)),
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
  cursada(Estudiante,Materia,Nota,_),
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
perdioPromocion(Estudiante,Materia):-
  promociono(Estudiante,Materia), % obtuvo la nota necesaria para promocionar
  necesariaParaCursar(Materia,Correlativa),
  adeudaFinal(Estudiante,Correlativa). % pero adeuda finales de materias correlativas anteriores


/* d) */
% no adeuda ningún final
estaAlDia(Estudiante):-
  cursada(Estudiante,_,_,_),
  not(adeudaFinal(Estudiante,_)).

% estudiante(Estudiante) :- cursada(Estudiante,_,_,_).

% puedeRendirFinal(Estudiante,Materia) :-
%   curso(Estudiante, Materia),
%   forall(necesariaParaCursar(Materia, Correlativa), aprobo(Estudiante,Correlativa)).

/*****************************************************************************/

/******************************** PARTE 2 *************************************/
/* PUNTO 1: FUTURAS CURSADAS */

puedeCursar(Estudiante,Materia):-
  materia(Materia,_),
  not(curso(Estudiante,Materia)),
  cumpleRegimenCorrelativas(Estudiante,Materia).

%cumple con el regimen de correlativas
cumpleRegimenCorrelativas(Estudiante,Materia):-
  cursoCorrelativasNecesarias(Estudiante,Materia),
  aproboSegundoNivelCorrelativas(Estudiante,Materia).

% curso todas las materias necesarias para cursar
cursoCorrelativasNecesarias(Estudiante,Materia):-
  forall(necesariaParaCursar(Materia,Correlativa),curso(Estudiante,Correlativa)).

%aprobadas como mínimo el segundo nivel de correlativas
aproboSegundoNivelCorrelativas(Estudiante,Materia):-
  forall(materiaCorrelativaSegundoNivel(Materia,CorrelativaSegundoNivel),aprobo(Estudiante,CorrelativaSegundoNivel)).

materiaCorrelativaSegundoNivel(Materia,CorrelativaSegundoNivel):-
  esCorrelativaDe(Materia,CorrelativaPrimerNivel),
  esCorrelativaDe(CorrelativaPrimerNivel,CorrelativaSegundoNivel),
  CorrelativaPrimerNivel\=CorrelativaSegundoNivel.


/* PUNTO 2: LOS CUATRIMESTRES */
% a)
enQueCuatrimestreCurso(Estudiante,Materia,Cuatrimestre,Anio):-
  cursoEn(Estudiante,Materia,cuatrimestral(Anio,Cuatrimestre)).

cursoEn(Estudiante,Materia,Epoca):-
  cursada(Estudiante,Materia,_,Epoca).

% b)
recurso(Estudiante,Materia):-
  cursoEn(Estudiante,Materia,Epoca),
  cursoEn(Estudiante,Materia,OtraEpoca),
  Epoca \= OtraEpoca.

% ------ MODALIDADES DE CURSADA ---------
% cuatrimestral(anio,cuatrimestre).
% anual(anio).
% verano(anio,mes).
-----------------------------------------

/* PUNTO 4: Perfiles de estudiantes */
% a)
sinDescanso(Estudiante):-
  recurso(Estudiante,_),
  forall(recurso(Estudiante,Materia),cursoEnLaSiguienteCursada(Estudiante,Materia)).

cursoEnLaSiguienteCursada(Estudiante,Materia):-
  cursada(Estudiante,Materia,_,Epoca),
  cursada(Estudiante,Materia,_,OtraEpoca),
  siguienteCursada(Epoca,OtraEpoca).

siguienteCursada(cuatrimestral(Anio,Cuatrimestre),cuatrimestral(OtroAnio,OtroCuatrimestre)):-
    siguienteCuatrimestre(Anio,Cuatrimestre,OtroAnio,OtroCuatrimestre).

siguienteCursada(anual(Anio),anual(OtroAnio)):- siguienteAnio(Anio,OtroAnio).
siguienteCursada(anual(Anio),cuatrimestral(OtroAnio,1)):- siguienteAnio(Anio,OtroAnio).
siguienteCursada(verano(Anio,_),anual(OtroAnio)):- mismoAnio(Anio,OtroAnio).
siguienteCursada(verano(Anio),cuatrimestral(OtroAnio,1)):- mismoAnio(Anio,OtroAnio).

siguienteCuatrimestre(Anio,1,OtroAnio,2):- mismoAnio(Anio,OtroAnio).
siguienteCuatrimestre(Anio,2,OtroAnio,1):- siguienteAnio(Anio,OtroAnio).

% ______________________________________

siguienteAnio(Anio,AnioSiguiente):- AnioSiguiente is Anio + 1.
mismoAnio(Anio,OtroAnio):- OtroAnio is Anio.

desaproboCursada(Nota) :- not(notaAprobacion(Nota)).

% ______________________________________

% b) No recurso ninguna materia
invictus(Estudiante) :-
  cursada(Estudiante,_,_,_), %Predicado generador
  not(recurso(Estudiante,_)).

% c)
repechaje(Estudiante):-
  recurso(Estudiante,Materia),
  recursoElPrimerCuatrimestreDelSiguienteAnio(Estudiante,Materia),
  promociono(Estudiante,Materia).

recursoElPrimerCuatrimestreDelSiguienteAnio(Estudiante, Materia) :-
  cursada(Estudiante,Materia,_,anual(Anio)),
  cursada(Estudiante,Materia,_,cuatrimestral(OtroAnio, 1)),
  siguienteAnio(Anio,OtroAnio).

% d)  Promocionó todas las materias promocionables que cursó.
buenasCursadas(Estudiante):-
  cursada(Estudiante,_,_,_),
  forall(cursada(Estudiante,Materia,_,_),promociono(Estudiante,Materia)).

% e)
seLoQueHicisteElVeranoPasado(Estudiante) :-
    cursada(Estudiante,_,_,_),
    forall(cursada(Estudiante,_,_,cuatrimestral(Anio,_)), cursoEnVerano(Estudiante,Anio)),
    forall(cursada(Estudiante,_,_,anual(OtroAnio)), cursoEnVerano(Estudiante,OtroAnio)).

cursoEnVerano(Estudiante, Anio) :-
    siguienteAnio(Anio,SiguienteAnio),  %ciclo lectivo
    cursada(Estudiante,_,_,verano(SiguienteAnio,_)).


/* PUNTO 5 */
% perfil(estudiante,perfil).
perfil(Estudiante, sinDescanso):-
  sinDescanso(Estudiante).

perfil(Estudiante, invictus):-
  invictus(Estudiante).

perfil(Estudiante, repechaje):-
  repechaje(Estudiante).

perfil(Estudiante, buenasCursadas):-
  buenasCursadas(Estudiante).

perfil(Estudiante, seLoQueHicisteElVeranoPasado):-
  seLoQueHicisteElVeranoPasado(Estudiante).

tieneUnicoPerfil(Estudiante):-
  perfil(Estudiante,_), %Predicado Generador
  not(tieneMuchosPerfiles(Estudiante)).

% tiene otro perfil
tieneMuchosPerfiles(Estudiante):-
  perfil(Estudiante,Perfil),
  perfil(Estudiante,OtroPerfil),
  Perfil \= OtroPerfil.


/* PUNTO 6: Desempeño académico */
desempenioAcademico(Estudiante,Materia,Indice):-
  cursada(Estudiante,Materia,Nota,Epoca),
  indice(Nota,Epoca,Indice).

indice(Nota,anual(_),Indice):- Indice is Nota.
indice(Nota,cuatrimestral(_,Cuatrimestre),Indice):- Indice is Nota - Cuatrimestre.

indice(Nota,verano(Anio,Mes),Indice):-
  anioMasMes(Anio,Mes,CantidadLetras),
  puntajeCursoDeVerano(CantidadLetras,Nota,Indice).

puntajeCursoDeVerano(CantidadLetras,Nota,Indice):-
  esPar(CantidadLetras),
  Indice is Nota.

puntajeCursoDeVerano(CantidadLetras,Nota,Indice):-
  not(esPar(CantidadLetras)),
  Indice is Nota/2.

anioMasMes(Anio,Mes,CantidadLetras):-
  atom_length(Mes,CantLetrasMes),
  CantidadLetras is Anio + CantLetrasMes.

esPar(Numero):- 0 is Numero mod 2.


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
% cursada(estudiante, materia, nota, epoca).

/* PUNTO 3 P2: Reformas en el plan */
% cuatrimestral(anio,cuatrimestre).
% anual(anio).
% verano(anio,mes).
cursada(pepo,matematicaI,8,cuatrimestral(2012,1)).
cursada(pepo,laboratorioDeComputacionI,8,cuatrimestral(2012,1)).
cursada(pepo,electricidadYMagnetismo,8,cuatrimestral(2012,1)).
cursada(pepo,laboratorioDeComputacionII,5,cuatrimestral(2012,2)).
cursada(pepo,matematicaII,6,cuatrimestral(2012,2)).
cursada(pepo,matematicaIII,4,anual(2013)).

cursada(pablitoLescano,matematicaI,8,cuatrimestral(2013,1)).
cursada(pablitoLescano,laboratorioDeComputacionI,10,cuatrimestral(2013,2)).
cursada(pablitoLescano,electricidadYMagnetismo,9,verano(2014,febrero)).


examenFinal(pepo,matematicaII,4).
examenFinal(pepo,laboratorioDeComputacionII,2).
examenFinal(pepo,sistemasDeProcesamientoDeDatos,6).  % libre


% ******************************TESTS*****************************

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

test(pepo_no_esta_al_dia_con_las_materias,fail) :-
  estaAlDia(pepo).

test(pepo_no_perdio_ninguna_promocion,fail) :-
  perdioPromocion(pepo,_).

test(matematicaIII_es_bloqueada_por_laboratorioDeComputacionII,[true(Materia == laboratorioDeComputacionII),nondet]) :-
  bloquea(Materia,matematicaIII,pepo).

/********** PARTE 2 ***********/
/* PUNTO 8 */
test(pepo_puede_cursar_solo_algorimtosI_y_sistemasOperativos, set(Materias==[algoritmosI,sistemasOperativos])) :-
  puedeCursar(pepo,Materias).

test(pepo_no_recurso_ninguna_materia,fail):-
  recurso(pepo,_).

/* PUNTO 10 */
test(pablitoLescano_encaja_con_el_perfil_de_BuenasCursadas,nondet):-
  buenasCursadas(pablitoLescano).

test(pablitoLescano_encaja_con_el_perfil_de_SeLoQueHicisteElVeranoPasado,nondet):-
  seLoQueHicisteElVeranoPasado(pablitoLescano).

test(pepo_y_pablitoLescano_son_estudiantes_invictus, set(Estudiantes==[pepo,pablitoLescano])):-
  invictus(Estudiantes).

test(la_valoracion_de_laboratorioDeComputacionII_para_pepo_es_3,nondet):-
  desempenioAcademico(pepo,laboratorioDeComputacionII,3).

test(la_valoracion_de_electricidadYMagnetismo_para_pablitoLescano_es_4_coma_5,nondet):-
  desempenioAcademico(pablitoLescano,electricidadYMagnetismo,4.5).

:- end_tests(cursada_universitaria).
