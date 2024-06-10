escribio(elsaBornemann, socorro).
escribio(neilGaiman, sandman).
escribio(alanMoore, watchmen).
escribio(neilGaiman, americanGods).
escribio(neilGaiman, buensoPresagios).
escribio(terryPratchett, buensoPresagios).
escribio(brianAzarello, cienBalas).
escribio(warenElis, planetary).
escribio(frankMiller, elCaballeroOscuroRegresa).
escribio(frankMiller, batmanAnioUno).
escribio(isaacAsimov, fundacion).
escribio(isaacAsimov, yoRobot).
escribio(isaacAsimov, elFinDeLaEternidad).
escribio(isaacAsimov, laBusquedaDeLosElementos).
escribio(joseHernandez, martinFierro).
escribio(stephenKing, it).
escribio(stephenKing, misery).
escribio(stephenKing, carrie).
escribio(stephenKing, elJuegoDeGerald).
escribio(julioCortazar, rayuela).
escribio(jorgeLuisBorges, ficciones).
escribio(jorgeLuisBorges, elAleph).
escribio(horacioQuiroga, cuentosDeLaSelva).
escribio(horacioQuiroga, cuentosDeLocuraAmorYMuerte).

% ¿Es cierto que alguien escribió una determinada obra?
alguienEscribio(Obra) :- escribio(_, Obra).

% ¿Quién o quienes escribieron una obra?
quienesEscribieron(Alguien, Obra) :- escribio(Alguien, Obra).

% ¿Qué obra escribió cierta persona?
queObraEscribio(Alguien, Obra) :- escribio(Alguien, Obra).

% Si es cierto que cierta persona escribió alguna obra, sin importar cual.
esEscritor(Alguien) :- escribio(Alguien, _).

% Si es cierto que cierta obra existe.
existeObra(Obra) :- escribio(_, Obra).

esComic(sandman).
esComic(cienBalas).
esComic(watchmen).
esComic(planetary).
esComic(elCaballeroOscuroRegresa).
esComic(batmanAnioUno).

esArtistaDeNovenoArte(Artista) :- escribio(Artista, Obra), esComic(Obra).

odia(platon, diogenes).

numero(1).
numero(2).
numero(3).
numero(4).
numero(5).

% siguienteNoInversible(N,S) :- S is N + 1.
siguiente(N,S) :- numero(N), S is N + 1.

programaEn(maria, cobol).
programaEn(maria, java).
programaEn(mario, cobol).
programaEn(mario, python).
programaEn(jose, cobol).
programaEn(jorge,java).
programaEn(jorge,python).

% ¿Es cierto que alguien programa en Cobol?
alguienProgramaEn(Programa) :- programaEn(_, Programa).

% ¿Quién o quienes programan en Cobol?
quienesProgramanEn(Alguien, Programa) :- programaEn(Alguien, Programa).

% ¿Qué lenguajes maneja María?
enQuePrograma(Alguien, Programa) :- programaEn(Alguien, Programa).

% ¿Maria y Mario son colegas?
sonColegas(P1, P2) :- programaEn(P1, Programa), programaEn(P2, Programa).

% ¿Quienes son colegas de lenguajes?
colegas(P1, P2) :- programaEn(P1, Programa), programaEn(P2, Programa), P1 @< P2.

esHijoDe(carlosIII, isabelII).
esHijoDe(ana, isabelII).
esHijoDe(andrew, isabelII).
esHijoDe(edward, isabelII).
esHijoDe(louise, edward).
esHijoDe(james, edward).
esHijoDe(eugenia, andrew).
esHijoDe(beatriz, andrew).
esHijoDe(sienna, beatriz).
esHijoDe(august, eugenia).
esHijoDe(zara, ana).
esHijoDe(peter, ana).
esHijoDe(enrique, carlosIII).
esHijoDe(guillermo, carlosIII).
esHijoDe(jorge, guillermo).
esHijoDe(carlota, guillermo).
esHijoDe(luis, guillermo).
esHijoDe(sabannah, peter).
esHijoDe(isla, peter).
esHijoDe(lucas, zara).
esHijoDe(lena, zara).
esHijoDe(mia, peter).
esHijoDe(archie, enrique).
esHijoDe(lilibeth, enrique).

% ¿Es cierto que la reina Isabel tiene hijos?	
% esHijoDe(_,isabelII).

% ¿Quién o quienes son hijos de la reina Isabel?
% esHijoDe(Hijo,isabelII).

% ¿Quiénes son los nietos del Carlos III?
% esNietoDe(carlosIII, Nieto).

esNietoDe(Persona, Nieto) :- esHijoDe(Hijo, Persona), esHijoDe(Nieto, Hijo).

% ¿Es cierto que Archie es primo del príncipe Jorge?
% esPrimoDe(archie, jorge).
% 
% ¿Quienes son primos?
% esPrimoDe(P1, P2).

sonHermanos(P1, P2):- esHijoDe(P1, Padre), esHijoDe(P2, Padre).

esPrimoDe(P1, P2) :- esHijoDe(P1, Padre1), esHijoDe(P2, Padre2), P1 @< P2, sonHermanos(Padre1, Padre2), Padre1 \= Padre2.

No se tiene inversibilidad si:
Hechos con variables.
Comparación por distinto
Negación (dado que no liga variables)
>, >=, <, <=
Is/2
