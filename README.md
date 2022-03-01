# Mostres
Codi per extreure les mostres de les unitats primàries de mostreig (seccions censals) a les enquestes presencials del CEO.

El codi genera una mostra de seccions censals d'exemple. En ser una extracció aleatòria, NO és una extracció utilitzada en cap enquesta del CEO.

Definim les tipologies a partir de 6 clusters basats en els resultats de les darreres eleccions al Parlament.

Extreiem una mostra estratificada per tipologia de secció censal. Cada secció té una probabilitat d'inclusió a la mostra proporcional a la seva mida.

El codi avalua la mostra, comparant-la amb la distribució de resultats electorals i característiques sociodemogràfiques.

També representa la mostra en un mapa.

També identifica les seccions adjacents que pertanyen al mateix cluster, que es poden emprar com a substitutes.
