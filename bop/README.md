# Mostres del Baròmetre d'Opinió Política (BOP)

Una de les enquestes que realitza el Centre d'Estudis d'Opinió és el Baròmetre d'Opinió Política (BOP). El BOP és una enquesta de caràcter periòdic que té com a principal objectiu conèixer la percepció de la societat catalana sobre política, economia, mitjans de comunicació, comportament electoral i la valoració de líders polítics.

## Descripció general

El codi extreu les mostres de les unitats primàries de mostreig (seccions censals) on es duran a terme les enquestes presencials del BOP.

El codi genera un total de `NMOSTRES` aleatòries (valor que es pot canviar a partir del fitxer de configuració `config/config.yaml`). A partir d'aquestes `NMOSTRES` es fa una anàlisi de diferents característiques de la mostra, com ara el nombre de municipis que es visiten, la distància en kilòmetres en cotxe que hi ha des del CEO, els resultats de les últimes eleccions del Parlament de Catalunya, variables sociodemogràfiques i la distribució de les províncies. Finalment, s'escull la mostra que s'ajusti correctament amb els valors de la població. 

Cal tenir en compte que en ser extraccions aleatòries, **el codi MAI reprodueix una mostra utilitzada en el CEO**.


## Estructura del codi

- `src` conté tots els scripts d'R. L'objectiu de cada script es detalla a l'apartat [descripció dels scripts](#descripció-dels-scripts).
- `dta` conté les carpetes on es guarden els fitxers d'entrada i de sortida. Les dades que s'usen per al mostreig es troba a `dta/raw-dta` i els fitxers resultants es guarden a `dta/output`.
- `config` conté un fitxer `config.yaml` que defineix les variables de configuració que s'usen en tot el codi. Hi ha ubicacions de les carpetes principals, nombre de seccions que s'escullen per la mostra i d'altres necessàries per calcular diferents estadístics.


## Descripció dels scripts

- `00-data_cleaning.R` llegeix diferents dades en brut que es transformen per tal de poder analitzar les característiques de les seccions censals i poder dibuixar els resultats en un mapa.
- `01-sample.R` realita el mostreig aleatori estratificat. Els estrats són definits mitjançant l'algoritme _k-means_, identificant clústers per similitud electoral. En cada estrat, s'extreu una mostra de seccions censals proporcional al pes demogràfic de l'estrat. La probabilitat d'inclusió de cada secció a la mostra final és proporcional al cens de la secció. En l'script es guarden diferents fitxers per un futur ús o per a fer una anàlisi més exhaustiva.
- `02-analysis.R` realitza una anàlisi dels resultats de la mostra. De cada estadístic que es vol que s'ajusti als valors poblacionals, es fa una puntuació a partir del nombre de mostres extretes. La mostra que tingui millor classificació (menor puntuació), és l'escollida per realitzar l'enquesta.
- `03-figures-mostra.R` extreu la informació necessària perquè es duguin a terme les enquestes i es dibuixa en el mapa les seccions censals escollides.

