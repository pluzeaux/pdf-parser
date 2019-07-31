# pdf-parser

## Description
Ce projet de parser de fichier PDF permet le découpage de contenu textuel d'un document PDF en paragraphes et tableaux. 
Le résultat du traitement est un fichier JSON contenant l'ensemble du contenu du document selon l'arborescence :

> - document
>> - pages
>>> - page
>>>> - paragraphes
>>>> - tables

Ce projet est développé en Scala 2.12.8 et s'appuie sur les librairies :
> - [pdfbox](https://pdfbox.apache.org/) 2.0.13  outil Java open source pour travailler avec des documents PDF. 
> - [play-json](https://www.playframework.com/documentation/2.7.x/ScalaJson) 2.7.3 analyse et génération de données au format JSON (JavaScript Object Notation).

## Project structure
Ce projet est découpé en parties distinctes correspondant à des préoccupations particulières :

> * ```src/main/scala/json```
* Les classes "wrapper" des objets json à écrire

> * ```src/main/scala/model```
* Les classes modélisant les différents éléments de texte :
> - caractère
> - ligne
> - bloc de texte
> - paragraphe
> - table


> * ```src/main/scala/parser```
* L'implémentation des algorithmes nécessaires au parsing du PDF :
> - Une classe "TextStripper" d'extraction du texte à partir d'un stream PDF (contenu encrypté)
> - Une classe d'écriture permettant la reconnaissance des différents éléments textuels et de les convertir au format JSON



## Compilation

La compilation du projet se fait avec l'outil [SBT](https://www.scala-sbt.org/) et le plugin [sbt-assembly](https://github.com/sbt/sbt-assembly)

### Téléchargement du projet
> `$ git clone https://github.com/pluzeaux/pdf-parser`

### Compilation du projet
> `$ cd ./pdf-parser`

> `$ sbt assembly`

Le fichier jar exécutable se trouve dans le répertoire target du projet.


## Usage

> `$ java -jar target/scala-2.12/pdf-parser-assembly-0.1.0-SNAPSHOT.jar <path-to-pdf-directories>`

Idéalement l'organisation des fichiers pdf doit se faire selon l'organisation suivante :

> /<path-to-pdf-directory>

>> /pdf

>>> /Danone

>>>> DDR_2018.pdf

>>>> DDR_2017.pdf

>>> /Thales
>>>> DDR_2018.pdf

>>>> DDR_2017.pdf

>>> ...

## Considérations technique
La classe la plus importante est `JsonDocumentWriter`, elle porte le coeur de l'algorithme.
En entrée de cette classe nous avons une  liste de caractère issue du `Stripper`,
chaque caractère est défini par :
> ses coordonnées (place dans le document) `x` et `y`

> la police du caractère

> sa dimension `height` et `width`

> son code unicode

> taille de l'espace entre 2 lettres

Nous devons nous limiter uniquement à ces données et ne pas prendre en compte des métadonnées du PDF, car les PDF traités sont de qualité très variable.
Ce traitement se voulant générique nous devons prendre en compte le sous-ensemble commun des informations disponibles à tous les PDF.

Le traitement va réaliser un découpage de cette liste de caractère en :

> lignes

> blocs

>> paragraphes

>> tables

suivant certains critères définis par le traitement.

### Caractère

Le caractère est encapsulé par la classe `TextPosition`

### Ligne

La rupture de ligne est détectée si l'intersection des alignements horizontaux des caractères de la ligne et du caratère suivant est vide.

voir les fonctions `within` et `overlap`


### Ligne tabulée

Une ligne tabulée est détectée si l'espace entre deux mots est supérieures à une certaine valeur (taille de l'espace blanc moyen + delta)

voir la classe `Tabular`

### Paragraphe

Un paragraphe est détecté si l'espace entre deux lignes est supérieur à une certaine valeur (taille normale de l'espace entre deux lignes + delta)

voir la classe `Paragraph`


### Table

Une table est une liste de ligne tabulées ayant le même nombre de colonne. Une colonne est le nombre d'espaces de type tabulation (voir `Ligne tabulée` ci-dessus)

Attention : Un paragraphe peut-être inclus dans une table dans le cas d'entête de ligne d'une table sur plusieurs lignes et ne contenant pas de valeurs dans ses cellules.
voir la fonction `filterBlocks`

### Bloc

Un bloc encapsule aussi bien un paragraphe q'une table



## Contact
Si vous détectez un bug, une piste d'amélioration ou une question sur ce projet vous pouvez me contacter via une « Pull request » GitHub, ou directement par mail [Philippe LUZEAUX](mailto:philippe.luzeaux@gmail.com).

Thanks and enjoy !