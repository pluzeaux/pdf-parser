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

## Usage

## Contact
Si vous détectez un bug, une piste d'amélioration ou une question sur ce projet vous pouvez me contacter via une « Pull request » GitHub, ou directement par mail [Philippe LUZEAUX](mailto:philippe.luzeaux@gmail.com).

Thanks and enjoy !