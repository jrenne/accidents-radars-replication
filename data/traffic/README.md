# Données de trafic routier

Ces fichiers proviennent du jeu officiel « Trafic moyen journalier annuel sur
le réseau routier national », diffusé par le ministère de la Transition
écologique sous Licence Ouverte 2.0 :

<https://www.data.gouv.fr/datasets/trafic-moyen-journalier-annuel-sur-le-reseau-routier-national>

Les millésimes 2011 à 2019 servent aux analyses pré-traitement : pour un radar
installé pendant l'année `g`, le script utilise exclusivement le TMJA de
`g - 1`. Le millésime 2011 permet ainsi de couvrir les installations de 2012.
La série nationale ne publie pas de ressources 2020 et 2021 sur la page source;
les cohortes 2021 et 2022 ne peuvent donc pas recevoir un TMJA strictement
antérieur d'un an dans cette analyse.

| Fichier | SHA-256 |
|---|---|
| `tmja_2011.csv` | `576577c62bd751bc41221fc860c3bca3ab67ffcf80d2fbf9265ec8a296c396ac` |
| `tmja_2012.csv` | `245312481dc84fe0ef0509ebdf0a37ed96788c9c8d5710f5f4682fd9dfde9c84` |
| `tmja_2013.csv` | `b0ea529f453d5e097f67ac34fba7d3bf26a33b859410d878d1568a6c1de661b8` |
| `tmja_2014.csv` | `acb63e1586f1765cb7d2c26324d48036d477f8f07b9ba0ebb8145be9c1d26401` |
| `tmja_2015.csv` | `3576f59b72427e4551a1685bfd7d45787b1cccebb9577939719a0302711e39e8` |
| `tmja_2016.csv` | `c9d2431b43cb9fe61270c61b37513eb7aac89b98bbc3925dafb744eac4f48e65` |
| `tmja_2017.csv` | `f3128f63b3b9bfa04d7d732d0f3dbdd991a29a37e3b8a2641838d3a8d95351f5` |
| `tmja_2018.csv` | `229b4b77d5530c8e6a7d026415b2dc85d6c7b1e9f5f9c345573a380e66d2fc89` |
| `tmja_2019.csv` | `9f3967d84f85eaf84da0b996ae916fbbdcc89a05fb4668827b8a732991c29b34` |

Le TMJA est un nombre moyen de véhicules par jour, tous sens confondus. Les
coordonnées des extrémités de section sont en Lambert-93. La couverture est
limitée au réseau routier national : elle ne fournit donc pas un contrôle
homogène pour les routes départementales et communales.

Les archives géométriques officielles téléchargées le 14 octobre 2026 sont
conservées dans `shapefiles/` et lues directement par le script 29b. Leurs
empreintes SHA-256 sont les suivantes :

| Archive | SHA-256 |
|---|---|
| `tmja_2011_shp.zip` | `ce083cdf5ed7eddccdc21e2692d0d58863e7268a6d71a49a672c66f4add7370f` |
| `tmja_2012_shp.zip` | `6460b34e8c115e433463dc81cc61fba568fbd212f0119f54d1995f334727da13` |
| `tmja_2013_shp.zip` | `b5754c369802886e84ba1a047fa97c3fe5c623a8f648a0ccd015fe73d170304e` |
| `tmja_2014_shp.zip` | `4a4e0c29cf1cd49f93d4525f52bb20f6ef08dd1c0e480932eb3d4031289e2178` |
| `tmja_2015_shp.zip` | `437f5121e19baaffe3f7fac3aa385274b428be1857bd426fced4211cd7482740` |
| `tmja_2016_shp.zip` | `4f820a531a02652ce6e6cb3ca571fcb466c7ad1be07f2015d317b7c0467895ef` |
| `tmja_2017_shp.zip` | `9dd6db336431ace19fbf9ba5d4e7f376d4159ec880bb1a30e5fda9565d99b8fb` |
| `tmja_2018_shp.zip` | `f020506528c950862c35d4921cb876d7eef98967cb9a84f572e12e6fd3eb1553` |
| `tmja_2019_shp.zip` | `a2ddf2761490879b8997a1ac2149cd7f4348df2483e13ecfd499ba8c81c2564f` |
