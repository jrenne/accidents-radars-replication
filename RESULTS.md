# Résultats de la réplication reconstruite

## Conclusion

Les résultats du mémoire sont **reproductibles à une très faible tolérance** à partir des
données conservées, même si le script final de l'étudiant manque. La concordance est
particulièrement forte pour les rayons de 0,5 km et 2 km, pour lesquels les échantillons
reconstruits ont exactement la même taille que dans les sorties originales.

## Comparaison des principaux coefficients

| Variable | Modèle | Rayon | Mémoire | Réplication |
|---|---|---:|---:|---:|
| Accidents | Effets fixes année | 0,5 km | 0,462 | 0,462 |
| Accidents | Effets fixes année + radar | 0,5 km | -1,358 | -1,361 |
| Accidents | Effets fixes année + radar | 2 km | -4,453 | -4,454 |
| Décès | Effets fixes année + radar | 1 km | -0,065 | -0,069 |
| Décès | Effets fixes année + radar | 2 km | -0,111 | -0,111 |
| Accidents | OLS | 1 km | 2,857 | 2,857 |
| Accidents | OLS | 2 km | 4,236 | 4,238 |
| Décès | OLS | 2 km | 0,064 | 0,064 |

Le fichier `output/comparison_with_thesis.csv` contient les trente comparaisons de
coefficients et d'erreurs-types. Sur les 30 coefficients d'intérêt, 22 diffèrent d'au plus
0,01 et tous diffèrent de moins de 0,05. Les 30 erreurs-types sont reproduites à moins de
0,01 près.

## Tailles d'échantillon

| Rayon | Mémoire | Réplication | Différence en radars |
|---:|---:|---:|---:|
| 0,5 km | 5 080 | 5 080 | 0 |
| 1 km | 4 800 | 4 816 | +2 |
| 2 km | 4 248 | 4 248 | 0 |
| 5 km | 2 680 | 2 688 | +1 |
| 7 km | 1 920 | 1 904 | -2 |

Chaque radar contribue huit observations : les neuf années 2013–2021 moins son année
d'installation. Les petits écarts à 1, 5 et 7 km sont compatibles avec des différences
mineures entre la copie historique de l'inventaire des radars utilisée en 2023 et la version
harmonisée aujourd'hui.

## Contrôle du radar nº 13618

Pour l'exemple descriptif du mémoire, les nombres d'accidents et de décès sont reproduits
exactement de 2010 à 2021, à une exception près : 24 accidents sont retrouvés en 2015
contre 25 dans le mémoire. En revanche, les années 2005–2009 ne sont pas reproductibles
avec les coordonnées observées conservées : leur couverture géographique est très
incomplète, et 2009 ne comporte pratiquement aucune coordonnée exploitable. Cette limite
n'affecte pas les régressions finales, qui utilisent 2013–2021.

## Interprétation prudente

La reproduction numérique ne valide pas automatiquement l'interprétation causale. Le
modèle à effets fixes compare l'évolution avant et après installation, mais ne contrôle pas
une éventuelle sélection des emplacements en fonction de tendances locales d'accidents.
Une reprise destinée à un article devrait notamment ajouter une étude d'événement, des
erreurs-types groupées au niveau du radar, des tests de pré-tendances et une stratégie
explicite pour les radars voisins.
