# Premières estimations améliorées

Dernière exécution : 12 octobre 2026.

Ces résultats prolongent la réplication du mémoire. Ils constituent des
diagnostics préliminaires et non encore une estimation causale publiable.

## 1. Effets fixes avec erreurs-types groupées

Le modèle du mémoire est réestimé avec effets fixes année et radar, mais avec
des erreurs-types groupées au niveau du radar.

| Variable | Rayon | Coefficient | Écart-type groupé | p-value |
|---|---:|---:|---:|---:|
| Accidents | 0,5 km | -1,361 | 0,375 | < 0,001 |
| Accidents | 1 km | -2,702 | 0,926 | 0,004 |
| Accidents | 2 km | -4,454 | 2,302 | 0,054 |
| Décès | 0,5 km | -0,004 | 0,011 | 0,690 |
| Décès | 1 km | -0,069 | 0,020 | < 0,001 |
| Décès | 2 km | -0,111 | 0,032 | < 0,001 |

Le groupement augmente fortement l'incertitude sur les accidents : à 2 km,
l'écart-type passe de 1,178 dans la réplication classique à 2,302. Les
coefficients ponctuels restent ceux du mémoire, mais leur précision était donc
surestimée pour plusieurs rayons.

## 2. Étude d'événement TWFE

Une étude d'événement utilise l'année précédant l'installation (`e = -1`)
comme référence. Les erreurs-types sont groupées par radar.

Les coefficients antérieurs à l'installation sont conjointement différents de
zéro pour les accidents :

| Rayon | p-value du test de pré-tendance |
|---:|---:|
| 0,5 km | < 0,001 |
| 1 km | 0,009 |
| 2 km | 0,100 |

À 0,5 et 1 km, le nombre d'accidents diminue donc déjà avant l'installation.
Cela est compatible avec une sélection des emplacements après un niveau ou un
pic d'accidents élevé, suivie d'un retour vers la moyenne. La condition de
tendances parallèles nécessaire à l'interprétation causale du modèle simple est
fragile.

## 3. Comparaison aux radars pas encore installés

Pour éviter de prendre des radars déjà traités comme témoins, chaque cohorte
d'installation est comparée uniquement aux radars installés plus tard. La
variation est calculée par rapport à l'année `g-1`; l'inférence repose sur 499
rééchantillonnages des radars entiers.

### Première année après installation

| Variable | Rayon | Estimation | IC à 95 % | p-value |
|---|---:|---:|---:|---:|
| Accidents | 0,5 km | 0,005 | [-0,239 ; 0,249] | 0,969 |
| Accidents | 1 km | -0,200 | [-0,728 ; 0,328] | 0,459 |
| Accidents | 2 km | -0,962 | [-2,466 ; 0,542] | 0,210 |
| Décès | 0,5 km | 0,011 | [-0,035 ; 0,056] | 0,647 |
| Décès | 1 km | -0,088 | [-0,199 ; 0,022] | 0,118 |
| Décès | 2 km | -0,104 | [-0,272 ; 0,064] | 0,224 |

Aucun effet de première année n'est statistiquement significatif aux rayons les
plus plausibles de 0,5, 1 et 2 km. Les estimations ultérieures sont beaucoup
moins précises, car seules les cohortes anciennes disposent encore d'un groupe
de comparaison non traité.

## Conclusion provisoire

Le résultat négatif du modèle à effets fixes du mémoire n'est pas robuste au
changement de groupe de comparaison. Les premières estimations ne permettent
pas de conclure que les radars réduisent causalement les accidents. Elles ne
permettent pas non plus de conclure à une absence d'effet : la comparaison avec
les radars pas encore installés manque rapidement de support et les données ne
contiennent pas de véritables emplacements jamais traités appariés.

Une version destinée à un article devrait construire un groupe témoin de
segments routiers comparables, intégrer l'exposition au trafic, passer à une
fréquence mensuelle et utiliser les dates exactes d'installation. Les résultats
devraient aussi être séparés selon le type de voie et de radar.

## 4. Panel mensuel avec contrôle spatial local

Une première extension mensuelle conserve uniquement les radars fixes et les
radars discriminants. Sont exclus les radars de feu rouge, les passages à
niveau, les dispositifs d'itinéraire et les radars de vitesse moyenne, dont le
traitement porte sur une section plutôt que sur un point. Parmi 425
installations ponctuelles intervenues entre 2013 et 2018, 282 sont situées à
plus de 2 km de tout autre radar ponctuel de vitesse connu.

Pour chaque radar, le nombre mensuel d'accidents dans le disque de 0 à 500 m est
comparé à la densité observée dans la couronne de 500 m à 1 km, pendant les 24
mois précédant et suivant l'installation. Le mois d'installation est exclu.

| Variable | Effet mensuel moyen | IC à 95 % | p-value |
|---|---:|---:|---:|
| Accidents | -0,0076 | [-0,0279 ; 0,0128] | 0,464 |
| Décès | -0,0006 | [-0,0035 ; 0,0023] | 0,677 |

L'exclusion des mois de confinement de mars à décembre 2020 ne modifie pas les
résultats. Les pré-tendances sont toutefois proches du seuil de 10 % pour les
accidents (`p = 0,099`) et la dynamique ne montre aucune baisse pendant les
douze premiers mois. Cette stratégie contrôle finement les chocs locaux, mais
la couronne peut appartenir à d'autres voies et subir des effets de report.

## 5. Segments témoins appariés sur le trafic pré-traitement

Le second exercice utilise les TMJA officiels du réseau routier national. Pour
chaque radar installé en année `g`, le contrôle est recherché à partir du TMJA
de `g-1`, sur la même route, sans radar connu à moins de 5 km. Les sites sont
appariés sur le trafic avant traitement ; aucun TMJA postérieur à l'installation
n'entre dans le matching.

Les contraintes strictes et la couverture limitée aux autoroutes et routes
nationales conduisent à seulement 23 paires. Le ratio médian entre le trafic du
contrôle et celui du radar vaut 0,935. Les pré-tendances ne sont pas rejetées
(`p = 0,153` pour les accidents).

| Variable | Effet mensuel moyen | IC à 95 % | p-value |
|---|---:|---:|---:|
| Accidents à 500 m | -0,0395 | [-0,1099 ; 0,0310] | 0,258 |
| Décès à 500 m | -0,0104 | [-0,0286 ; 0,0078] | 0,248 |

L'estimation ponctuelle est négative, mais l'échantillon est trop petit pour
établir un effet. Ce résultat constitue une preuve de concept du matching, pas
une estimation finale. Une extension crédible nécessite les comptages des
réseaux départementaux et une géométrie routière complète.

## 6. Échantillon élargi : futurs radars comme témoins

Le matching TMJA strict ne peut pas constituer l'estimation principale avec
seulement 23 paires. Une stratégie complémentaire utilise donc comme témoins
des emplacements appartenant à la même strate de classe de route et de vitesse
maximale autorisée, où un radar sera installé plus tard. Ces emplacements sont
non traités pendant la fenêtre de comparaison et sont, par construction, des
sites jugés comparables par la politique publique.

L'échantillon élargi combine les inventaires 2018 et 2025. Lorsqu'un équipement
2025 correspond à un radar ponctuel déjà observé en 2018, la première des deux
dates est retenue : un remplacement d'appareil n'est ainsi pas codé comme une
nouvelle implantation. Cette correction est substantielle, notamment pour les
dates déclarées en 2019 et 2020.

L'inventaire combiné contient 3 073 radars ponctuels disposant d'une date. Tous
ne constituent pas des traitements utilisables : 1 259 ont été installés entre
2012 et 2024, dont 996 au plus tard en 2022 et donc assez tôt pour offrir jusqu'à
24 mois postérieurs dans les BAAC disponibles. L'exclusion des sites déjà
exposés à un radar voisin et l'exigence d'au moins dix témoins futurs dans la
même strate expliquent ensuite l'essentiel de la réduction d'échantillon. Le
détail de cet entonnoir est enregistré dans
`output/monthly_future_radar_sample_flow.csv`.

Après exclusion des sites ayant déjà un radar de vitesse à moins de 1 km,
l'échantillon comprend 970 sites admissibles : 757 installations entre 2012 et
2022 peuvent être traitées, tandis que 213 implantations de 2023-2024 servent
uniquement de témoins futurs. Une observation est censurée si un radar voisin
apparaît pendant sa fenêtre post-traitement.

Selon l'horizon, entre 372 et 536 radars traités contribuent aux estimations
postérieures, avec 41 à 54 contrôles futurs médians par radar. Les trois
fenêtres antérieures mobilisent 634 radars, avec 59 témoins médians, et sont
toutes proches de zéro.

| Variable | Fenêtre postérieure | Radars traités | Estimation mensuelle | IC à 95 % | p-value |
|---|---:|---:|---:|---:|---:|
| Accidents | mois 1–12 | 467 | 0,0033 | [-0,0117 ; 0,0183] | 0,668 |
| Accidents | mois 1–24 | 372 | 0,0045 | [-0,0096 ; 0,0186] | 0,531 |
| Décès | mois 1–12 | 467 | -0,0019 | [-0,0050 ; 0,0013] | 0,246 |
| Décès | mois 1–24 | 372 | -0,0026 | [-0,0066 ; 0,0014] | 0,208 |

Cette spécification élargie ne détecte aucune diminution moyenne des accidents
ou des décès. Le coefficient de la fenêtre 19-24 mois est positif pour les
accidents (`0,0242`, `p = 0,048`), alors que les fenêtres antérieures et les
autres horizons postérieurs sont proches de zéro. Ce résultat isolé, tout juste
significatif sans correction pour tests multiples, doit être interprété avec
prudence ; il ne soutient pas l'hypothèse d'une baisse durable.

### Sensibilité au rayon

La même spécification est réestimée sans modifier l'échantillon ni les témoins
pour les disques de 250 mètres, 500 mètres et 1 km. Des sensibilités à 2, 3 et 5 km
sont également calculées avec des règles plus strictes : absence de radar
antérieur dans tout le disque, censure lors de l'apparition d'un radar voisin et
distance minimale de respectivement 4, 6 et 10 km entre traité et témoins. Il
reste 373 traitements estimables à 2 km sur douze mois, 295 à 3 km et 192 à
5 km.

| Rayon | Horizon | Accidents mensuels | IC à 95 % | p-value |
|---:|---:|---:|---:|---:|
| 0,25 km | mois 1–12 | -0,0019 | [-0,0114 ; 0,0077] | 0,704 |
| 0,50 km | mois 1–12 | 0,0033 | [-0,0117 ; 0,0183] | 0,668 |
| 1,00 km | mois 1–12 | 0,0104 | [-0,0240 ; 0,0449] | 0,554 |
| 0,25 km | mois 1–24 | 0,0023 | [-0,0080 ; 0,0126] | 0,665 |
| 0,50 km | mois 1–24 | 0,0045 | [-0,0096 ; 0,0186] | 0,531 |
| 1,00 km | mois 1–24 | 0,0199 | [-0,0163 ; 0,0562] | 0,282 |
| 2,00 km | mois 1–12 | -0,0022 | [-0,0496 ; 0,0452] | 0,928 |
| 2,00 km | mois 1–24 | 0,0373 | [-0,0148 ; 0,0894] | 0,160 |
| 3,00 km | mois 1–12 | -0,0103 | [-0,0832 ; 0,0626] | 0,782 |
| 3,00 km | mois 1–24 | -0,0015 | [-0,0647 ; 0,0617] | 0,962 |
| 5,00 km | mois 1–12 | 0,0329 | [-0,1878 ; 0,2536] | 0,770 |
| 5,00 km | mois 1–24 | -0,0792 | [-0,1920 ; 0,0335] | 0,168 |

L'absence de baisse détectable des accidents est donc robuste au rayon, mais
l'incertitude augmente nettement pour les grands disques et aucun effet moyen
n'est significatif. Les décès diminuent ponctuellement pendant la première
année à 2 et 3 km (`p = 0,060` et `p = 0,070`), mais des écarts négatifs sont
déjà présents 19 à 24 mois avant l'installation. Ces coefficients ne satisfont
donc pas un diagnostic élémentaire de pré-tendance.

### Exclusion des grandes villes

Une sensibilité retire les radars situés à moins de 10 km du centre d'une
commune comptant au moins 100 000 habitants. Le filtre exclut 196 des 1 259
sites installés entre 2012 et 2024. Les règles d'isolation et de non-chevauchement
restent adaptées à chaque rayon.

| Rayon | Horizon | Radars traités | Accidents mensuels | IC à 95 % | p-value |
|---:|---:|---:|---:|---:|---:|
| 1 km | mois 1–12 | 405 | 0,0029 | [-0,0245 ; 0,0303] | 0,837 |
| 1 km | mois 1–24 | 322 | 0,0107 | [-0,0133 ; 0,0346] | 0,382 |
| 2 km | mois 1–12 | 342 | -0,0107 | [-0,0447 ; 0,0233] | 0,538 |
| 2 km | mois 1–24 | 263 | 0,0300 | [-0,0134 ; 0,0734] | 0,175 |
| 3 km | mois 1–12 | 271 | -0,0212 | [-0,0572 ; 0,0148] | 0,249 |
| 3 km | mois 1–24 | 212 | 0,0028 | [-0,0506 ; 0,0562] | 0,918 |

L'exclusion des grandes villes ne fait apparaître aucune baisse significative
des accidents. À 3 km, les deux premières fenêtres postérieures sont négatives,
mais l'effet moyen de la première année reste imprécis. Pour les décès à 2 km,
les coefficients postérieurs négatifs sont précédés de différences négatives
significatives dans chacune des trois fenêtres antérieures : ils ne peuvent pas
être interprétés comme un effet causal des radars.

La spécification ne contrôle le trafic qu'indirectement par la classe de route,
la vitesse autorisée et le choix de futurs emplacements de radar. Le matching
TMJA reste donc un contrôle de robustesse externe sur le réseau national.

## 7. Accidents mesurés le long de la route du radar

Les cercles peuvent attribuer au radar des accidents sur des rues parallèles ou
des routes croisées. Une nouvelle mesure rattache donc chaque radar à la couche
`troncon_de_route` de la BD TOPO IGN, puis ne conserve que les accidents situés
à moins de 50 ou 100 mètres de la route numérotée correspondante. Les distances
de 1, 2 et 3 km restent mesurées localement autour du radar. Les grandes villes
sont exclues selon la règle décrite plus haut.

Parmi les 1 259 radars ponctuels datés de 2012 à 2024, 1 165 sont appariés à une
route numérotée. La distance médiane radar–axe est de 6,8 mètres et 90 % des
appariements sont à moins de 23,1 mètres. Après exclusion des grandes villes,
1 012 corridors sont construits avec succès.

### Échantillon principal

| Rayon | Largeur | Radars, mois 1–12 | Effet mensuel accidents | IC à 95 % | p-value | Pré-tendance jointe |
|---:|---:|---:|---:|---:|---:|---:|
| 1 km | ±50 m | 529 | -0,0074 | [-0,0234 ; 0,0085] | 0,362 | 0,630 |
| 1 km | ±100 m | 529 | -0,0068 | [-0,0225 ; 0,0089] | 0,398 | 0,653 |
| 2 km | ±50 m | 427 | -0,0215 | [-0,0344 ; -0,0087] | 0,001 | 0,081 |
| 2 km | ±100 m | 427 | -0,0194 | [-0,0323 ; -0,0065] | 0,003 | 0,122 |
| 3 km | ±50 m | 348 | -0,0070 | [-0,0203 ; 0,0063] | 0,302 | 0,381 |
| 3 km | ±100 m | 348 | -0,0037 | [-0,0179 ; 0,0104] | 0,605 | 0,242 |

À 2 km, la baisse représente environ 40 à 47 % du niveau mensuel de référence
et se concentre sur les douze premiers mois. Elle n'est plus significative sur
l'ensemble des mois 1 à 24. Le test conjoint de pré-tendance est proche de 10 %
avec la bande de 50 mètres, mais ne rejette pas à 10 % avec la bande de 100
mètres. Ce signal est donc prometteur, mais pas encore une preuve définitive.

### Concordances exactes de numéro de route

Une robustesse ne conserve que les radars dont le numéro de route historique
concorde exactement avec la BD TOPO. Sur 210 traitements estimables à 1 km, la
baisse de première année vaut `-0,0124` avec la bande de 50 mètres (`p = 0,019`)
et `-0,0128` avec celle de 100 mètres (`p = 0,016`). Les tests conjoints de
pré-tendance donnent respectivement `p = 0,434` et `p = 0,462`. À 2 km, les
coefficients restent négatifs mais sont seulement marginaux (`p = 0,056` et
`p = 0,076`).

Ces résultats sont les premiers à indiquer une baisse robuste lorsque l'issue
est mesurée sur la route effectivement surveillée. Ils doivent encore être
soumis à une mesure de distance le long du graphe routier, à des corrections
pour tests multiples et à des robustesses par cohorte.

## 8. Estimateur doublement robuste sur les corridors routiers

Une extension applique un estimateur AIPW aux mêmes comparaisons empilées. Pour
chaque radar traité, les témoins sont des emplacements de la même strate
route--vitesse qui seront équipés plus tard et dont le corridor ne chevauche
pas celui du radar traité. Le modèle de propension et le modèle de variation
de l'issue utilisent les accidents moyens des mois 24--13, 12--7 et 6--1 avant
l'installation, l'importance et le caractère urbain de la route, sa vitesse
moyenne, la strate d'appariement et le calendrier d'installation. Les placebos
antérieurs sont estimés sans inclure l'issue de la fenêtre évaluée dans ces
modèles auxiliaires.

### Première année après installation

| Rayon | Largeur | Radars | Estimation DR | IC à 95 % | p-value | Pré-tendance jointe |
|---:|---:|---:|---:|---:|---:|---:|
| 1 km | ±50 m | 529 | -0,0082 | [-0,0199 ; 0,0035] | 0,170 | 0,630 |
| 1 km | ±100 m | 529 | -0,0073 | [-0,0193 ; 0,0047] | 0,233 | 0,635 |
| 2 km | ±50 m | 427 | -0,0180 | [-0,0279 ; -0,0081] | < 0,001 | 0,062 |
| 2 km | ±100 m | 427 | -0,0168 | [-0,0275 ; -0,0062] | 0,002 | 0,094 |
| 3 km | ±50 m | 348 | -0,0085 | [-0,0188 ; 0,0019] | 0,108 | 0,407 |
| 3 km | ±100 m | 348 | -0,0074 | [-0,0186 ; 0,0038] | 0,193 | 0,290 |

À 2 km et avec la bande de 100 mètres, les estimateurs calculés sur les mêmes
comparaisons sont proches : `-0,0194` pour le DiD non ajusté, `-0,0170` pour la
régression de l'issue, `-0,0167` pour la pondération inverse et `-0,0168` pour
l'AIPW. Aucun score de propension n'est tronqué et le radar traité médian
dispose de 48 témoins effectifs. La baisse DR représente environ 35 % du niveau
mensuel de référence. Elle est similaire avec la bande de 50 mètres.

Le signal reste néanmoins concentré sur la première année : sur les mois 1--24,
l'estimation à 2 km vaut `-0,0045` avec la bande de 100 mètres (`p = 0,385`) et
`-0,0060` avec celle de 50 mètres (`p = 0,218`). De plus, les tests joints des
trois fenêtres antérieures sont proches du seuil de 10 % à 2 km, surtout avec
la bande étroite. La stabilité entre méthodes est rassurante, mais ne remplace
donc pas la validation de l'hypothèse de tendances parallèles.

Cette implémentation est une version empilée et localement appariée de l'AIPW,
adaptée aux ensembles de témoins propres à chaque radar. Elle n'est pas encore
la version finale de l'inférence : les modèles auxiliaires sont maintenus fixes
dans le bootstrap et ne sont pas estimés par échantillonnage croisé. Avant de
faire de ce résultat l'estimation principale, il faudra au minimum ajouter une
validation croisée par site ou cohorte, des erreurs-types tenant compte de
l'incertitude des modèles auxiliaires, des contrôles de sensibilité au support
commun et une correction pour la sélection parmi plusieurs rayons.

## Fichiers produits

- `output/estimates_clustered_twfe.csv`
- `output/event_study_twfe.csv`
- `output/event_study_diagnostics.csv`
- `output/event_study_not_yet_treated.csv`
- `output/panel_monthly_local_control.csv.gz`
- `output/estimates_monthly_local_control.csv`
- `output/event_study_monthly_local_control.csv`
- `output/matched_traffic_segments.csv`
- `output/panel_monthly_matched_traffic.csv.gz`
- `output/estimates_monthly_matched_traffic.csv`
- `output/event_study_monthly_matched_traffic.csv`
- `output/monthly_future_radar_sample.csv`
- `output/monthly_future_radar_sample_flow.csv`
- `output/event_study_monthly_future_radar_controls.csv`
- `output/estimates_monthly_future_radar_controls.csv`
- `output/estimates_monthly_future_radar_controls_by_radius.csv`
- `output/event_study_monthly_future_radar_controls_by_radius.csv`
- `output/event_study_monthly_accidents.png`
- `output/event_study_future_radars_by_radius.png`
- `output/event_study_future_radars_large_radii.png`
- `output/estimates_monthly_future_radar_controls_by_radius_no_large_cities.csv`
- `output/event_study_monthly_future_radar_controls_by_radius_no_large_cities.csv`
- `output/event_study_no_large_cities_1_2_3km.png`
- `output/radar_ign_road_matches.csv`
- `output/road_corridor_geometry_diagnostics.csv`
- `output/estimates_monthly_road_corridor.csv`
- `output/estimates_monthly_road_corridor_100m.csv`
- `output/pretrend_tests_monthly_road_corridor.csv`
- `output/event_study_road_corridors.png`
- `output/estimates_monthly_road_corridor_dr.csv`
- `output/estimates_monthly_road_corridor_dr_100m.csv`
- `output/event_study_monthly_road_corridor_dr.csv`
- `output/event_study_monthly_road_corridor_dr_100m.csv`
- `output/diagnostics_monthly_road_corridor_dr.csv`
- `output/diagnostics_monthly_road_corridor_dr_100m.csv`
- `output/pretrend_tests_monthly_road_corridor_dr.csv`
- `output/pretrend_tests_monthly_road_corridor_dr_100m.csv`

Les scripts complets sont `02_improved_estimates.R`,
`03_monthly_local_control.R`, `04_monthly_matched_traffic.R`,
`05_plot_monthly_results.R`, `06_monthly_future_radar_controls.R`,
`07_compare_future_radar_radii.R` et `08_plot_future_radar_radii.R`.
La figure hors grandes villes est produite par `09_plot_no_large_cities.R`.
Les corridors routiers sont construits par `10_match_radars_to_ign_roads.R` et
`11_monthly_road_corridor_did.R`, puis représentés par
`12_plot_road_corridor_results.R`. Leur extension doublement robuste est
estimée par `13_doubly_robust_road_corridor_did.R`.
