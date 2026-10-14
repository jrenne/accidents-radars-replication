# Recherche d'un historique des radars fixes

Dernière vérification : 11 octobre 2026.

## Résultat exploitable

Le jeu officiel « Liste des radars fixes en France » conserve deux ressources
complètes comparables :

| Millésime retenu | Publication | Nombre d'identifiants uniques |
|---|---:|---:|
| `2024-10` | 18 octobre 2024 | 3 268 |
| `2025-12` | 30 décembre 2025 | 3 309 |

Après harmonisation des identifiants, 3 174 radars figurent dans les deux
ressources, 94 figurent seulement en 2024 et 135 seulement en 2025. Les 135
entrées ont toutes une date de mise en service déclarée en 2024 ou en 2025.

Ces deux inventaires sont intégrés à la table
`radars_inventaires_officiels`. Les expressions « entrée » et « sortie »
signifient uniquement présence ou absence dans une ressource publiée. Une
sortie ne doit pas être interprétée comme une fermeture définitive sans source
complémentaire.

Source officielle :
<https://www.data.gouv.fr/datasets/liste-des-radars-fixes-en-france>

Métadonnées de l'API data.gouv.fr :
<https://www.data.gouv.fr/api/1/datasets/liste-des-radars-fixes-en-france/>

## Ce qui reste manquant

La page officielle a été créée en octobre 2024. Elle ne fournit donc pas de
photographies annuelles localisées pour 2019-2023. Les bilans annuels de
l'ONISR donnent des effectifs nationaux agrégés du parc, mais ils ne permettent
pas de reconstituer l'emplacement ni la date de retrait de chaque dispositif.

Le dépôt `1e1/Open-GATSO-POI`, qui interrogeait l'ancienne API officielle, a
également été examiné. Son historique Git conserve le code de collecte mais pas
les exports générés : ceux-ci étaient supprimés après chaque construction. Il
ne fournit donc pas les photographies intermédiaires recherchées.

## Conséquences pour l'étude

1. Les dates de mise en service déclarées peuvent servir à définir l'arrivée
   d'un radar, sous réserve de validation des cas atypiques.
2. Les inventaires 2024 et 2025 permettent une analyse de cohérence et une
   extension récente, mais pas un historique complet du statut opérationnel.
3. Une étude d'événement ne doit pas coder un radar comme actif chaque année
   après son installation si sa date de retrait ou ses interruptions sont
   inconnues.
4. L'étape prioritaire est d'obtenir auprès de la Délégation à la sécurité
   routière des extractions annuelles localisées (au minimum 2013-2025), avec
   identifiant stable, type, coordonnées, dates de mise en service, retrait et
   éventuelles périodes d'indisponibilité.

Au 11 octobre 2026, le jeu BAAC officiel reste intitulé « Années de 2005 à
2024 » : les accidents 2025 ne sont pas encore diffusés. Les 102 radars dont la
mise en service déclarée date de 2025 ne peuvent donc pas encore être évalués
avec les microdonnées annuelles définitives.

## Pistes suivantes

- demander directement au producteur data.gouv.fr les anciens exports de l'API ;
- déposer, si nécessaire, une demande d'accès aux documents administratifs ;
- retenter l'exploration d'archives du Web pour les anciens exports de l'API ;
- utiliser les effectifs ONISR comme contrôle agrégé, jamais comme substitut
  d'un inventaire localisé ;
- valider manuellement les entrées et sorties 2024-2025 avant toute estimation.
