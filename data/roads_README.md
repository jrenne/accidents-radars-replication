# Réseau routier utilisé pour les corridors

Source : **BD TOPO® version 3, couche `BDTOPO_V3:troncon_de_route`**, produite
par l'Institut national de l'information géographique et forestière (IGN).

- service : API Géoplateforme de diffusion d'objets WFS 2.0 ;
- point d'accès : `https://data.geopf.fr/wfs/ows` ;
- date de récupération : 12 octobre 2026 ;
- système de coordonnées demandé : `CRS:84` ;
- licence : Licence Ouverte / Open Licence version 2.0.

Les scripts ne téléchargent pas la France entière. Ils interrogent le service
autour de chaque radar, d'abord dans un rayon de 350 mètres pour identifier la
route numérotée, puis dans une emprise couvrant 3,5 km autour du radar pour
reconstituer le corridor local. Les résultats dérivés et les diagnostics sont
conservés dans `replication-master/output/` ; les géométries brutes ne sont pas
redistribuées.

Le script `10_match_radars_to_ign_roads.R` réalise le rattachement radar–route.
Le script `11_monthly_road_corridor_did.R` construit les comptages mensuels et
estime le diff-in-diff.
