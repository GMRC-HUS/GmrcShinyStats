# GMRC Shiny Stats

[![R-CMD-check](https://github.com/GMRC-HUS/GmrcShinyStats/actions/workflows/R-CMD-check/badge.svg)](https://github.com/GMRC-HUS/GmrcShinyStats/actions/workflows/R-CMD-check)

**GMRC Shiny Stats** permet de réaliser facilement des analyses biostatistiques,
sans aucune connaissance en programmation. L'application est développée par le
Groupe Méthode en Recherche Clinique (GMRC) des Hôpitaux Universitaires de
Strasbourg et est conçue pour la formation en biostatistiques.

## Fonctionnalités

- **Descriptifs** : description univariée des variables quantitatives (moyenne,
  médiane, écart-type, quantiles, test de normalité) et qualitatives
  (effectifs, proportions, diagrammes).
- **Croisements / Inférence** : tableaux croisés, tests d'association
  (Chi2, Fisher) avec aide au choix du test approprié, corrélation
  (Pearson, Spearman).
- **Analyse de survie** : courbes de Kaplan-Meier, comparaison inter-groupes
  (test du Log-Rank).
- **Tests diagnostiques** : courbes ROC, seuil optimal (indice de Youden),
  sensibilité, spécificité, valeurs prédictives.
- **Concordance** : coefficient Kappa de Cohen avec intervalle de confiance
  par bootstrap.
- **Rédaction** : aide à la rédaction de la partie « Matériel et Méthodes ».

Deux modes de travail sont proposés : à partir d'une base de données importée
(fichier `.csv`) ou par saisie manuelle.

## Lien vers la chaîne YouTube

Des tutoriels en vidéo sont disponibles :
<https://youtube.com/playlist?list=PLRC56KyFX6kk3FJ6FuwvTFRd3ZoNFSLlG>

## Procédure d'installation

### Téléchargement du logiciel R

Télécharger R au lien suivant : <https://cloud.r-project.org/>

### Utilisateur Windows : installation de rTools

<https://cran.r-project.org/bin/windows/Rtools/>

### Installation de l'application et mise à jour

Lancer le logiciel R, puis coller les lignes de code suivantes :

```r
if (!require(remotes)) install.packages("remotes", quiet = TRUE)

remotes::install_github("GMRC-HUS/GmrcShinyStats", dep = TRUE,
                        INSTALL_opts = c("--no-lock"))
```

Après quelques minutes d'installation, l'application peut être lancée.

### Lancement du logiciel

Lancer le logiciel R, puis coller la ligne de code suivante :

```r
GmrcShinyStats::run_app()
```

---

### Citation

Thibaut Fabacher, Michael Schaeffer, Nicolas Tuzin, François Séverac,
François Lefebvre, Marie Mielcarek, Erik-André Sauleau, Nicolas Meyer,
Julien Godet. Biostatistiques médicales avec GMRC Shiny Stats - un outil de
formation par la pratique, Annales Pharmaceutiques Françaises, 2020.
ISSN 0003-4509, <https://doi.org/10.1016/j.pharma.2020.06.001>.

---

### License

This package is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License, version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the GNU
General Public License for more details.

A copy of the GNU General Public License, version 3, is available at
<https://www.r-project.org/Licenses/GPL-3>
