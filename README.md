# StereoMorph

StereoMorph is an R package for the collection of shape data using a stereo camera setup.

# Introduction

Standard digital cameras are commonly used to collect 2D landmarks and curves from relatively flat objects. But if the objects are more three-dimensional, users often have to resort to methods such as microscribes or surface scanning. The StereoMorph R package allows users to collect 3D landmarks and curves from objects using two standard digital cameras. Owing to its low cost, portability and speed, stereo camera reconstruction is an ideal method for collecting data from a large number of specimens or objects.

The StereoMorph package includes tools for automatically calibrating a set of two cameras using a checkerboard pattern and a digitizing application for digitizing landmarks and curves. Once the cameras are calibrated, StereoMorph allows users to reconstruct any landmarks and curves digitized in both camera views into 3D. Those interested in using StereoMorph will find several useful resources at https://aaronolsen.github.io/software/stereomorph.html, including step-by-step tutorials and accompanying project folders with all the files needed for the tutorial.

# Citing StereoMorph
To cite StereoMorph in a publication, please use:

- Olsen Aaron M, Mark W Westneat. 2015. StereoMorph: an R package for the collection of 3D landmarks and curves using a stereo camera set-up. Methods in Ecology and Evolution 6:351-356. DOI: 10.1111/2041-210X.12326.

- Olsen, AM & A Haber (2017). StereoMorph: Stereo Camera Calibration and Reconstruction. Version 1.6.1. https://CRAN.R-project.org/package=StereoMorph.

# License
Shield: [![CC BY-SA 4.0][cc-by-sa-shield]][cc-by-sa]

This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: https://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg
