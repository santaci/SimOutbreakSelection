# SimOutbreakSelection

[![DOI](https://zenodo.org/badge/213929000.svg)](https://doi.org/10.5281/zenodo.15634447)

#### !!! ``SOS`` is still a WIP.  Please be mindful that there are paths that require the user to manually change. !!!

## Table of Contents
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Citation](#citation)
- [Preparing Inputs](#preparing-inputs)
- [Usage](#usage)
- [License](#license)

Prerequisites
-------------
`SimOutbreakSelection` or `SOS` relies on [`SLiM`](https://github.com/MesserLab/SLiM/releases/download/v3.3.1/SLiM.zip) (3.3.1) , python (3.7.12), and R (4.2.2). 

Crucially `SOS` uses the following modules within python to handle `SLiM` tree sequence outputs: 
* `tskit` (0.2.3) 
* `msprime` (0.7.4)
* `pyslim` (0.403)

An appropriate environment can be installed using conda or micromamba. [Micromamba](https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html) is highly recommended.

Installation
------------
```
# Create a conda environment
micromamba env create -f environment.yml
```
## Citation
Our preprint is available on [BioRxiv](https://www.biorxiv.org/content/10.1101/2024.06.27.601009v1.full).

## Usage
Documentation for `SOS` can be found here at the [SimOutbreakSelection wiki](https://github.com/santaci/SimOutbreakSelection/wiki/Documentation).

## License
Distributed under the GNU General Public License v3.0. See the [LICENSE](./LICENSE) file for more information.

