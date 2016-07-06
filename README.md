# GENPRO

GENPRO is a software package developed at NCAR designed for scientific data
processing. The data files output from this software are written in a
binary format, which came to be known as GENPRO format.

This repository contains all the old GENPRO-I and GENPRO-II code that could be
found around NCAR. In addition it contains software to convert GENPRO-I and
GENPRO-II to netCDF.


# NCAR/EOL GENPRO-I Conversion Utility & Documentation

## Getting the source

```
$ git clone https://github.com/ncareol/GENPRO.git
```

## Compiling

`genpro2nc` has no external dependencies. Compile with:

```
$ cd GENPRO/g1nc/c
$ make
```

## Documentation

Documentation of the GENPRO software can be found in the
[NCAR archives](https://opensky.ucar.edu/islandora/search/GENPRO?type=dismax).

A PDF document describing the GENPRO-I format is available as a part of this
project: [genpro.pdf](https://ncareol.github.io/GENPRO/files/genpro.pdf).
