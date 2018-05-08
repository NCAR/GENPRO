© 2016 University Corporation for Atmospheric Research

This work was performed under the auspices of the National Center for
Atmospheric Research (NCAR) Earth Observing Laboratory (EOL) Summer
Undergraduate Projects in Engineering Research (SUPER) program, which is
managed by the University Corporation for Atmospheric Research (UCAR) and is
funded by the National Science Foundation (NSF) (www.eol.ucar.edu).

# GENPRO

GENPRO is a software package developed at NCAR designed for scientific data
processing. The data files output from this software are written in a
binary format, which came to be known as GENPRO format.

This repository contains all the old GENPRO-I and GENPRO-II code that could be
found around NCAR. In addition it contains software to convert GENPRO-I and
GENPRO-II to netCDF.


### Getting the source

```
$ git clone https://github.com/ncareol/GENPRO.git
```

## NCAR/EOL GENPRO-I Conversion Utility & Documentation

### Compiling

`genpro2nc` has no external dependencies. Compile with:

```
$ cd GENPRO/g1nc/c
$ make
```

### Documentation

Documentation of the GENPRO software can be found in the
[NCAR archives](https://opensky.ucar.edu/islandora/search/GENPRO?type=dismax).

In particular, please note that information on GBYTES and SBYTES can be found in [Gacnik, B., and B. Lackman, 1983: GenPro: GEN Scientific Data Processor; Reference Manual, Release 1.0. NCAR Technical Note NCAR/TN-209+IA, doi:10.5065/D6V122Q1.](https://opensky.ucar.edu/islandora/object/technotes%3A311), Appendix B (Pp 332) 

A PDF document describing the GENPRO-I format is available as a part of this
project: [genpro.pdf](https://ncareol.github.io/GENPRO/files/genpro.pdf).

### Additional Resources

The following repositories may be useful:

* https://github.com/NCAR/TBMconv utilities to examing and extract the contents of TBM archives
* https://github.com/NCAR/GENPRO-batch-convert [private] batch scripts to automate converting large quantities of old EOL/RAF GENPRO-I files.

## NCAR/EOL GENPRO-II Conversion Utility & Documentation

### Compiling

To compile `g2n` 

```
$ cd g2n
$ make
```

### Documentation
