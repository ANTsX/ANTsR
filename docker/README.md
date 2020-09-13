# ANTsR docker script

## Versioning

The Dockerfile copies the ANTsR source directory in which it resides to the
container. ITKR and ANTsRCore are cloned from Github, then all three packages
are installed. The commit hash of the installed versions is stored in the
container under `/opt`.

To build, run

```
docker build -t antsr .
```

## Running R or Rscript

To run R interactively:

```
docker run --rm -it antsr
```

To run Rscript:

```
docker run --rm  antsr Rscript -e 'print("Hello")'
```

## Controlling threads

The default number of threads is 1, override by passing the
`ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS` variable, eg 

```
docker run -e ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=2 ...
```

## R package dependencies

A full set of dependencies are included in `docker/installDependencies.R`. This
list is sourced from the
[https://github.com/ANTsX/ANTsR/wiki/ANTsR-Dependencies-for-(close-to)-full-functionality](ANTsR
wiki).
