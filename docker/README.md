# ANTsR docker

This Dockerfile builds ANTsR using "method1" as described in the main README,
with devtools and install_github.

## Building and version information

The default is to build the main branch HEAD. You can build a specific version
using build args (from ANTsR/docker), for example

```
docker build --build-arg antsr_version="v0.5.6.5" -t antsr .
```

The version can be a tag, commit, or branch. See the documentation for
install_github in devools.

This allows you to control the ANTsR version, but ITKR and ANTsRCore will be
built from the latest commit.

From within the container, find the version of all packages and dependencies
with 

```
sessioninfo::package_info("ANTsR")
```

By default, all dependencies are included. To build a minimal set of
dependencies, run 

```
docker build --build-arg antsr_dependencies=NA -t antsr .
```

## Running R or Rscript

To run R interactively:

```
docker run --rm -it antsr
```

To run Rscript:

```
docker run --rm antsr Rscript -e 'print("Hello")'
```

## Controlling threads

The default number of threads is 1, override by passing the
`ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS` variable, eg 

```
docker run -e ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=2 ...
```

