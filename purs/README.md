## Development (Posix only)

1. Install `purescript`, `spago`, `parcel-bundler`: 

```
npm install
```

2. (optional) working with .dhall files: 

```
stack install dhall dhall-json
```

3. Download purescript libraries (1x only): 

```
make install
```

4. build dist/app.min.js: 

```
make
```

On a successful build, `make` will also update `../static/js/`, 
since the `purs/` folder is opaque to the espial executable build process. 

