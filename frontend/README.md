## Build

Minimal Makefile build flow:

1. Install dependencies:

```sh
make install
```

2. Typecheck, build, minify, gzip, and copy assets to `../static/js/`:

```sh
make
```

Output includes `dist/app.min.js` and `dist/app.min.js.map`.

