import * as esbuild from "esbuild";
import babel from "esbuild-plugin-babel";

await esbuild.build({
  entryPoints: ["./src/index.tsx"],
  bundle: true,
  format: "esm",
  target: ["chrome61", "firefox60", "edge18"],
  minify: true,
  sourcemap: true,
  entryNames: "app-[hash].min",
  outdir: "dist",
  plugins: [
    babel({
      filter: /\.tsx?$/,
      config: {
        presets: [
          "@babel/preset-typescript",
          ["@babel/preset-react", { runtime: "automatic" }],
        ],
        plugins: ["babel-plugin-react-compiler"],
      },
    }),
  ],
});
