import { transformAsync } from "@babel/core";
import * as esbuild from "esbuild";
import fs from "node:fs/promises";

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

function babel({ filter, config }) {
  return {
    name: "babel",
    setup(build) {
      build.onLoad({ filter }, async (args) => {
        const source = await fs.readFile(args.path, "utf8");
        const result = await transformAsync(source, {
          ...config,
          filename: args.path,
          caller: { name: "esbuild", supportsStaticESM: true },
        });
        return { contents: result.code, loader: "js" };
      });
    },
  };
}
