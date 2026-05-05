import fs from "node:fs";
import path from "node:path";
import zlib from "node:zlib";

const frontendRoot = process.cwd();
const distDir = path.join(frontendRoot, "dist");
const staticJsDir = path.join(frontendRoot, "..", "static", "js");

const distFiles = fs.readdirSync(distDir, { withFileTypes: true });
const bundleCandidates = distFiles
  .filter((entry) => entry.isFile())
  .map((entry) => entry.name)
  .filter((name) => /^app-.*\.min\.js$/.test(name) && !name.endsWith(".map"))
  .sort();

if (bundleCandidates.length === 0) {
  console.error("hashed bundle not found");
  process.exit(1);
}

const bundleFile = bundleCandidates[0];
const sourceJs = path.join(distDir, bundleFile);
const sourceMap = path.join(distDir, `${bundleFile}.map`);

if (!fs.existsSync(sourceMap)) {
  console.error(`sourcemap not found for ${bundleFile}`);
  process.exit(1);
}

const jsGz = `${sourceJs}.gz`;
const mapGz = `${sourceMap}.gz`;

fs.writeFileSync(jsGz, zlib.gzipSync(fs.readFileSync(sourceJs)));
fs.writeFileSync(mapGz, zlib.gzipSync(fs.readFileSync(sourceMap)));

const oldStaticFiles = fs
  .readdirSync(staticJsDir, { withFileTypes: true })
  .filter((entry) => entry.isFile())
  .map((entry) => entry.name)
  .filter((name) => /^app-.*\.min\.js(\.map)?(\.gz)?$/.test(name));

for (const oldFile of oldStaticFiles) {
  fs.rmSync(path.join(staticJsDir, oldFile), { force: true });
}

for (const fileName of [bundleFile, `${bundleFile}.map`, `${bundleFile}.gz`, `${bundleFile}.map.gz`]) {
  fs.copyFileSync(path.join(distDir, fileName), path.join(staticJsDir, fileName));
}

const manifestPath = path.join(staticJsDir, "manifest.json");
const manifestContent = `${JSON.stringify({ appBundle: bundleFile })}\n`;
fs.writeFileSync(manifestPath, manifestContent);

console.log(`Published ${bundleFile}`);
