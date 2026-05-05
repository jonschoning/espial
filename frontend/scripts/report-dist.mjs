import fs from "node:fs";
import path from "node:path";

const distDir = path.join(process.cwd(), "dist");

const entries = fs
  .readdirSync(distDir, { withFileTypes: true })
  .filter((entry) => entry.isFile())
  .map((entry) => {
    const filePath = path.join(distDir, entry.name);
    const stat = fs.statSync(filePath);
    const sizeKb = Math.max(1, Math.ceil(stat.size / 1024));
    return `${sizeKb}K\t${path.posix.join("dist", entry.name)}`;
  })
  .sort((a, b) => a.localeCompare(b));

for (const line of entries) {
  console.log(line);
}
