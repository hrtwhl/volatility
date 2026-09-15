import { existsSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";

// Framework runs .py data loaders with a bare python3 from $PATH, which is
// whatever the shell inherited, not necessarily the venv holding pandas and
// pyarrow. Pin it: use the project venv when present, else python3, which is
// correct in CI where the package goes into the runner's system Python.
const siteDir = dirname(fileURLToPath(import.meta.url));
const venvPython = join(siteDir, "..", ".venv", "bin", "python");
const python = process.env.PYTHON ?? (existsSync(venvPython) ? venvPython : "python3");

const FONTS =
  '<link rel="preconnect" href="https://fonts.googleapis.com">' +
  '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>' +
  '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@400;500;600&display=swap">';

// Top navigation. With the sidebar disabled Framework no longer renders the
// `pages` tree, so links live here. Adding a section is one <a>.
//
// The inline script marks the active link by path prefix, so /rates/anything
// highlights "Rates" without needing to enumerate every page.
const NAV =
  '<div class="topnav">' +
  '<a class="topnav-brand" href="/">Macropinion</a>' +
  '<nav class="topnav-links">' +
  '<a href="/inflation/us" data-section="/inflation">Inflation</a>' +
  '</nav>' +
  '</div>' +
  '<script>' +
  "for (const a of document.querySelectorAll('.topnav-links a')) {" +
  "  if (location.pathname.startsWith(a.dataset.section)) a.setAttribute('aria-current','page');" +
  '}' +
  '<\/script>';

export default {
  title: "Macropinion",

  interpreters: { ".py": [python] },

  root: "src",
  theme: "dark",
  head: FONTS,
  header: NAV,
  sidebar: false,
  style: "theme.css",

  footer: "",
  toc: false,
  pager: false,
  search: false,
};
