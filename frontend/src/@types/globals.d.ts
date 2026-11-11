import type { App } from '../globals';

declare global {
  // Provided by the server in a <script> tag.
  var app: App;
}
