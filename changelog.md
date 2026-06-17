# Changelog

## v0.0.35 (2026-06-17)

- add "appMigrations" on startup; normalize utctime fields in db for correct sorting
- record "appMigration" history and app version history in db
- add startup logs with version and revision information
- add 'view in context' link, visible when viewing single bookmark listing
- on bookmark and note listing, change '« earlier' and 'later »' links from pagenum-based params to time-based 'before'/'after' params
- update robots.txt to disallow robots
- add short versions of search term specifiers & updated search documentation:
  - u: for url:, t: for tags:, ti: for title:, d: for description:, b: for before:, a: for after:
- add OCI Image Specification annotation keys to docker image (including version & revision)

## v0.0.34 (2026-06-11)

- fix serialization bug in Add bookmark handler (introduced in v0.0.33)

## v0.0.33 (2026-06-08)

- add new `archive-backend`: `archivebox07`
  - additional documentation in readme
- add new archive-related elements on UI (visible when `archive-backend` is enabled)
  - add url popup: `archive` checkbox (only displays for new bookmarks)
    - when `Account Settings`: `Archive new non-Private bookmarks by default` is checked, the Archive checkbox is checked by default
    - upon submission, the archive backend will run if checked
  - bookmark edit form: `archive` button
    - when clicked, immediately sends a request to the archive backend
  - when a bookmark is marked as private, the archive checkbox/archive button are disabled
  - `Account Settings`: `Privacy Lock (Private Account)` only controls authorization to view user's content on espial; it has no effect on whether bookmarks are archived.
- update shared libs in runtime docker image
- add docker compose examples

## v0.0.32 (2026-05-28)

- change runtime docker image from `jonschoning/espial:scratch` to `gcr.io/distroless/base-debian12`
- fix css for 404 page

## v0.0.31 (2026-05-27)

- use values in `config/settings.yml` at run-time (on app startup), instead of only the values from compile-time embedding of `config/settings.yml`
- add Archive Backend: `wayback-machine`
  - add Wayback Machine Backend-related settings:
    - `wayback-machine-access-key`
    - `wayback-machine-secret-key`
- remove Archive Backend: `archive-li`
- only show the archive link icon on the user's bookmark page if the the `archiveUrl` exists
- adjust dark-mode link visited color and toggle
- hide status of starred, to-read, and archive properties of bookmarks when not viewed by the bookmark owner

## v0.0.30 (2026-05-24)

- add Dark Mode (fixes #59)

## v0.0.29 (2026-05-07)

- Extend migration to add commands: `importnetscapebookmarks`, `exportnetscapebookmarks` (fixes #30)
- fix migration command `ImportBookmarks` (pinboard import file) parsing bug to handle when the description is a boolean (fixes #32)
- tag suggestions: when a tag exactly matches an existing tag, it will appear first in the suggestion list

## v0.0.28 (2026-05-06)

- add new app setting: `archive-backend: "_env:ARCHIVE_BACKEND:disabled"`
  - default to `disabled`, since `archive-li` appears to no longer be working due to added captchas
  - when archiving is disabled, the `Archive Non-Private Bookmarks` Account Setting is also disabled in the UI

## v0.0.27 (2026-05-05)

- prevent private tags (starts with ".") from showing on public bookmark listing for a user if not owned

## v0.0.26 (2026-05-04)

- Add Tag Suggestion when adding/editing bookmarks.
- Add account settings controls for `suggestTags` (default `True`).
- Extend migration `createuser` with `suggestTags` setting.
- Extend migration to add `showuser` command
- Extend migration to load sql connection from yaml settings if not provided
- use hashed js output + manifest.json in frontend build

## v0.0.25 (2026-05-03)

- Fix display bug for non-markdown notes.

## v0.0.24 (2026-04-24)

- Correct `extra-source-files` in `package.yaml`.
- Correct GHC warnings.

## v0.0.23 (2026-04-24)

- Migrate frontend code from PureScript to TypeScript/React.
- Update Stackage resolver: `lts-24.37`.

## v0.0.22 (2025-05-23)

- Update to Stackage LTS 23 (`ghc-9.8.4`).
- Purs package-set update.

## v0.0.21 (2024-01-15)

- Switch to ESM JavaScript module imports.

## v0.0.20 (2024-01-18)

- Update version bounds for Cabal dependencies.

## v0.0.19 (2023-12-19)

- Update GHC and PureScript dependencies to latest.

## v0.0.18 (2023-10-02)

- Update GHC and PureScript dependencies to latest.

## v0.0.17 (2023-07-04)

- Update to GHC 9.4.5.

## v0.0.16 (2023-01-05)

- Add SIGTERM handler on Linux.
- Adjust Docker CMD entrypoint.

## v0.0.15 (2022-09-11)

- Avoid using external `static/tmp` folder for generated static files.

## v0.0.14 (2022-06-01)

- Upgrade to PureScript v0.15.
- Increase bookmarklet window height.

## v0.0.13 (2022-04-26)

- Add setting `ALLOW_NON_HTTP_URL_SCHEMES` (default `false`).

## v0.0.12 (2022-04-17)

- Update to GHC 9.

## v0.0.11 (2022-04-16)

- Add API key auth.
- Add `CreateApiKey`/`DeleteApiKey` commands to executable `migration`.

## v0.0.10 (2022-04-05)

- Update PureScript and package versions.

## v0.0.9 (2022-04-05)

- Rolling releases.

## v0.0.7 (2019-01-30)

- Init.
