# Espial

Espial is an open-source, web-based bookmarking server.

It allows mutiple accounts, but currently intended for self-host scenarios.

The bookmarks are stored in a sqlite3 database, for ease of deployment & maintenence.

The easist way for logged-in users to add bookmarks, is with the "bookmarklet", found on the Settings page.


## Demo Server

Log in with:

- username: `demo`
- password: `demo`

https://esp.ae8.org/u:demo

![jpg](./docs/demo-bookmarks-page.png)


## Installation

### Docker Setup

See:

https://github.com/jonschoning/espial-docker

### Server Setup (From Source)

1. Install the Stack executable here:
   - https://tech.fpcomplete.com/haskell/get-started
2. Build executables:

```bash
stack build
```

3. Create the database:

```bash
stack exec migration -- createdb
```

4. Create a user:

```bash
stack exec migration -- createuser --userName myusername --userPassword myuserpassword
```

5. Import a pinboard bookmark file for a user (optional):

```bash
stack exec migration -- importbookmarks --userName myusername --bookmarkFile sample-bookmarks.json
```

6. Import a firefox bookmark file for a user (optional):

```bash
stack exec migration -- importfirefoxbookmarks --userName myusername --bookmarkFile firefox-bookmarks.json
```

7. Start a production server:

```bash
stack exec espial
```

## Configuration

See `config/settings.yml` for changing default run-time parameters & environment variables.

- `config/settings.yml` is embedded into the app executable when compiled and also read once when the app starts.  Current settings in `config/settings.yml` will override the embedded compile-time settings.
- `config/settings.yml` values formatted like `_env:ENV_VAR_NAME:default_value` can be overridden by the specified environment variable.
- Example:
  - `_env:PORT:3000`
  - environment variable `PORT`
  - default app http port: `3000`

SSL: use reverse proxy

## Archive Backends

Espial supports configurable archive backends for saving bookmark snapshots.

Set the backend with `archive-backend` in `config/settings.yml`:

- `disabled`: archiving is turned off (default).
- `wayback-machine`: enables submission to the Internet Archive Wayback Machine.

    Wayback Machine support requires the following settings:

    - `wayback-machine-access-key`
    - `wayback-machine-secret-key`

        Create these by signing in to your Internet Archive account and generating S3-style API credentials at `https://archive.org/account/s3.php`. \
        If `wayback-machine` is selected but the access key or secret key is missing, archiving is disabled at runtime.

Optional proxy settings for archive requests:

- `archive-socks-proxy-host`
- `archive-socks-proxy-port`

## Related Projects

Also, see the android app for adding bookmarks via an Android Share intent:

https://github.com/jonschoning/espial-share-android

## Development

### Backend

- Install the `yesod` command line tool:

```bash
stack install yesod-bin --install-ghc
```

- Start a development server:

```bash
yesod devel
```

### Frontend

- See `frontend/` folder

## CLI

Migration commands are run via:

```bash
stack exec migration -- <command> [options]
```

All commands take an optional `--conn` parameter for the database location; if omitted, the database location is loaded from `config/settings.yml`.

### Commands

| Command | Example |
| --- | --- |
| `createdb` | `stack exec migration -- createdb` |
| `createuser` | `stack exec migration -- createuser --userName myusername --userPassword myuserpassword` |
| `createuser` (password file) | `stack exec migration -- createuser --userName myusername --userPasswordFile mypassword.txt` |
| `deleteuser` | `stack exec migration -- deleteuser --userName myusername` |
| `createapikey` | `stack exec migration -- createapikey --userName myusername` |
| `deleteapikey` | `stack exec migration -- deleteapikey --userName myusername` |
| `importbookmarks` | `stack exec migration -- importbookmarks --userName myusername --bookmarkFile sample-bookmarks.json` |
| `importfirefoxbookmarks` | `stack exec migration -- importfirefoxbookmarks --userName myusername --bookmarkFile firefox-bookmarks.json` |
| `importnetscapebookmarks` | `stack exec migration -- importnetscapebookmarks --userName myusername --bookmarkFile bookmarks.html` |
| `importnotes` | `stack exec migration -- importnotes --userName myusername --noteDirectory ./notes` |
| `exportbookmarks` | `stack exec migration -- exportbookmarks --userName myusername --bookmarkFile exported-bookmarks.json` |
| `exportnetscapebookmarks` | `stack exec migration -- exportnetscapebookmarks --userName myusername --bookmarkFile exported-bookmarks.html` |
| `printmigratedb` | `stack exec migration -- printmigratedb` |
| `showuser` | `stack exec migration -- showuser --userName myusername` |

### `importbookmarks` Command Notes:

See `sample-bookmarks.json`, which contains a JSON array, each line containing a `FileBookmark` object.

Example:

```json
[ {"href":"http://raganwald.com/2018/02/23/forde.html","description":"Forde's Tenth Rule, or, \"How I Learned to Stop Worrying and \u2764\ufe0f the State Machine\"","extended":"","time":"2018-02-26T22:57:20Z","shared":"yes","toread":"yes","tags":"raganwald"},
, {"href":"http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html","description":"7.6. Flag reference \u2014 Glasgow Haskell Compiler 8.2.2 User's Guide","extended":"-fprint-expanded-synonyms","time":"2018-02-26T21:52:02Z","shared":"yes","toread":"no","tags":"ghc haskell"},
]
```
