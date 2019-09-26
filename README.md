# Espial

Espial is an open-source, web-based bookmarking server.

It allows mutiple accounts, but currently intended for self-host scenarios.

The bookmarks are stored in a sqlite3 database, for ease of deployment & maintenence.

The easist way for logged-in users to add bookmarks, is with the "bookmarklet", found on the Settings page.

## demo server

log in — username: demo  password: demo

https://esp.ae8.org/u:demo

![jpg](https://i.imgur.com/XikHLua.png)

## Docker Setup

see https://github.com/jonschoning/espial-docker

## Server Setup (from source)

1. Install the Stack executable here:
    - https://tech.fpcomplete.com/haskell/get-started

2. Build executables
 
    ```
    stack build
    ```

3. Create the database

    ```
    stack exec migration -- createdb --conn espial.sqlite3
    ```

4. Create a user

    ```
    stack exec migration -- createuser --conn espial.sqlite3 --userName myusername --userPassword myuserpassword
    ```

5. Import a bookmark file for a user (optional)

    ```
     stack exec migration -- importbookmarks --conn espial.sqlite3 --userName myusername --bookmarkFile sample-bookmarks.json
    ```

6. Start a production server:

    ```
    stack exec espial -- +RTS -T
    ```

see `config/settings.yml` for changing default run-time parameters / environment variables

default app http port: `3000`

default ekg http port: `8000`

ssl: use reverse proxy

## Development 

### Backend

- Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`


- Start a development server:

    ```
    yesod devel
    ```

### Frontend

- See `purs/` folder

## Import Bookmark file format

see `sample-bookmarks.json`, which contains a JSON array, each line containing a `FileBookmark` object. 

example:

```
[ {"href":"http://raganwald.com/2018/02/23/forde.html","description":"Forde's Tenth Rule, or, \"How I Learned to Stop Worrying and \u2764\ufe0f the State Machine\"","extended":"","time":"2018-02-26T22:57:20Z","shared":"yes","toread":"yes","tags":"raganwald"},
, {"href":"http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html","description":"7.6. Flag reference \u2014 Glasgow Haskell Compiler 8.2.2 User's Guide","extended":"-fprint-expanded-synonyms","time":"2018-02-26T21:52:02Z","shared":"yes","toread":"no","tags":"ghc haskell"},
]
```
