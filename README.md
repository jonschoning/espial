# Espial

Espial is an open-source, web-based bookmarking server.

It allows mutiple accounts, but currently intended for self-host scenarios.

The bookmarks are stored in a sqlite3 database, for ease of deployment & maintenence.

The easist way for logged-in users to add bookmarks, is with the "bookmarklet", found on the Settings page.

Also, see the android app for adding bookmarks via an Android Share intent https://github.com/jonschoning/espial-share-android

## demo server

log in — username: demo  password: demo

https://esp.ae8.org/u:demo

![jpg](https://i.imgur.com/jdnV93c.png)

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

5. Import a pinboard bookmark file for a user (optional)

    ```
    stack exec migration -- importbookmarks --conn espial.sqlite3 --userName myusername --bookmarkFile sample-bookmarks.json
    ```

6. Import a firefox bookmark file for a user (optional)

    ```
    stack exec migration -- importfirefoxbookmarks --conn espial.sqlite3 --userName myusername --bookmarkFile firefox-bookmarks.json
    ```

7. Start a production server:

    ```
    stack exec espial
    ```

### Configuration

See `config/settings.yml` for changing default run-time parameters & environment variables.
 - `config/settings.yml` is embedded into the app executable when compiled, so after changing `config/settings.yml`, run `stack build` again to apply the new settings.
 - `config/settings.yml` values formatted like `_env:ENV_VAR_NAME:default_value` can be
   overridden by the specified environment variable.
 - Example
    - `_env:PORT:3000`
        - environment variable `PORT`
        - default app http port: `3000`

SSL: use reverse proxy

## Development 

### Backend

- Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`


- Start a development server:

    ```
    yesod devel
    ```

### Frontend

- See `purs/` folder

## Import Bookmark file format (pinboard compatible format)

see `sample-bookmarks.json`, which contains a JSON array, each line containing a `FileBookmark` object. 

example:

```
[ {"href":"http://raganwald.com/2018/02/23/forde.html","description":"Forde's Tenth Rule, or, \"How I Learned to Stop Worrying and \u2764\ufe0f the State Machine\"","extended":"","time":"2018-02-26T22:57:20Z","shared":"yes","toread":"yes","tags":"raganwald"},
, {"href":"http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html","description":"7.6. Flag reference \u2014 Glasgow Haskell Compiler 8.2.2 User's Guide","extended":"-fprint-expanded-synonyms","time":"2018-02-26T21:52:02Z","shared":"yes","toread":"no","tags":"ghc haskell"},
]
```
