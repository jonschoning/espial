stack exec migration -- createdb --conn espial.sqlite3
stack exec migration -- createuser --conn espial.sqlite3 --userName myusername --userPassword myuserpassword
stack exec migration -- importbookmarks --conn espial.sqlite3 --userName myusername --bookmarkFile sample-bookmarks.json
