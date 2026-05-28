stack exec migration -- createdb
stack exec migration -- createuser --userName myusername --userPassword myuserpassword
stack exec migration -- importbookmarks --userName myusername --bookmarkFile sample-bookmarks.json
