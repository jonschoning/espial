UPDATE bookmark
SET time = substr(time, 1, length(time) - 1)
WHERE substr(time, -1) = 'Z';