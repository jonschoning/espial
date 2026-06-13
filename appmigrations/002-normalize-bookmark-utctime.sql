UPDATE bookmark
SET time = x.new_time
FROM (
  SELECT
    rowid,
    CASE
      WHEN instr(t, '.') = 0 THEN t || 'Z'
      ELSE
        substr(t, 1, instr(t, '.') - 1)
        ||
        CASE
          WHEN rtrim(substr(t, instr(t, '.') + 1), '0') = '' THEN ''
          ELSE '.' || rtrim(substr(t, instr(t, '.') + 1), '0')
        END
        || 'Z'
    END AS new_time
  FROM (
    SELECT
      rowid,
      CASE
        WHEN time LIKE '%Z' THEN substr(time, 1, length(time) - 1)
        ELSE time
      END AS t
    FROM bookmark
  )
) x
WHERE bookmark.rowid = x.rowid;