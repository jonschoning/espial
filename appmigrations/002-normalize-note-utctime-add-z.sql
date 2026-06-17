UPDATE note
SET created = x.new_created,
    updated = x.new_updated
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
    END AS new_created,
    CASE
      WHEN instr(t2, '.') = 0 THEN t2 || 'Z'
      ELSE
        substr(t2, 1, instr(t2, '.') - 1)
        ||
        CASE
          WHEN rtrim(substr(t2, instr(t2, '.') + 1), '0') = '' THEN ''
          ELSE '.' || rtrim(substr(t2, instr(t2, '.') + 1), '0')
        END
        || 'Z'
    END AS new_updated
  FROM (
    SELECT
      rowid,
      CASE
        WHEN created LIKE '%Z' THEN substr(created, 1, length(created) - 1)
        ELSE created      
      END AS t,
      CASE
        WHEN updated LIKE '%Z' THEN substr(updated, 1, length(updated) - 1)
        ELSE updated      
      END AS t2

    FROM note
  )
) x
WHERE note.rowid = x.rowid;