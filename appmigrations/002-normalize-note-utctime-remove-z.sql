UPDATE note
SET
    created = CASE
        WHEN substr(created, -1) = 'Z'
        THEN substr(created, 1, length(created) - 1)
        ELSE created
    END,
    updated = CASE
        WHEN substr(updated, -1) = 'Z'
        THEN substr(updated, 1, length(updated) - 1)
        ELSE updated
    END
WHERE
    substr(created, -1) = 'Z'
    OR substr(updated, -1) = 'Z';