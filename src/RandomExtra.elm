module RandomExtra exposing (..)

import Random



-- 0-9 48-57 // 10
-- A-Z 65-90 // 26
-- a-z 97-122 // 26


alphaNum : Random.Generator Char
alphaNum =
    Random.map
        (\n ->
            if n <= 10 then
                Char.fromCode (n + 48)

            else if n <= 10 + 26 then
                Char.fromCode (n + 65)

            else
                Char.fromCode (n + 97)
        )
        (Random.int 0 (10 + 26 + 26))
