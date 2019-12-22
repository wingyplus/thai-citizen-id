module ThaiCitizenId exposing (CitizenId, validate)

{-| It's library uses for validate thai citizen id. This library implements base on:

  - <https://www.memo8.com/%E0%B8%A7%E0%B8%B4%E0%B8%98%E0%B8%B5%E0%B8%81%E0%B8%B2%E0%B8%A3%E0%B8%84%E0%B8%B3%E0%B8%99%E0%B8%A7%E0%B8%93%E0%B9%80%E0%B8%9E%E0%B8%B7%E0%B9%88%E0%B8%AD%E0%B8%95%E0%B8%A3%E0%B8%A7%E0%B8%88%E0%B8%AA/>
  - <https://github.com/codeforthailand/Thai-Citizen-ID-Validator>

-}


{-| CitizenId it's a String type to represents citizen id.
-}
type alias CitizenId =
    String


{-| Validate citizen id card.
-}
validate : CitizenId -> Bool
validate id =
    thirteenLength id && validateId id


thirteenLength : CitizenId -> Bool
thirteenLength id =
    String.length id == 13


validateId : CitizenId -> Bool
validateId id =
    case Maybe.map2 (==) (sum id) (lastDigit id) of
        Just b ->
            b

        Nothing ->
            False


lastDigit : CitizenId -> Maybe Int
lastDigit id =
    String.right 1 id |> String.toInt


sum : CitizenId -> Maybe Int
sum id =
    String.left 12 id
        |> String.toList
        |> List.indexedMap (\idx ch -> Maybe.map2 (*) (String.fromChar ch |> String.toInt) (Just (13 - idx)))
        |> List.foldl (\a b -> Maybe.map2 (+) a b) (Just 0)
        |> Maybe.map (Basics.remainderBy 11)
        |> Maybe.map2 (-) (Just 11)
        |> Maybe.map (Basics.remainderBy 10)
