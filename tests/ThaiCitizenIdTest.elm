module ThaiCitizenIdTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import ThaiCitizenId exposing (validate)


suite : Test
suite =
    describe "validate"
        [ test "valid citizen id" <|
            \_ ->
                validate "1234567890121"
                    |> Expect.equal True
        , test "invalid citizen id" <|
            \_ ->
                [ "12314123" -- not 13 length.
                , "12345a7890121" -- contains non-digit character.
                ]
                    |> List.map (\id -> validate id)
                    |> List.all (\valid -> valid == False)
                    |> Expect.equal True
        ]
