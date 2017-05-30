port module Main exposing (..)

import Html exposing (div, header, h1, text, main_, p, form, input, button, section, ul, li, footer, a)
import Html.Attributes as Attr exposing (class, type_, maxlength, placeholder, id, href)
import Html.Events as Events exposing (onSubmit, onInput)
import Http
import Json.Decode as Decode


port logInfo : String -> Cmd msg

port logError : String -> Cmd msg


type alias Model =
    { input : String
    , dreams : List String
    }


type Msg
    = ReceiveGetDreamsResponse (Result Http.Error (List String))
    | InputDream String
    | AddDream
    | ReceiveAddDreamResponse String (Result Http.Error String)


main =
    Html.program
        { init = (emptyModel, Cmd.batch [ logInfo "hello world :o", getDreams ])
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


emptyModel =
    { input = ""
    , dreams = []
    }


getDreams =
    Http.get "/dreams" (Decode.list Decode.string)
        |> Http.send ReceiveGetDreamsResponse


addDream dream =
    Http.request
        { method = "POST"
        , headers = []
        , url = "/dreams?dream=" ++ Http.encodeUri dream
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (ReceiveAddDreamResponse dream)


update msg model =
    case msg of
        ReceiveGetDreamsResponse (Ok dreams) ->
            ({ model | dreams = dreams }, Cmd.none)
        InputDream input ->
            ({ model | input = input }, Cmd.none)
        AddDream ->
            ({ model | input = "" }, addDream model.input)
        ReceiveAddDreamResponse dream (Ok _) ->
            ({ model | dreams = model.dreams ++ [ dream ] }, Cmd.none)
        msg ->
            (model, logError ("Unexpected msg: " ++ toString msg))


view model =
    div []
      [ headerView
      , mainView model
      , footerView
      ]


headerView =
    header []
        [ h1 [] [ text "A Dream of the Future" ]]


mainView model =
    main_ []
        [ p [ class "bold" ] [ text "Oh hi," ]
        , p [] [ text "Tell me your hopes and dreams:" ]
        , form [ onSubmit AddDream ]
            [ input [ onInput InputDream, type_ "text", maxlength 100, placeholder "Dreams!" ]
                [ text model.input ]
            , button [ type_ "submit" ] [ text "Submit" ]
            ]
        , section [ class "dreams" ]
            [ ul [ id "dreams" ]
                (model.dreams |> List.map (\dream -> li [] [ text dream ]))
            ]
        ]


footerView =
    footer []
        [ a [ href "https://glitch.com" ]
            [ text "Remix this in Glitch" ]
        ]
