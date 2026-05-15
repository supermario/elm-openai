module OpenAI.Realtime exposing (..)

import Ext.Http
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Time


type alias SessionInput =
    { model : String
    , output_modalities : Maybe (List String)
    , instructions : Maybe String
    , audio : Maybe AudioConfig
    , tools : Maybe (List Tool)
    , tool_choice : Maybe String
    , max_response_output_tokens : Maybe IntOrInf
    }


type alias AudioConfig =
    { input : Maybe AudioInputConfig
    , output : Maybe AudioOutputConfig
    }


type alias AudioInputConfig =
    { format : Maybe AudioFormat
    , turn_detection : Maybe TurnDetection
    , transcription : Maybe InputAudioTranscription
    }


type alias AudioOutputConfig =
    { format : Maybe AudioFormat
    , voice : Maybe String
    }


type alias AudioFormat =
    { type_ : String
    , rate : Maybe Int
    }


type alias InputAudioTranscription =
    { model : String }


type alias TurnDetection =
    { type_ : String
    , threshold : Maybe Float
    , prefix_padding_ms : Maybe Int
    , silence_duration_ms : Maybe Int
    , create_response : Maybe Bool
    }


type alias Tool =
    { type_ : Maybe String
    , name : Maybe String
    , description : Maybe String
    , parameters : Maybe String
    }


type IntOrInf
    = IntValue Int
    | Inf


encodeIntOrInf : IntOrInf -> Encode.Value
encodeIntOrInf intOrInf =
    case intOrInf of
        IntValue int ->
            Encode.int int

        Inf ->
            Encode.string "inf"


encodeSessionInput : SessionInput -> Encode.Value
encodeSessionInput input =
    Encode.object
        [ ( "session"
          , Encode.object
                (List.filterMap identity
                    [ Just ( "type", Encode.string "realtime" )
                    , Just ( "model", Encode.string input.model )
                    , Maybe.map (\a -> ( "output_modalities", Encode.list Encode.string a )) input.output_modalities
                    , Maybe.map (\a -> ( "instructions", Encode.string a )) input.instructions
                    , Maybe.map (\a -> ( "audio", encodeAudioConfig a )) input.audio
                    , Maybe.map (\a -> ( "tools", Encode.list encodeTool a )) input.tools
                    , Maybe.map (\a -> ( "tool_choice", Encode.string a )) input.tool_choice
                    , Maybe.map (\a -> ( "max_response_output_tokens", encodeIntOrInf a )) input.max_response_output_tokens
                    ]
                )
          )
        ]


encodeAudioConfig : AudioConfig -> Encode.Value
encodeAudioConfig audio =
    Encode.object
        (List.filterMap identity
            [ Maybe.map (\a -> ( "input", encodeAudioInputConfig a )) audio.input
            , Maybe.map (\a -> ( "output", encodeAudioOutputConfig a )) audio.output
            ]
        )


encodeAudioInputConfig : AudioInputConfig -> Encode.Value
encodeAudioInputConfig input =
    Encode.object
        (List.filterMap identity
            [ Maybe.map (\a -> ( "format", encodeAudioFormat a )) input.format
            , Maybe.map (\a -> ( "turn_detection", encodeTurnDetection a )) input.turn_detection
            , Maybe.map (\a -> ( "transcription", encodeInputAudioTranscription a )) input.transcription
            ]
        )


encodeAudioOutputConfig : AudioOutputConfig -> Encode.Value
encodeAudioOutputConfig output =
    Encode.object
        (List.filterMap identity
            [ Maybe.map (\a -> ( "format", encodeAudioFormat a )) output.format
            , Maybe.map (\a -> ( "voice", Encode.string a )) output.voice
            ]
        )


encodeAudioFormat : AudioFormat -> Encode.Value
encodeAudioFormat format =
    Encode.object
        ([ ( "type", Encode.string format.type_ ) ]
            ++ List.filterMap identity
                [ Maybe.map (\a -> ( "rate", Encode.int a )) format.rate
                ]
        )


encodeInputAudioTranscription : InputAudioTranscription -> Encode.Value
encodeInputAudioTranscription transcription =
    Encode.object
        [ ( "model", Encode.string transcription.model )
        ]


encodeTurnDetection : TurnDetection -> Encode.Value
encodeTurnDetection turnDetection =
    Encode.object
        ([ ( "type", Encode.string turnDetection.type_ ) ]
            ++ List.filterMap identity
                [ Maybe.map (\a -> ( "threshold", Encode.float a )) turnDetection.threshold
                , Maybe.map (\a -> ( "prefix_padding_ms", Encode.int a )) turnDetection.prefix_padding_ms
                , Maybe.map (\a -> ( "silence_duration_ms", Encode.int a )) turnDetection.silence_duration_ms
                , Maybe.map (\a -> ( "create_response", Encode.bool a )) turnDetection.create_response
                ]
        )


encodeTool : Tool -> Encode.Value
encodeTool tool =
    Encode.object
        (List.filterMap identity
            [ Maybe.map (\a -> ( "type", Encode.string a )) tool.type_
            , Maybe.map (\a -> ( "name", Encode.string a )) tool.name
            , Maybe.map (\a -> ( "description", Encode.string a )) tool.description
            , Maybe.map (\a -> ( "parameters", Encode.string a )) tool.parameters
            ]
        )


type alias SessionOutput =
    { id : String
    , client_secret : ClientSecret
    }


type alias ClientSecret =
    { value : String
    , expires_at : Time.Posix
    }


decodeSessionOutput : Decode.Decoder SessionOutput
decodeSessionOutput =
    Decode.map2 SessionOutput
        (Decode.field "id" Decode.string)
        (Decode.field "client_secret" decodeClientSecret)


decodeClientSecret : Decode.Decoder ClientSecret
decodeClientSecret =
    Decode.map2 ClientSecret
        (Decode.field "value" Decode.string)
        (Decode.field "expires_at" Decode.int |> Decode.map Time.millisToPosix)


createSession : SessionInput -> Ext.Http.TaskInput (Ext.Http.Error String) SessionOutput
createSession sessionInput =
    { method = "POST"
    , headers = []
    , url = "/realtime/client_secrets"
    , body = Http.jsonBody <| encodeSessionInput sessionInput
    , resolver =
        Http.stringResolver
            (Ext.Http.jsonResolver decodeSessionOutput >> Result.map .data)
    , timeout = Nothing
    }
