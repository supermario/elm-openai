module OpenAI.Realtime exposing
    ( ClientSecret
    , SessionInput
    , SessionOutput
    , createSession
    )

import Ext.Http
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Time


type alias SessionInput =
    { model : String
    , modalities : Maybe (List String)
    , instructions : Maybe String
    , voice : Maybe String
    , input_audio_format : Maybe String
    , output_audio_format : Maybe String
    , input_audio_transcription : Maybe InputAudioTranscription
    , turn_detection : Maybe TurnDetection
    , tools : Maybe (List Tool)
    , tool_choice : Maybe String
    , temperature : Maybe Float
    , max_response_output_tokens : Maybe IntOrInf
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
    , parameters : Maybe Encode.Value
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
        (List.filterMap identity
            [ Just ( "model", Encode.string input.model )
            , Maybe.map (\a -> ( "modalities", Encode.list Encode.string a )) input.modalities
            , Maybe.map (\a -> ( "instructions", Encode.string a )) input.instructions
            , Maybe.map (\a -> ( "voice", Encode.string a )) input.voice
            , Maybe.map (\a -> ( "input_audio_format", Encode.string a )) input.input_audio_format
            , Maybe.map (\a -> ( "output_audio_format", Encode.string a )) input.output_audio_format
            , Maybe.map (\a -> ( "input_audio_transcription", encodeInputAudioTranscription a )) input.input_audio_transcription
            , Maybe.map (\a -> ( "turn_detection", encodeTurnDetection a )) input.turn_detection
            , Maybe.map (\a -> ( "tools", Encode.list encodeTool a )) input.tools
            , Maybe.map (\a -> ( "tool_choice", Encode.string a )) input.tool_choice
            , Maybe.map (\a -> ( "temperature", Encode.float a )) input.temperature
            , Maybe.map (\a -> ( "max_response_output_tokens", encodeIntOrInf a )) input.max_response_output_tokens
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
            , Maybe.map (\a -> ( "parameters", a )) tool.parameters
            ]
        )


type alias SessionOutput =
    { id : String
    , object : String
    , model : String
    , modalities : List String
    , instructions : String
    , voice : String
    , input_audio_format : String
    , output_audio_format : String
    , input_audio_transcription : Maybe InputAudioTranscription
    , turn_detection : Maybe TurnDetection
    , tools : List Tool
    , tool_choice : String
    , temperature : Float
    , max_response_output_tokens : IntOrInf
    , client_secret : ClientSecret
    }


type alias ClientSecret =
    { value : String
    , expires_at : Time.Posix
    }


decodeSessionOutput : Decode.Decoder SessionOutput
decodeSessionOutput =
    Decode.map8 SessionOutput
        (Decode.field "id" Decode.string)
        (Decode.field "object" Decode.string)
        (Decode.field "model" Decode.string)
        (Decode.field "modalities" (Decode.list Decode.string))
        (Decode.field "instructions" Decode.string)
        (Decode.field "voice" Decode.string)
        (Decode.field "input_audio_format" Decode.string)
        (Decode.field "output_audio_format" Decode.string)
        |> Decode.andThen
            (\sessionOutputPartial ->
                Decode.map7 sessionOutputPartial
                    (Decode.field "input_audio_transcription" (Decode.maybe decodeInputAudioTranscription))
                    (Decode.field "turn_detection" (Decode.maybe decodeTurnDetection))
                    (Decode.field "tools" (Decode.list decodeTool))
                    (Decode.field "tool_choice" Decode.string)
                    (Decode.field "temperature" Decode.float)
                    (Decode.field "max_response_output_tokens" decodeIntOrInf)
                    (Decode.field "client_secret" decodeClientSecret)
            )


decodeInputAudioTranscription : Decode.Decoder InputAudioTranscription
decodeInputAudioTranscription =
    Decode.map InputAudioTranscription
        (Decode.field "model" Decode.string)


decodeTurnDetection : Decode.Decoder TurnDetection
decodeTurnDetection =
    Decode.map5 TurnDetection
        (Decode.field "type" Decode.string)
        (Decode.field "threshold" (Decode.maybe Decode.float))
        (Decode.field "prefix_padding_ms" (Decode.maybe Decode.int))
        (Decode.field "silence_duration_ms" (Decode.maybe Decode.int))
        (Decode.field "create_response" (Decode.maybe Decode.bool))


decodeTool : Decode.Decoder Tool
decodeTool =
    Decode.map4 Tool
        (Decode.field "type" (Decode.maybe Decode.string))
        (Decode.field "name" (Decode.maybe Decode.string))
        (Decode.field "description" (Decode.maybe Decode.string))
        (Decode.field "parameters" (Decode.maybe Decode.value))


decodeIntOrInf : Decode.Decoder IntOrInf
decodeIntOrInf =
    Decode.oneOf
        [ Decode.map IntValue Decode.int
        , Decode.map (always Inf) Decode.string
        ]


decodeClientSecret : Decode.Decoder ClientSecret
decodeClientSecret =
    Decode.map2 ClientSecret
        (Decode.field "value" Decode.string)
        (Decode.field "expires_at" Decode.int |> Decode.map Time.millisToPosix)



-- createSession : SessionInput -> Task Http.Error SessionOutput


createSession : SessionInput -> Ext.Http.TaskInput (Ext.Http.Error String) SessionOutput
createSession sessionInput =
    { method = "POST"
    , headers = []
    , url = "/realtime/sessions"
    , body = Http.jsonBody <| encodeSessionInput sessionInput
    , resolver =
        Http.stringResolver
            (Ext.Http.jsonResolver decodeSessionOutput >> Result.map .data)
    , timeout = Nothing
    }
