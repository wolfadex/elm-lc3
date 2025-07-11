module Main exposing (main)

import Array exposing (Array)
import Bitwise
import Browser
import Css
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Parser exposing ((|.), (|=), Parser)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { current : Computer
    , initial : Computer
    , rawSource : String
    , compileError : Maybe String
    }


type alias Computer =
    { registers : Array Int
    , register_programmingCounter : Int
    , register_condition : Flag
    , memory : Array Instruction
    }


type Instruction
    = ADD Int Int ModeValue
    | AND Int Int ModeValue
    | NOT Int Int
    | BR
    | JMP Int
    | JSR
    | LD Int Int
    | LDI
    | LDR
    | ST
    | STI
    | STR
    | LEA
    | TRAP TrapCode
    | Literal Int


type ModeValue
    = Immediate Int
    | RegisterValue Int


type TrapCode
    = GETC -- = 0x20,  /* get character from keyboard, not echoed onto the terminal */
    | OUT -- = 0x21,   /* output a character */
    | PUTS -- = 0x22,  /* output a word string */
    | IN -- = 0x23,    /* get character from keyboard, echoed onto the terminal */
    | PUTSP -- = 0x24, /* output a byte string */
    | HALT -- = 0x25   /* halt the program */


type Flag
    = POS
    | ZER
    | NEG


type MemoryMappedRegisters
    = MR_KBSR -- = 0xFE00, /* keyboard status */
    | MR_KBDR -- = 0xFE02  /* keyboard data */


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initial =
            { registers = Array.repeat 8 0
            , register_programmingCounter = 0
            , register_condition = ZER
            , memory =
                -- Arra.repeat 65536 0
                -- Array.empty
                Array.fromList
                    [ ADD 1 0 (Immediate 3)
                    , ADD 2 0 (Immediate -5)
                    , ADD 0 1 (RegisterValue 2)
                    , TRAP HALT
                    ]
            }
    in
    ( { current = initial
      , initial = initial
      , rawSource = """ADD R1, R0, #3
ADD R2, R0, #-5
ADD R0, R1, R2
HALT"""
      , compileError = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = Run
    | Step
    | Reset
    | UserChangedRawSource String
    | UserCllickedCompile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            ( { model | current = run model.current }, Cmd.none )

        Step ->
            ( { model | current = step model.current }, Cmd.none )

        Reset ->
            ( { model | current = model.initial }, Cmd.none )

        UserChangedRawSource rawSource ->
            ( { model | rawSource = rawSource }, Cmd.none )

        UserCllickedCompile ->
            case parse model.rawSource of
                Err err ->
                    ( { model | compileError = Just err }, Cmd.none )

                Ok instructions ->
                    let
                        initial =
                            model.initial

                        newInitial =
                            { initial | memory = instructions }
                    in
                    ( { model
                        | initial = newInitial
                        , current = newInitial
                        , compileError = Nothing
                      }
                    , Cmd.none
                    )


run : Computer -> Computer
run model =
    case Array.get model.register_programmingCounter model.memory of
        Nothing ->
            Debug.todo "memory access error"

        Just inst ->
            let
                ( m, continue ) =
                    eval inst { model | register_programmingCounter = model.register_programmingCounter + 1 }
            in
            if continue then
                run m

            else
                m


step : Computer -> Computer
step model =
    case Array.get model.register_programmingCounter model.memory of
        Nothing ->
            Debug.todo "memory access error"

        Just inst ->
            let
                ( m, _ ) =
                    eval inst { model | register_programmingCounter = model.register_programmingCounter + 1 }
            in
            m


eval : Instruction -> Computer -> ( Computer, Bool )
eval instruction model =
    case instruction of
        ADD destinationRegister leftRegister rightValue ->
            case rightValue of
                Immediate rightInt ->
                    case Array.get leftRegister model.registers of
                        Nothing ->
                            Debug.todo "register access error"

                        Just leftInt ->
                            ( { model | registers = Array.set destinationRegister (leftInt + rightInt) model.registers }
                                |> setFlags destinationRegister
                            , True
                            )

                RegisterValue rightRegister ->
                    case Array.get leftRegister model.registers of
                        Nothing ->
                            Debug.todo "register access error"

                        Just leftInt ->
                            case Array.get rightRegister model.registers of
                                Nothing ->
                                    Debug.todo "register access error"

                                Just rightInt ->
                                    ( { model | registers = Array.set destinationRegister (leftInt + rightInt) model.registers }
                                        |> setFlags destinationRegister
                                    , True
                                    )

        AND destinationRegister leftRegister rightValue ->
            case rightValue of
                Immediate rightInt ->
                    case Array.get leftRegister model.registers of
                        Nothing ->
                            Debug.todo "register access error"

                        Just leftInt ->
                            ( { model | registers = Array.set destinationRegister (Bitwise.and leftInt rightInt) model.registers }
                                |> setFlags destinationRegister
                            , True
                            )

                RegisterValue rightRegister ->
                    case Array.get leftRegister model.registers of
                        Nothing ->
                            Debug.todo "register access error"

                        Just leftInt ->
                            case Array.get rightRegister model.registers of
                                Nothing ->
                                    Debug.todo "register access error"

                                Just rightInt ->
                                    ( { model | registers = Array.set destinationRegister (Bitwise.and leftInt rightInt) model.registers }
                                        |> setFlags destinationRegister
                                    , True
                                    )

        NOT destinationRegister sourceRegister ->
            case Array.get sourceRegister model.registers of
                Nothing ->
                    Debug.todo "register access error"

                Just sourceInt ->
                    ( { model | registers = Array.set destinationRegister (Bitwise.complement sourceInt) model.registers }
                        |> setFlags destinationRegister
                    , True
                    )

        BR ->
            Debug.todo "implement"

        JMP destinationRegister ->
            case Array.get destinationRegister model.registers of
                Nothing ->
                    Debug.todo "register access error"

                Just registerId ->
                    ( { model | register_programmingCounter = registerId }
                    , True
                    )

        JSR ->
            Debug.todo "implement"

        LD destinationRegister pcOffset ->
            case Array.get (model.register_programmingCounter + pcOffset) model.memory of
                Nothing ->
                    Debug.todo "memory access error"

                Just (Literal val) ->
                    ( { model | registers = Array.set destinationRegister val model.registers }
                        |> setFlags destinationRegister
                    , True
                    )

                Just _ ->
                    Debug.todo "memory value mismatch"

        LDI ->
            Debug.todo "implement"

        LDR ->
            Debug.todo "implement"

        ST ->
            Debug.todo "implement"

        STI ->
            Debug.todo "implement"

        STR ->
            Debug.todo "implement"

        LEA ->
            Debug.todo "implement"

        TRAP trap ->
            case trap of
                GETC ->
                    -- = 0x20,  /* get character from keyboard, not echoed onto the terminal */
                    Debug.todo "implement"

                OUT ->
                    -- = 0x21,   /* output a character */
                    Debug.todo "implement"

                PUTS ->
                    -- = 0x22,  /* output a word string */
                    Debug.todo "implement"

                IN ->
                    -- = 0x23,    /* get character from keyboard, echoed onto the terminal */
                    Debug.todo "implement"

                PUTSP ->
                    -- = 0x24, /* output a byte string */
                    Debug.todo "implement"

                HALT ->
                    -- = 0x25   /* halt the program */
                    ( model, False )

        Literal _ ->
            Debug.todo "found literal but expected an instruction"


setFlags : Int -> Computer -> Computer
setFlags r model =
    case Array.get r model.registers of
        Just 0 ->
            { model | register_condition = ZER }

        Just i ->
            if i < 0 then
                { model | register_condition = NEG }

            else
                { model | register_condition = POS }

        Nothing ->
            Debug.todo "bad register access"


view : Model -> Browser.Document Msg
view model =
    { title = "LC-3"
    , body =
        [ Html.h1 [] [ Html.text "Little Computer 3" ]
        , Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "20rem auto auto"
            ]
            [ Html.table
                []
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [ Html.Attributes.scope "col" ]
                            [ Html.text "Register" ]
                        , Html.th
                            [ Html.Attributes.scope "col" ]
                            [ Html.text "Value" ]
                        ]
                    ]
                , model.current.registers
                    |> Array.toList
                    |> List.indexedMap
                        (\id value ->
                            Html.tr []
                                [ Html.td [] [ Html.text (String.fromInt id) ]
                                , Html.td [] [ Html.text (String.fromInt value) ]
                                ]
                        )
                    |> (\rs ->
                            rs
                                ++ [ Html.tr []
                                        [ Html.td [] [ Html.text "PC" ]
                                        , Html.td [] [ Html.text (String.fromInt model.current.register_programmingCounter) ]
                                        ]
                                   , Html.tr []
                                        [ Html.td [] [ Html.text "COND" ]
                                        , Html.td []
                                            [ Html.text <|
                                                case model.current.register_condition of
                                                    ZER ->
                                                        "Zero"

                                                    NEG ->
                                                        "Negative"

                                                    POS ->
                                                        "Positive"
                                            ]
                                        ]
                                   ]
                       )
                    |> Html.tbody
                        []
                ]
            , Html.div [ Html.Attributes.style "padding-left" "2rem" ]
                [ Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "gap" "1rem"
                    , Html.Attributes.style "padding-bottom" "1rem"
                    ]
                    [ Html.button [ Html.Events.onClick Run ]
                        [ Html.text "Run" ]
                    , Html.button [ Html.Events.onClick Step ]
                        [ Html.text "Step" ]
                    , Html.button [ Html.Events.onClick Reset ]
                        [ Html.text "Reset" ]
                    ]
                , let
                    toReg i =
                        " R" ++ String.fromInt i
                  in
                  model.current.memory
                    |> Array.toList
                    |> List.indexedMap
                        (\offset mem ->
                            [ Html.span
                                [ Html.Attributes.style "text-align" "center"
                                , if model.current.register_programmingCounter == offset then
                                    Css.memoryPointedAt

                                  else
                                    Html.Attributes.class ""
                                ]
                                [ Html.text <|
                                    if model.current.register_programmingCounter == offset then
                                        ">"

                                    else
                                        " "
                                ]
                            , Html.span
                                [ Html.Attributes.style "text-align" "right"
                                , if model.current.register_programmingCounter == offset then
                                    Css.memoryPointedAt

                                  else
                                    Html.Attributes.class ""
                                ]
                                [ Html.text (String.fromInt offset) ]
                            , Html.span
                                [ Html.Attributes.style "padding-left" "1rem"
                                , if model.current.register_programmingCounter == offset then
                                    Css.memoryPointedAt

                                  else
                                    Html.Attributes.class ""
                                ]
                                [ Html.text <|
                                    case mem of
                                        ADD destinationRegister leftRegister modeValue ->
                                            String.join " "
                                                [ "ADD"
                                                , toReg destinationRegister ++ ","
                                                , toReg leftRegister ++ ","
                                                , case modeValue of
                                                    Immediate int ->
                                                        "#" ++ String.fromInt int

                                                    RegisterValue rightRegister ->
                                                        toReg rightRegister
                                                ]

                                        AND destinationRegister leftRegister modeValue ->
                                            ""

                                        NOT destinationRegister sourceRegister ->
                                            ""

                                        BR ->
                                            ""

                                        JMP destinationRegister ->
                                            ""

                                        JSR ->
                                            ""

                                        LD destinationRegister pcOffset ->
                                            String.join " "
                                                [ "LD"
                                                , toReg destinationRegister
                                                , String.fromInt pcOffset
                                                ]

                                        LDI ->
                                            ""

                                        LDR ->
                                            ""

                                        ST ->
                                            ""

                                        STI ->
                                            ""

                                        STR ->
                                            ""

                                        LEA ->
                                            ""

                                        TRAP trap ->
                                            case trap of
                                                GETC ->
                                                    -- = 0x20,  /* get character from keyboard, not echoed onto the terminal */
                                                    ""

                                                OUT ->
                                                    -- = 0x21,   /* output a character */
                                                    ""

                                                PUTS ->
                                                    -- = 0x22,  /* output a word string */
                                                    ""

                                                IN ->
                                                    -- = 0x23,    /* get character from keyboard, echoed onto the terminal */
                                                    ""

                                                PUTSP ->
                                                    -- = 0x24, /* output a byte string */
                                                    ""

                                                HALT ->
                                                    -- = 0x25   /* halt the program */
                                                    "HALT"

                                        Literal _ ->
                                            ""
                                ]
                            ]
                        )
                    |> List.concat
                    |> Html.div
                        [ Html.Attributes.style "display" "grid"
                        , Html.Attributes.style "grid-template-columns" "2rem 2rem auto"
                        , Html.Attributes.style "align-content" "flex-start"
                        ]
                ]
            , Html.label
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                ]
                [ Html.span []
                    [ Html.text "Raw source "
                    , Html.button
                        [ Html.Events.onClick UserCllickedCompile ]
                        [ Html.text "Compile" ]
                    ]
                , Html.textarea
                    [ Html.Attributes.value model.rawSource
                    , Html.Events.onInput UserChangedRawSource
                    ]
                    []
                , model.compileError
                    |> Maybe.withDefault ""
                    |> Html.text
                ]
            ]
        ]
    }



--


parse : String -> Result String (Array Instruction)
parse source =
    case Parser.run parseSource source of
        Ok instructions ->
            Ok instructions

        Err err ->
            Err (Debug.toString err)


parseSource : Parser (Array Instruction)
parseSource =
    Parser.succeed Array.fromList
        |. Parser.spaces
        |= Parser.loop [] parseInstructions


parseInstructions : List Instruction -> Parser (Parser.Step (List Instruction) (List Instruction))
parseInstructions reverseInstructions =
    Parser.oneOf
        [ Parser.succeed (\instruction -> Parser.Loop (instruction :: reverseInstructions))
            |= parseInstruction
        , Parser.succeed (Parser.Done (List.reverse reverseInstructions))
        ]


parseInstruction : Parser Instruction
parseInstruction =
    Parser.oneOf
        [ parseAdd
        , parseTrap
        ]


parseAdd : Parser Instruction
parseAdd =
    Parser.succeed ADD
        |. Parser.keyword "ADD"
        |. Parser.spaces
        |= parseRegister
        |. Parser.chompIf ((==) ',')
        |. Parser.spaces
        |= parseRegister
        |. Parser.chompIf ((==) ',')
        |. Parser.spaces
        |= Parser.oneOf
            [ parseRegister |> Parser.map RegisterValue
            , parseLiteral |> Parser.map Immediate
            ]
        |. Parser.spaces


parseRegister : Parser Int
parseRegister =
    Parser.succeed identity
        |. Parser.chompIf ((==) 'R')
        |= parseInt


parseLiteral : Parser Int
parseLiteral =
    Parser.succeed identity
        |. Parser.chompIf ((==) '#')
        |= parseInt


parseTrap : Parser Instruction
parseTrap =
    Parser.succeed TRAP
        |= Parser.oneOf
            [ Parser.succeed GETC
                |. Parser.keyword "GETC"
            , Parser.succeed OUT
                |. Parser.keyword "OUT"
            , Parser.succeed PUTS
                |. Parser.keyword "PUTS"
            , Parser.succeed IN
                |. Parser.keyword "IN"
            , Parser.succeed PUTSP
                |. Parser.keyword "PUTSP"
            , Parser.succeed HALT
                |. Parser.keyword "HALT"
            ]
        |. Parser.spaces


parseInt : Parser Int
parseInt =
    Parser.succeed ()
        |. Parser.oneOf
            [ Parser.chompIf ((==) '-')
            , Parser.succeed ()
            ]
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\digits ->
                case String.toInt digits of
                    Nothing ->
                        Parser.problem "Expected an integer"

                    Just int ->
                        Parser.succeed int
            )
