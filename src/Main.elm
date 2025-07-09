module Main exposing (main)

import Array exposing (Array)
import Bitwise
import Browser
import Css
import Html exposing (Html)
import Html.Attributes
import Html.Events


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
    ( { current = initial, initial = initial }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = Run
    | Step
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run ->
            ( { model | current = run model.current }, Cmd.none )

        Step ->
            ( { model | current = step model.current }, Cmd.none )

        Reset ->
            ( { model | current = model.initial }, Cmd.none )


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
            , Html.Attributes.style "grid-template-columns" "20rem auto"
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
            ]
        ]
    }
