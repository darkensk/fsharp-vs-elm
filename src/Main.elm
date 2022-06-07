module Main exposing (Model, Msg, main)

import Browser
import Html as Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import Url.Builder as UrlBuilder


type alias Flags =
    Int


type alias Contact =
    { firstName : String
    , lastName : String
    , nickname : Maybe String
    }


type alias Model =
    { contactDetail : WebData Contact
    , contactList : WebData (List Contact)
    , search : List Contact
    , searchQuery : String
    , lastCreatedId : Int
    , form : Contact
    , appInitiatedAt : Int
    }


emptyNewContactForm : Contact
emptyNewContactForm =
    { firstName = ""
    , lastName = ""
    , nickname = Nothing
    }


type InputId
    = FirstName
    | LastName
    | Nickname


type Msg
    = GetAllContactsResponse (WebData (List Contact))
    | GetSearchResponse (WebData (List Contact))
    | GetContactDetail (WebData Contact)
    | GetCreateContactResponse (WebData Int)
    | LoadContactList
    | SubmitSearch
    | SubmitCreateContact
    | InputFormField InputId String
    | InsertSearchQuery String


contactEncoder : Contact -> Encode.Value
contactEncoder contact =
    Encode.object
        ([ ( "firstName", Encode.string contact.firstName )
         , ( "lastName", Encode.string contact.lastName )
         ]
            ++ (case contact.nickname of
                    Just "" ->
                        []

                    Just nick ->
                        [ ( "nickName", Encode.string nick ) ]

                    Nothing ->
                        []
               )
        )


contactListDecoder : Decode.Decoder (List Contact)
contactListDecoder =
    Decode.list contactDecoder


contactDecoder : Decode.Decoder Contact
contactDecoder =
    Decode.map3 Contact
        (Decode.field "firstName" Decode.string)
        (Decode.field "lastName" Decode.string)
        (Decode.maybe (Decode.field "nickName" Decode.string))



-- contactDecoder : Decode.Decoder Contact
-- contactDecoder =
--     Decode.succeed Contact
--         |> Pipeline.required "firstName" Decode.string
--         |> Pipeline.required "lastName" Decode.string
--         |> Pipeline.optional "nickName" (Decode.maybe Decode.string) Nothing


getAllContacts : Cmd Msg
getAllContacts =
    Http.get
        { url = "https://fsharpvselm.azurewebsites.net/"
        , expect = Http.expectJson (RemoteData.fromResult >> GetAllContactsResponse) contactListDecoder
        }


getSearchResults : String -> Cmd Msg
getSearchResults searchQuery =
    Http.get
        { url = UrlBuilder.crossOrigin "https://fsharpvselm.azurewebsites.net" [] [ UrlBuilder.string "search" searchQuery ]
        , expect = Http.expectJson (RemoteData.fromResult >> GetSearchResponse) contactListDecoder
        }


postCreateContact : Contact -> Cmd Msg
postCreateContact contact =
    Http.post
        { url = "https://fsharpvselm.azurewebsites.net/"
        , body = Http.jsonBody <| contactEncoder contact
        , expect = Http.expectJson (RemoteData.fromResult >> GetCreateContactResponse) Decode.int
        }


getContactDetail : Int -> Cmd Msg
getContactDetail contactId =
    Http.get
        { url = UrlBuilder.crossOrigin "https://fsharpvselm.azurewebsites.net" [ String.fromInt contactId ] []
        , expect = Http.expectJson (RemoteData.fromResult >> GetContactDetail) contactDecoder
        }


init : Flags -> ( Model, Cmd msg )
init flags =
    ( { contactDetail = RemoteData.NotAsked
      , contactList = RemoteData.NotAsked
      , search = []
      , searchQuery = ""
      , lastCreatedId = 0
      , form = emptyNewContactForm
      , appInitiatedAt = flags
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAllContactsResponse response ->
            ( { model | contactList = response }, Cmd.none )

        GetSearchResponse response ->
            let
                searchResult =
                    case response of
                        RemoteData.Success data ->
                            data

                        _ ->
                            []
            in
            ( { model | search = searchResult }, Cmd.none )

        GetContactDetail response ->
            ( { model | contactDetail = response }, Cmd.none )

        GetCreateContactResponse response ->
            let
                newCmd =
                    response
                        |> RemoteData.map getContactDetail
                        |> RemoteData.withDefault Cmd.none
            in
            ( model, newCmd )

        LoadContactList ->
            ( { model | contactList = RemoteData.Loading }, getAllContacts )

        SubmitSearch ->
            let
                searchCmd =
                    if String.isEmpty model.searchQuery then
                        Cmd.none

                    else
                        getSearchResults model.searchQuery
            in
            ( model, searchCmd )

        SubmitCreateContact ->
            let
                { firstName, lastName } =
                    model.form

                createCmd =
                    if String.isEmpty firstName || String.isEmpty lastName then
                        Cmd.none

                    else
                        postCreateContact model.form
            in
            ( model, createCmd )

        InputFormField inputId value ->
            let
                { form } =
                    model

                updatedForm =
                    case inputId of
                        FirstName ->
                            { form | firstName = value }

                        LastName ->
                            { form | lastName = value }

                        Nickname ->
                            { form | nickname = Just value }
            in
            ( { model | form = updatedForm }, Cmd.none )

        InsertSearchQuery searchQuery ->
            ( { model | searchQuery = searchQuery }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "container" ]
        [ Html.h1 [] [ Html.text "Fsharp vs Elm" ]
        , Html.p [] [ Html.text """Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Mauris metus. Nullam dapibus fermentum ipsum. Vivamus luctus egestas leo. Et harum quidem rerum facilis est et expedita distinctio. Maecenas sollicitudin.""" ]
        , Html.div [ Attributes.class "column" ]
            [ Html.form [ Attributes.class "column", Events.onSubmit SubmitCreateContact ]
                [ Html.label
                    [ Attributes.class "label"
                    , Attributes.for "fname"
                    ]
                    [ Html.text "First name:" ]
                , Html.input
                    [ Attributes.type_ "text"
                    , Attributes.id "fname"
                    , Attributes.name "fname"
                    , Attributes.value model.form.firstName
                    , Events.onInput (InputFormField FirstName)
                    ]
                    []
                , Html.label
                    [ Attributes.class "label"
                    , Attributes.for "lname"
                    ]
                    [ Html.text "Last name:" ]
                , Html.input
                    [ Attributes.type_ "text"
                    , Attributes.id "lname"
                    , Attributes.name "lname"
                    , Attributes.value model.form.lastName
                    , Events.onInput (InputFormField LastName)
                    ]
                    []
                , Html.label
                    [ Attributes.class "label"
                    , Attributes.for "nickname"
                    ]
                    [ Html.text "Nickname:" ]
                , Html.input
                    [ Attributes.type_ "text"
                    , Attributes.id "nickname"
                    , Attributes.name "nickname"
                    , case model.form.nickname of
                        Just nick ->
                            Attributes.value nick

                        Nothing ->
                            Attributes.value ""
                    , Events.onInput (InputFormField Nickname)
                    ]
                    []
                , Html.button
                    [ Attributes.class "button"
                    , Attributes.type_ "submit"
                    ]
                    [ Html.text "Create new contact" ]
                ]
            , case model.contactDetail of
                RemoteData.Success newContact ->
                    Html.div []
                        [ Html.h3 [] [ Html.text "Created!" ]
                        , contactView newContact
                        ]

                _ ->
                    Html.div [] []
            ]
        , Html.div []
            [ Html.button
                [ Attributes.class "button"
                , Events.onClick LoadContactList
                ]
                [ Html.text "Load contact list" ]
            , case model.contactList of
                RemoteData.NotAsked ->
                    Html.div [] [ Html.text "Contact list not loaded" ]

                RemoteData.Loading ->
                    Html.div [] [ Html.text "Contact list is loading" ]

                RemoteData.Failure _ ->
                    Html.div [] [ Html.text "Contact list loading failed" ]

                RemoteData.Success data ->
                    if List.isEmpty data then
                        Html.div [] [ Html.text "Contact list is empty" ]

                    else
                        Html.div [ Attributes.class "row" ] <| List.map contactView data
            ]
        , Html.div [ Attributes.class "column" ]
            [ Html.div [ Attributes.class "row" ]
                [ Html.form
                    [ Attributes.class "row"
                    , Events.onSubmit SubmitSearch
                    ]
                    [ Html.label
                        [ Attributes.class "label"
                        , Attributes.for "query"
                        ]
                        [ Html.text "Search" ]
                    , Html.input
                        [ Attributes.type_ "text"
                        , Attributes.id "query"
                        , Attributes.name "query"
                        , Attributes.value model.searchQuery
                        , Events.onInput InsertSearchQuery
                        ]
                        []
                    , Html.button
                        [ Attributes.class "button"
                        , Attributes.type_ "submit"
                        ]
                        [ Html.text "Search for contact" ]
                    ]
                ]
            , if List.isEmpty model.search then
                Html.div [] [ Html.text "Search results are empty" ]

              else
                Html.div [ Attributes.class "row" ] <| List.map contactView model.search
            ]
        , Html.footer [] [ Html.span [] [ Html.text <| String.fromInt model.appInitiatedAt ] ]
        ]


contactView : Contact -> Html Msg
contactView contact =
    Html.div [ Attributes.class "card centered" ]
        [ Html.img [ Attributes.src "https://via.placeholder.com/100" ] []
        , Html.div [ Attributes.class "column" ]
            [ Html.div [] [ Html.span [ Attributes.class "label" ] [ Html.text "First Name: " ], Html.text contact.firstName ]
            , Html.div [] [ Html.span [ Attributes.class "label" ] [ Html.text "Last Name: " ], Html.text contact.lastName ]
            , case contact.nickname of
                Just nick ->
                    Html.div [] [ Html.span [ Attributes.class "label" ] [ Html.text "Nickname: " ], Html.text nick ]

                Nothing ->
                    Html.div [] []
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
