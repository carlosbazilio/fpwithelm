
import Browser
import Html exposing (Html, button, div, text, input, textarea, form)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, name, value)


-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

init = { nota = { titulo = "", texto = "", labels = []}, 
         notas = [] }

type alias Model =
    { nota : Nota
    , notas : List Nota
    }

type alias Nota =
    { titulo : String
    , texto : String
    , labels : List String 
    }


-- VIEW

view : Model -> Html Msg
view model =
    div [ ] ( [ form [] [ novanota model.nota ] ] ++ (List.map nota2text model.notas) )
    
novanota : Nota -> Html Msg
novanota nota =
    div [ ] [ div [] [ input [ type_ "text", name "titulo", value nota.titulo, onInput InsereTitulo ] [ ] ],
              div [] [ textarea [ name "conteudo", value nota.texto, onInput InsereTexto ] [ ] ], 
              div [] [ input [ type_ "text", name "tags", value (String.join ", " nota.labels), onInput InsereLabels ] [ ] ],
              div [] [ button [ onClick CriaNota ] [ text "Cria Nota" ] ] ]

nota2text : Nota -> Html Msg
nota2text { titulo, texto, labels } = 
    div [ ] [ input [ type_ "text", name "titulo", value titulo ] [],
              input [ type_ "text", name "conteudo", value texto ] [],
              input [ type_ "text", name "tags", value (String.join ", " labels) ] [] ]


-- UPDATE
    
type Msg 
  = CriaNota
  | InsereTitulo String
  | InsereTexto String
  | InsereLabels String

update : Msg -> Model -> Model 
update msg model =
  case msg of
    CriaNota ->
      { model | nota = Nota "" "" [], 
                notas = (Nota model.nota.titulo model.nota.texto model.nota.labels) :: model.notas }

    InsereTitulo titulo ->
      { model | nota = (Nota titulo model.nota.texto model.nota.labels) }

    InsereTexto texto ->
      { model | nota = (Nota model.nota.titulo texto model.nota.labels) }

    InsereLabels labels ->
      { model | nota = (Nota model.nota.titulo model.nota.texto (String.split "," labels)) }

