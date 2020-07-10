module Html.Parser exposing
    ( run, Node(..), Attribute
    , node, nodeToString
    , Voidness(..)
    , runWithIsVoid, nodeWithIsVoid, nodeToStringWithIsVoid
    , isVoidElement, isLooselyVoidElement
    )

{-| Parse HTML 5 in Elm.
See <https://www.w3.org/TR/html5/syntax.html>

@docs run, Node, Attribute


# Internals

If you are building a parser of your own using [`elm/parser`][elm-parser] and
you need to parse HTML... This section is for you!

[elm-parser]: https://package.elm-lang.org/packages/elm/parser/latest

@docs node, nodeToString


# Customization

If you need to customize the default list of void node names, use these.

@docs Voidness
@docs runWithIsVoid, nodeWithIsVoid, nodeToStringWithIsVoid
@docs isVoidElement, isLooselyVoidElement

-}

import Dict exposing (Dict)
import Hex
import Html.Parser.NamedCharacterReferences as NamedCharacterReferences
import Parser exposing ((|.), (|=), Parser)


{-| Run the parser!

    run "<div><p>Hello, world!</p></div>"
    -- => Ok [ Element "div" [] [ Element "p" [] [ Text "Hello, world!" ] ] ]

-}
run : String -> Result (List Parser.DeadEnd) (List Node)
run =
    runWithIsVoid isVoidElement


{-| Run the parser, customizing which elements may be void.

    run =
        runWithIsVoid isVoidElement

-}
runWithIsVoid : (String -> Voidness) -> String -> Result (List Parser.DeadEnd) (List Node)
runWithIsVoid isVoid str =
    if String.isEmpty str then
        Ok []

    else
        Parser.run (oneOrMore "node" <| nodeWithIsVoid isVoid) str



-- Node


{-| An HTML node. It can either be:

  - Text
  - Element (with its **tag name**, **attributes** and **children**)
  - Comment

-}
type Node
    = Text String
    | Element String (List Attribute) (List Node)
    | Comment String


{-| An HTML attribute. For instance:

    ( "href", "https://elm-lang.org" )

-}
type alias Attribute =
    ( String, String )


{-| Parse an HTML node.

You can use this in your own parser to add support for HTML 5.

-}
node : Parser Node
node =
    nodeWithIsVoid isVoidElement


{-| Parse an HTML node, customizing which element names may be void.

    node =
        nodeWithIsVoid isVoidElement

To allow any element to be void:

    nodeWithIsVoid (\_ -> True)

-}
nodeWithIsVoid : (String -> Voidness) -> Parser Node
nodeWithIsVoid isVoid =
    Parser.oneOf
        [ text
        , comment
        , element isVoid
        ]


{-| Turn a parser node back into its HTML string.

For instance:

    Element "a"
        [ ( "href", "https://elm-lang.org" ) ]
        [ Text "Elm" ]
        |> nodeToString

Produces `<a href="https://elm-lang.org">Elm</a>`.

-}
nodeToString : Node -> String
nodeToString =
    nodeToStringWithIsVoid isVoidElement


{-| Same as `nodeToString`, but allows you to customize the void elements.

    nodeToString =
        nodeToStringWithIsVoid isVoidElement

If you parse with `nodeWithIsVoid`, you should use the same predicate
(or `(\_ -> True)`) to turn the `Node` back into a `String`.

-}
nodeToStringWithIsVoid : (String -> Voidness) -> Node -> String
nodeToStringWithIsVoid isVoid node_ =
    let
        attributeToString ( attr, value ) =
            attr ++ "=\"" ++ value ++ "\""
    in
    case node_ of
        Text text_ ->
            text_

        Element name attributes children ->
            let
                maybeAttributes =
                    case attributes of
                        [] ->
                            ""

                        _ ->
                            " " ++ String.join " " (List.map attributeToString attributes)
            in
            case isVoid name of
                IsVoid ->
                    String.concat
                        [ "<"
                        , name
                        , maybeAttributes
                        , ">"
                        ]

                void ->
                    if children == [] && void == MaybeVoid then
                        String.concat
                            [ "<"
                            , name
                            , maybeAttributes
                            , "/>"
                            ]

                    else
                        String.concat
                            [ "<"
                            , name
                            , maybeAttributes
                            , ">"
                            , String.join "" (List.map nodeToString children)
                            , "</"
                            , name
                            , ">"
                            ]

        Comment comment_ ->
            "<!-- " ++ comment_ ++ " -->"



-- Text


text : Parser Node
text =
    Parser.oneOf
        [ Parser.getChompedString (chompOneOrMore (\c -> c /= '<' && c /= '&'))
        , characterReference
        ]
        |> oneOrMore "text element"
        |> Parser.map (String.join "" >> Text)


characterReference : Parser String
characterReference =
    Parser.succeed identity
        |. Parser.chompIf ((==) '&')
        |= Parser.oneOf
            [ Parser.backtrackable namedCharacterReference
                |. chompSemicolon
            , Parser.backtrackable numericCharacterReference
                |. chompSemicolon
            , Parser.succeed "&"
            ]


namedCharacterReference : Parser String
namedCharacterReference =
    Parser.getChompedString (chompOneOrMore Char.isAlpha)
        |> Parser.map
            (\reference ->
                Dict.get reference NamedCharacterReferences.dict
                    |> Maybe.withDefault ("&" ++ reference ++ ";")
            )


numericCharacterReference : Parser String
numericCharacterReference =
    let
        codepoint =
            Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.chompIf (\c -> c == 'x' || c == 'X')
                    |= hexadecimal
                , Parser.succeed identity
                    |. Parser.chompWhile ((==) '0')
                    |= Parser.int
                ]
    in
    Parser.succeed identity
        |. Parser.chompIf ((==) '#')
        |= Parser.map (Char.fromCode >> String.fromChar) codepoint



-- Element


element : (String -> Voidness) -> Parser Node
element isVoid =
    Parser.succeed Tuple.pair
        |. Parser.chompIf ((==) '<')
        |= tagName
        |. Parser.chompWhile isSpaceCharacter
        |= tagAttributes
        |> Parser.andThen
            (\( name, attributes ) ->
                let
                    whenVoid =
                        Parser.succeed (Element name attributes [])
                            |. Parser.oneOf
                                [ Parser.chompIf ((==) '/')
                                , Parser.succeed ()
                                ]
                            |. Parser.chompIf ((==) '>')

                    whenNotVoid =
                        Parser.succeed (Element name attributes)
                            |. Parser.chompIf ((==) '>')
                            |= many (Parser.backtrackable <| nodeWithIsVoid isVoid)
                            |. closingTag name
                in
                case isVoid name of
                    IsVoid ->
                        whenVoid

                    IsNotVoid ->
                        whenNotVoid

                    MaybeVoid ->
                        Parser.oneOf [ whenNotVoid, whenVoid ]
            )


tagName : Parser String
tagName =
    Parser.getChompedString
        (Parser.chompIf Char.isAlphaNum
            |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '-')
        )
        |> Parser.map String.toLower


tagAttributes : Parser (List Attribute)
tagAttributes =
    many tagAttribute


tagAttribute : Parser Attribute
tagAttribute =
    Parser.succeed Tuple.pair
        |= tagAttributeName
        |. Parser.chompWhile isSpaceCharacter
        |= tagAttributeValue
        |. Parser.chompWhile isSpaceCharacter


tagAttributeName : Parser String
tagAttributeName =
    Parser.getChompedString (chompOneOrMore isTagAttributeCharacter)
        |> Parser.map String.toLower


tagAttributeValue : Parser String
tagAttributeValue =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.chompIf ((==) '=')
            |. Parser.chompWhile isSpaceCharacter
            |= Parser.oneOf
                [ tagAttributeUnquotedValue
                , tagAttributeQuotedValue '"'
                , tagAttributeQuotedValue '\''
                ]
        , Parser.succeed ""
        ]


tagAttributeUnquotedValue : Parser String
tagAttributeUnquotedValue =
    let
        isUnquotedValueChar c =
            not (isSpaceCharacter c) && c /= '"' && c /= '\'' && c /= '=' && c /= '<' && c /= '>' && c /= '`' && c /= '&'
    in
    Parser.oneOf
        [ chompOneOrMore isUnquotedValueChar
            |> Parser.getChompedString
        , characterReference
        ]
        |> oneOrMore "attribute value"
        |> Parser.map (String.join "")


tagAttributeQuotedValue : Char -> Parser String
tagAttributeQuotedValue quote =
    let
        isQuotedValueChar c =
            c /= quote && c /= '&'
    in
    Parser.succeed identity
        |. Parser.chompIf ((==) quote)
        |= (Parser.oneOf
                [ Parser.getChompedString (chompOneOrMore isQuotedValueChar)
                , characterReference
                ]
                |> many
                |> Parser.map (String.join "")
           )
        |. Parser.chompIf ((==) quote)


closingTag : String -> Parser ()
closingTag name =
    let
        chompName =
            chompOneOrMore (\c -> not (isSpaceCharacter c) && c /= '>')
                |> Parser.getChompedString
                |> Parser.andThen
                    (\closingName ->
                        if String.toLower closingName == name then
                            Parser.succeed ()

                        else
                            Parser.problem ("closing tag does not match opening tag: " ++ name)
                    )
    in
    Parser.chompIf ((==) '<')
        |. Parser.chompIf ((==) '/')
        |. chompName
        |. Parser.chompWhile isSpaceCharacter
        |. Parser.chompIf ((==) '>')



-- Comment


comment : Parser Node
comment =
    Parser.succeed Comment
        |. Parser.token "<!"
        |. Parser.token "--"
        |= Parser.getChompedString (Parser.chompUntil "-->")
        |. Parser.token "-->"



-- Void elements


{-| Whether an element may have no closing tag.

`IsVoid` means it may NEVER have a closing tag.
`IsNotVoid` means it must ALWAYS have a closing tag.
`MaybeVoid` means it can, but does not need to have a closing tag.

-}
type Voidness
    = IsVoid
    | IsNotVoid
    | MaybeVoid


{-| Determine whether an element may have no closing tag.

Always returns either `IsVoid` or `IsNotVoid`.

Your custom isVoid function may also return `MaybeVoid`.

-}
isVoidElement : String -> Voidness
isVoidElement name =
    if List.member name voidElements then
        IsVoid

    else
        IsNotVoid


{-| A common isVoid function for loose HTML parsing.

Returns `IsVoid` when `isVoidElement` does.
Otherwise returns `MaybeVoid`.

-}
isLooselyVoidElement : String -> Voidness
isLooselyVoidElement name =
    case isVoidElement name of
        IsVoid ->
            IsVoid

        _ ->
            MaybeVoid


voidElements : List String
voidElements =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]



-- Character validators


isTagAttributeCharacter : Char -> Bool
isTagAttributeCharacter c =
    not (isSpaceCharacter c) && c /= '"' && c /= '\'' && c /= '>' && c /= '/' && c /= '='


isSpaceCharacter : Char -> Bool
isSpaceCharacter c =
    c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}' || c == '\u{000C}' || c == '\u{00A0}'



-- Chomp


chompSemicolon : Parser ()
chompSemicolon =
    Parser.chompIf ((==) ';')


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore fn =
    Parser.chompIf fn
        |. Parser.chompWhile fn



-- Types


hexadecimal : Parser Int
hexadecimal =
    chompOneOrMore Char.isHexDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\hex ->
                case Hex.fromString (String.toLower hex) of
                    Ok value ->
                        Parser.succeed value

                    Err error ->
                        Parser.problem error
            )



-- Loops


many : Parser a -> Parser (List a)
many parser_ =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ parser_ |> Parser.map (\new -> Parser.Loop (new :: list))
                , Parser.succeed (Parser.Done (List.reverse list))
                ]
        )


oneOrMore : String -> Parser a -> Parser (List a)
oneOrMore type_ parser_ =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ parser_ |> Parser.map (\new -> Parser.Loop (new :: list))
                , if List.isEmpty list then
                    Parser.problem ("expecting at least one " ++ type_)

                  else
                    Parser.succeed (Parser.Done (List.reverse list))
                ]
        )
