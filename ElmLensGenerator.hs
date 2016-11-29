module ElmLensGenerator (createLens, createModuleHeader, parseRecord) where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative

data RecordType
    = Lens String
    | Optional String

instance Show RecordType where
    show (Lens recordType') =
        recordType'
    show (Optional recordType') =
        recordType'

data Record = Record
    { fieldName :: String
    , fieldType :: RecordType
    }
    deriving (Show)

{-| Matches any sort of whitespace characters
-}
whitespace :: Parser String
whitespace =
    many (char ' ')

{-| Combinator that matches the given parser plus any amount of whitespace
-}
lexeme :: Parser a -> Parser a
lexeme p =
    p <* whitespace

{-| Unwords a list of strings, but wrap it in parentheses if there is more than one
    word in the list to unword
-}
unwordsIntoParens :: [String] -> String
unwordsIntoParens strs
    | length strs == 1 = unwords strs
    | otherwise        = "(" ++ unwords strs ++ ")"

{-| Matches a word consisting of any number of letters
-}
recordFieldName :: Parser String
recordFieldName =
    lexeme $ many letter

{-| Matches the string literaly "Maybe"
-}
maybeToken :: Parser String
maybeToken =
    lexeme $ string "Maybe"

{-| Matches many type tokens wrapped in parentheses, and unwords them together
    into a string wrapped in parentheses
-}
wrappedTypeToken :: Parser String
wrappedTypeToken = do
    _ <- lexeme (char '(')
    parsedTokens <- many typeToken
    _ <- lexeme (char ')')
    return $ "(" ++ unwords parsedTokens ++ ")"

{-| Matches a string of letters that begins with a capital letter
-}
unwrappedTypeToken :: Parser String
unwrappedTypeToken =
    lexeme $ do
        firstLetter <- upper
        rest <- many letter
        return $ firstLetter : rest

{-| Matches a single type token or wrapped type token
-}
typeToken :: Parser String
typeToken =
    unwrappedTypeToken <|> wrappedTypeToken

{-| Matches a list of type tokens that begins with the "Maybe" string literal, and
    converts them into a RecordType
-}
maybeType :: Parser RecordType
maybeType =
    Optional . unwordsIntoParens <$> (maybeToken *> many typeToken)

{-| Matches a list of type tokens and converts them into a RecordType
-}
normalType :: Parser RecordType
normalType =
    Lens . unwordsIntoParens <$> many typeToken

{-| Tries to match a maybeType token, but if it can't then tries to match a normalType token
-}
recordType :: Parser RecordType
recordType =
    try maybeType <|> normalType

{-| Matches a recordFieldName token and a recordType token demarkated by a ':', and returns
    a Record type
-}
recordEntry :: Parser Record
recordEntry = do
    fieldName' <- recordFieldName
    _ <- lexeme $ char ':'
    fieldType' <- recordType
    return $ Record fieldName' fieldType'

{-| Matches the Elm record syntax with {} and returns a list of recordEntry tokens as an AST
-}
elmRecord :: Parser [Record]
elmRecord =
    lexeme (char '{') *> (recordEntry `sepBy` lexeme (char ',')) <* char '}'

{-| Morphism from RecordType to String for building the Elm type signatures
-}
signatureMonocleType :: RecordType -> String
signatureMonocleType (Lens _) =
    "Lens"
signatureMonocleType (Optional _) =
    "Optional"

{-| Builds up the full Elm type signature for a given RecordType
-}
createTypeSignature :: String -> RecordType -> String
createTypeSignature fieldName' type' =
    signatureMonocleType type'
        ++ " { "
        ++ fieldName'
        ++ " : "
        ++ show type'
        ++ " } "
        ++ show type'

{-| Builds up the full Elm lens function for a given Record
-}
createLens :: Record -> String
createLens record =
    let
        fieldName' = fieldName record
        fieldType' = fieldType record
    in
        fieldName'
            ++ " : "
            ++ createTypeSignature fieldName' fieldType'
            ++ "\n"
            ++ fieldName'
            ++ " =\n"
            ++ "\tlet\n"
            ++ "\t\tset value model =\n"
            ++ "\t\t\t{ model | "
            ++ fieldName'
            ++ " = value }\n"
            ++ "\tin\n\t\t"
            ++ signatureMonocleType fieldType'
            ++ " ."
            ++ fieldName'
            ++ " set\n"

createModuleHeader :: String -> String
createModuleHeader moduleName =
    "module "
        ++ moduleName
        ++ " exposing (..)\n\n"
        ++ "import Monocle.Optional exposing (Optional)\nimport Monocle.Lens exposing (Lens)\n"

{-| Parses a string into a list of Records
-}
parseRecord :: String -> Either ParseError [Record]
parseRecord =
    parse elmRecord ""
