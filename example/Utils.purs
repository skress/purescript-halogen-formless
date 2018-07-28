module Example.Utils where

import Prelude

import Data.Array (singleton)
import Data.Either (Either, either, fromRight)
import Data.Foldable (length) as Foldable
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (contains, length, null)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Partial.Unsafe (unsafePartial)
import Polyform.Validation (Validation(..))
import Polyform.Validation as Validation

--------------------
-- Helper types
--------------------

-- | A type synonym for purescript-validation semigroup errors
type Errs = Array FieldError

data FieldError
  = EmptyField
  | InvalidEmail String
  | TooShort Int Int
  | TooLong Int Int
  | InvalidInt String
  | NotEqual String String

derive instance genericFieldError :: Generic FieldError _
instance showFieldError :: Show FieldError where
  show = genericShow

-- | Some useful types we'll parse to
newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive newtype instance showName :: Show Name

newtype Email = Email String
derive instance newtypeEmail :: Newtype Email _
derive newtype instance showEmail :: Show Email

-- | Unpacks errors to render as a string
showError
  :: ∀ e o
   . Show e
  => Maybe (Either e o)
  -> Maybe String
showError = (=<<) (either (Just <<< show) (const Nothing))

--------------------
-- Polyform Validation
--------------------

notRequired
  :: ∀ m a t0
   . Monad m
  => Monoid t0
  => Validation m t0 a a
notRequired = Validation.hoistFnV pure

emailFormat
  :: ∀ m
   . Monad m
  => Validation m Errs String Email
emailFormat = Validation.hoistFnV \e ->
  if contains (Pattern "@") e
    then pure (Email e)
    else Validation.Invalid $ singleton $ InvalidEmail e

emailIsUsed
  :: ∀ m
   . MonadEffect m
  => Validation m Errs Email Email
emailIsUsed = Validation \e -> do
  v <- liftEffect random
  pure $ if v > 0.5
    then Validation.Invalid $ singleton $ InvalidEmail (unwrap e)
    else pure e

minLength
  :: ∀ m
   . Monad m
  => Int
  -> Validation m Errs String String
minLength n = Validation.hoistFnV \p ->
  let p' = length p
  in if p' < n
       then Validation.Invalid $ singleton $ TooShort p' n
       else pure p

-- | The opposite of minLength.
maxLength
  :: ∀ m
   . Monad m
  => Int
  -> Validation m Errs String String
maxLength n = Validation.hoistFnV \p ->
  let p' = length p
   in if length p > n
        then Validation.Invalid $ singleton $ TooLong p' n
        else pure p

--------------------
-- Semigroup Validation
--------------------

validateMaybe :: ∀ a. Maybe a -> V Errs a
validateMaybe Nothing = invalid (singleton EmptyField)
validateMaybe (Just a) = pure a

validateEqual :: String -> String -> V Errs String
validateEqual a b
  | a == b = pure b
  | otherwise = invalid (singleton $ NotEqual a b)

validateInt :: String -> V Errs Int
validateInt str = case Int.fromString str of
  Nothing -> invalid (singleton $ InvalidInt str)
  Just v -> pure v

validateNonEmptyArray
  :: Array String
  -> V Errs (Array String)
validateNonEmptyArray input
  | Foldable.length input >= 1 = pure input
  | otherwise = invalid (singleton EmptyField)

-- | Validate that an input string is not empty
validateNonEmpty :: String -> V Errs String
validateNonEmpty input
  | null input = invalid (singleton EmptyField)
  | otherwise = pure input

-- | Validates that an input string conforms to some regular expression that
-- | checks for valid email addresses
validateEmailRegex :: String -> V Errs String
validateEmailRegex input
  | test emailRegex input = pure input
  | otherwise = invalid (singleton (InvalidEmail input))

-- | Validate that an input string is at least as long as some given `Int`
validateMinimumLength
  :: String
  -> Int
  -> V Errs String
validateMinimumLength input n
  | (length input) < n = invalid (singleton (TooShort (length input) n))
  | otherwise = pure input

-- | Validate that an input string is shorter than given `Int`
validateMaximumLength
  :: String
  -> Int
  -> V Errs String
validateMaximumLength input n
  | (length input) > n = invalid (singleton (TooLong (length input) n))
  | otherwise = pure input

unsafeRegexFromString :: String -> Regex
unsafeRegexFromString str = unsafePartial (fromRight (regex str noFlags))

emailRegex :: Regex
emailRegex =
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"
