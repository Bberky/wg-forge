module WgForge.Error (SpecError (..)) where

-- | Errors produced while loading a network spec.
data SpecError
  = -- | File could not be read (missing, permissions, ...).
    SpecIoError String
  | -- | Input is not well-formed YAML.
    YamlSyntaxError String
  | -- | YAML is well-formed but does not match the spec schema.
    SpecParseError String
  deriving (Eq, Show)
