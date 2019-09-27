{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Helper functions to write @swagger@ instances easier.
-}

module CakeSlayer.Swagger
       ( schemaRef
       , namedSchema
       , declareSingleFieldSchema
       ) where

import Data.Swagger (Definitions, HasType (type_), NamedSchema (NamedSchema), Referenced, Schema,
                     SwaggerType (SwaggerObject), ToSchema, declareSchemaRef, properties, required)
import Data.Swagger.Declare (Declare)
import Relude.Extra.Lens ((.~))
import Relude.Extra.Type (typeName)

{- | Shorter version of 'declareSchemaRef'. So instead of

@
declareSchemaRef (Proxy @MyType)
@

you can write

@
schemaRef @MyType
@
-}
schemaRef
    :: forall t . ToSchema t
    => Declare (Definitions Schema) (Referenced Schema)
schemaRef = declareSchemaRef (Proxy @t)

{- | Helper function to return named schemas. So instead of:

@
pure $ NamedSchema (Just "LoginResponse") $ mempty
    & type_ .~ SwaggerObject
    & properties .~ fromList
        [ ("jwtToken", jwtTokenSchema)
        ]
    & required .~ ["jwtToken"]
@

you will use it like:

@
'namedSchema' @LoginResponse $ \s -> s
    & properties .~ fromList
        [("jwtToken", jwtTokenSchema)]
    & required .~ ["jwtToken"]
@
-}
namedSchema
    :: forall t f .
       (Typeable t, Applicative f)
    => (Schema -> Schema)
    -> f NamedSchema
namedSchema updateSchema = pure $ NamedSchema
    (Just $ typeName @t)
    (updateSchema $ mempty & type_ .~ Just SwaggerObject)

{- | Helper function to declare 'ToSchema' instances for data types that contain
only single field.
-}
declareSingleFieldSchema
    :: forall field a proxy .
       (ToSchema field, Typeable a)
    => Text
    -> proxy a
    -> Declare (Definitions Schema) NamedSchema
declareSingleFieldSchema fieldName _ = do
    fieldsSchema <- schemaRef @field
    namedSchema @a $ \s -> s
        & properties .~ fromList
            [(fieldName, fieldsSchema)]
        & required .~ [fieldName]
