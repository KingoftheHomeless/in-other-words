{-# OPTIONS_HADDOCK not-home #-}

-- This file is copied almost verbatim from polysemy.

module Control.Effect.Internal.TH.Effect
  ( makeEff,
    makeEff_,
  )
where

import Control.Effect.Internal.TH.Common
  ( ConLiftInfo (..),
    checkExtensions,
    foldArrowTs,
    getEffectMetadata,
    makeMemberConstraint,
    makeUnambiguousSend,
  )
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- TODO: write tests for what should (not) compile

------------------------------------------------------------------------------

-- | If @T@ is a GADT representing an effect algebra, @$('makeEff' ''T)@ automatically
-- generates a smart constructor for every data constructor of @T@. This also
-- works for data family instances. Names of smart constructors are created by
-- changing first letter to lowercase or removing prefix @:@ in case of
-- operators. Fixity declaration is preserved for both normal names and
-- operators.
--
-- @since 0.2.2.0
makeEff :: Name -> Q [Dec]
makeEff = genEff True

------------------------------------------------------------------------------

-- | Like 'makeEff', but does not provide type signatures and fixities. This
-- can be used to attach Haddock comments to individual arguments for each
-- generated function.
--
-- @
-- data Output o m a where
--   Output :: o -> Output o m ()
--
-- makeEff_ ''Output
--
-- -- | Output the value \@o\@.
-- output :: forall o m
--        .  Eff (Output o) m
--        => o         -- ^ Value to output.
--        -> m ()  -- ^ No result.
-- @
--
-- Because of limitations in Template Haskell, signatures have to follow some
-- rules to work properly:
--
-- * 'makeEff_' must be used /before/ the explicit type signatures
-- * all arguments in effect's type constructor have to follow naming scheme
-- from data constructor's declaration:
--
-- @
-- data Foo e m a where
--   FooC1 :: Foo x m ()
--   FooC2 :: Foo (Maybe x) m ()
-- @
--
-- should have @x@ in type signature of @fooC1@:
--
-- @fooC1 :: forall x m. Eff (Foo x) m => m ()@
--
-- and @Maybe x@ in signature of @fooC2@:
--
-- @fooC2 :: forall x m. Eff (Foo (Maybe x)) m => m ()@
--
-- * all effect's type variables and @m@ have to be explicitly quantified
-- using @forall@ (order is not important)
--
-- These restrictions may be removed in the future, depending on changes to
-- the compiler.
--
-- @since 0.2.2.0
makeEff_ :: Name -> Q [Dec]
makeEff_ = genEff False

-- NOTE(makeEff_):
-- This function uses an ugly hack to work --- it changes names in data
-- constructor's type to capturable ones. This allows users to provide them to
-- us from their signature through 'forall' with 'ScopedTypeVariables'
-- enabled, so that we can compile liftings of constructors with ambiguous
-- type arguments (see issue #48 of polysemy).
--
-- Please, change this as soon as GHC provides some way of inspecting
-- signatures, replacing code or generating haddock documentation in TH.

------------------------------------------------------------------------------

-- | Generates declarations and possibly signatures for functions to lift GADT
-- constructors into the effect monad.
genEff :: Bool -> Name -> Q [Dec]
genEff should_mk_sigs type_name = do
  checkExtensions [ScopedTypeVariables, FlexibleContexts, DataKinds]
  cl_infos <- getEffectMetadata type_name
  decs <- traverse (genDec should_mk_sigs) cl_infos

  let sigs = if should_mk_sigs then genSig <$> cl_infos else []
  pure $ join $ sigs ++ decs

------------------------------------------------------------------------------

-- | Generates signature for lifting function and type arguments to apply in
-- its body on effect's data constructor.
genSig :: ConLiftInfo -> [Dec]
genSig cli =
  maybe [] (pure . flip InfixD (cliFunName cli)) (cliFunFixity cli)
    ++ [ SigD (cliFunName cli) $
           quantifyType $
             ForallT [] (member_cxt : cliFunCxt cli) $
               foldArrowTs sem $
                 snd <$> cliFunArgs cli
       ]
  where
    member_cxt = makeMemberConstraint (cliMonadName cli) cli
    sem = VarT (cliMonadName cli) `AppT` cliEffRes cli

------------------------------------------------------------------------------

-- | Builds a function definition of the form
-- @x a b c = send (X a b c :: E m a)@.
genDec :: Bool -> ConLiftInfo -> Q [Dec]
genDec should_mk_sigs cli = do
  let fun_args_names = fst <$> cliFunArgs cli

  pure
    [ PragmaD $ InlineP (cliFunName cli) Inlinable ConLike AllPhases,
      FunD
        (cliFunName cli)
        [ Clause
            (VarP <$> fun_args_names)
            (NormalB $ makeUnambiguousSend should_mk_sigs cli)
            []
        ]
    ]
