# in-other-words
[![build GHC 8.6](https://github.com/KingoftheHomeless/in-other-words/workflows/build%20GHC%208.6/badge.svg)](https://github.com/KingoftheHomeless/in-other-words/actions?query=workflow%3A%22build+GHC+8.6%22)
[![build GHC 8.8](https://github.com/KingoftheHomeless/in-other-words/workflows/build%20GHC%208.8/badge.svg)](https://github.com/KingoftheHomeless/in-other-words/actions?query=workflow%3A%22build+GHC+8.8%22)
[![build GHC 8.10](https://github.com/KingoftheHomeless/in-other-words/workflows/build%20GHC%208.10/badge.svg)](https://github.com/KingoftheHomeless/in-other-words/actions?query=workflow%3A%22build+GHC+8.10%22)

- [Overview][]
- [Features][]
- [Required Language Extensions][]
- [Examples of Simple Usage][]
- [Advanced Usage][]
- [Troubleshooting][]
- [Performance][]

[Overview]: https://github.com/KingoftheHomeless/in-other-words#overview
[Features]: https://github.com/KingoftheHomeless/in-other-words#features
[Required Language Extensions]: https://github.com/KingoftheHomeless/in-other-words#required-language-extensions
[Examples of Simple Usage]: https://github.com/KingoftheHomeless/in-other-words#examples-of-simple-usage
[Advanced Usage]: https://github.com/KingoftheHomeless/in-other-words#advanced-usage
[Troubleshooting]: https://github.com/KingoftheHomeless/in-other-words#troubleshooting
[Performance]: https://github.com/KingoftheHomeless/in-other-words#performance

## Overview
`in-other-words` is an effect system in the vein of [`freer-simple`](https://github.com/lexi-lambda/freer-simple),
[`fused-effects`](https://github.com/fused-effects/fused-effects),
[`polysemy`](https://github.com/polysemy-research/polysemy),
and [`eff`](https://github.com/hasura/eff). It represents effects through data types,
making it simple to define, use, and interpret them.

The goal of `in-other-words` is to be as expressive and general of an
effect system as possible while solving the *O(n<sup>2</sup>)* instances
problem. Its hallmark feature is the novel approach it takes to support
higher-order effects, making it significantly more powerful -- and sometimes
easier to use -- than other effect libraries of its kind.

If you're experienced with the mechanisms behind `freer-simple`,
`fused-effects`, and `polysemy`, and would like to learn more about what makes
`in-other-words` different, see
[this wiki page](https://github.com/KingoftheHomeless/in-other-words/wiki/The-inner-workings-of-the-library).

Unfortunately, in its current state `in-other-words` is rather inaccessible.
Ample documentation and guides are provided for the library, but inexperienced
users are still likely to run into gotchas and confusing error
messages. As such, if you're a beginner to effect systems,
`freer-simple` or `polysemy` would serve as better starting points.

## Features

### Simple higher-order effects
Unlike `fused-effects` and `polysemy` -- which both have intimidating
boilerplate associated with the interpretation of higher-order effects
--`in-other-words` makes it just as easy to interpret higher-order effects as
first-order effects. Go [here](#higher-order) for an example.

### No cumbersome restrictions to effects
Every effect-system previously mentioned has serious restrictions in what
effects they may represent.
* `freer-simple` is restricted to first-order effects.
* `fused-effects` and `polysemy` are built around
[*Effect Handlers in Scope*](https://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf),
whose approach doesn't allow for sensible implementations of effects for
continuations, coroutines, or nondeterminism.
* `eff` is limited to what's implementable with delimited continuations, which
excludes actions such as
[`pass`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Writer-Class.html#v:pass)
from `MonadWriter`, and `async`/`await` style concurrency.

`in-other-words` also places restrictions on what effects may be represented
-- but in comparison to the libraries mentioned above, these restrictions are
very minor. <sup id="a1">[1](#f1)</sup> This is because `in-other-words`
does not attempt to make every possible effect play nicely together with
every other effect: instead, just like `mtl`, some effects can't be used
together with other effects (depending on how they're interpreted), and
this is enforced by constraints that interpreters may introduce.

## Required Language Extensions
The following extensions are needed for basic usage of the library:

```
  - ConstraintKinds
  - DataKinds
  - FlexibleContexts
  - GADTs
  - LambdaCase
  - PolyKinds
  - RankNTypes
  - TypeApplications
  - TypeOperators
  - TypeFamilies
```

Some features of the library could require enabling more extensions.


## Examples of Simple Usage

First-order usage:
```haskell

import Control.Effect
import Control.Effect.Error
import Control.Effect.State
import Control.Effect.Reader
import Control.Effect.Writer

import Text.Read (readMaybe)

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

readTTY :: Eff Teletype m => m String
readTTY = send ReadTTY

writeTTY :: Eff Teletype m => String -> m ()
writeTTY str = send (WriteTTY str)

teletypeToIO :: Eff (Embed IO) m => SimpleInterpreterFor Teletype m
teletypeToIO = interpretSimple $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

challenge :: Eff Teletype m => m ()
challenge = do
  writeTTY "What is 3 + 4?"
  readTTY >>= \str -> case readMaybe @Int str of
    Just 7  -> writeTTY "Correct!"
    _       -> writeTTY "Nope." >> challenge

-- Interpret a `Teletype` effect in terms of `Ask` and `Tell` effects
runTeletype :: Effs '[Ask String, Tell String] m
            => SimpleInterpreterFor Teletype m
runTeletype = interpretSimple $ \case
  ReadTTY -> ask
  WriteTTY msg -> tell msg

-- Runs a challenge with the provided inputs purely.
runChallengePure :: [String] -> Either String [String]
runChallengePure testInputs =
    -- Extract the final result, now that all effects have been interpreted.
    run
    -- Run the @Throw String@ effect, resulting in @Either String [String]@
  $ runThrow @String
    -- We discard the output of @challenge@; we're not interested in it.
  $ fmap fst
    -- Run the @Tell String@ effect by gathering all told
    -- strings into a list, resulting in ([String], ())
  $ runTellList @String
    -- Run the @State [String]@ effect with initial state
    -- @inputs@. @evalState@ discards the end state.
  $ evalState testInputs
    -- interpret the @Ask String@ effect by going through the provided inputs.
    -- Throw an exception if we go through all the inputs without completing the
    -- challenge.
  $ runAskActionSimple (do
      get >>= \case
        []     -> throw "Inputs exhausted!"
        (x:xs) -> put xs >> return x
    )
    -- Interpret @Teletype@ in terms of @Ask String@ and @Tell String@
  $ runTeletype
  $ challenge

-- evaluates to True
testChallenge :: Bool
testChallenge =
    runChallengePure ["4","-7", "i dunno", "7"]
 == Right ["What is 3 + 4?", "Nope."
          ,"What is 3 + 4?", "Nope."
          ,"What is 3 + 4?", "Nope."
          ,"What is 3 + 4?", "Correct!"
          ]

-- Make a challenge to the user
challengeIO :: IO ()
challengeIO = runM $ teletypeToIO $ challenge
```

<span id="higher-order">Higher-order usage:</span>
```haskell

import Control.Effect
import Control.Effect.Bracket
import Control.Effect.Trace
import GHC.Clock (getMonotonicTime)

data ProfileTiming m a where
  ProfileTiming :: String -> m a -> ProfileTiming m a

time :: Eff ProfileTiming m => String -> m a -> m a
time label m = send (ProfileTiming label m)

profileTimingToIO :: Effs '[Embed IO, Trace, Bracket] m
                  => SimpleInterpreterFor ProfileTiming m
profileTimingToIO = interpretSimple $ \case
  ProfileTiming str action -> do
    before <- embed getMonotonicTime
    a <-   action
        `onError`
           trace ("Timing of " ++ str ++ " failed due to some error!")
    after <- embed getMonotonicTime
    trace ("Timing of " ++ str ++ ": " ++ show (after - before) ++ " seconds.")
    return a

spin :: Monad m => Integer -> m ()
spin 0 = pure ()
spin i = spin (i - 1)

profileSpin :: IO ()
profileSpin = runM $ bracketToIO $ runTracePrinting $ profileTimingToIO $ do
  time "spin" (spin 1000000)
  time "spinAndFail" (spin 1000000 >> undefined)
{-
This prints the following (exact times are machine specific):

  Timing of spin: 1.3399935999768786 seconds.
  Timing of spinAndFail failed due to some error!
  *** Exception: Prelude.undefined
-]
```

## Advanced Usage

The examples above are somewhat disingeneuos; they cover only the simplest
uses of the library. The library has a wide variety of features,
and using them properly can get very complicated. Because of this,
[`in-other-words` offers a wiki covering more advanced topics of the
library.](https://github.com/KingoftheHomeless/in-other-words/wiki/Advanced-topics)
Check it out if you're interested in learning more about the
library,  or are struggling with a feature or error message you're having a
time with.


## Troubleshooting
[The wiki has a page for common error messages.](https://github.com/KingoftheHomeless/in-other-words/wiki/Common-Error-Messages-and-Issues)
If you run into any issues or strange error messages that you can't figure out
from the wiki, feel free to make an issue about it. If not already covered, and
if I can generalize the problem enough, then I'll expand the wiki to cover the
issue.

## Performance
`in-other-words` was not designed with performance as a main goal; however, unlike
the free-monad based solutions of `freer-simple` and `polysemy`, the underlying
mechanisms of `in-other-words` are class-based, just like `fused-effects`. This
suggests that if optimized, `in-other-words` could compete with `mtl`.
Benchmarking, profiling, and optimizations are currently considered future goals
of the library.

[As noted by Alexis King](https://github.com/ghc-proposals/ghc-proposals/pull/313#issuecomment-590143835),
the incredible performance of `mtl` is more or less a myth that is only achievable
under unrealistic conditions. Even if optimized, `in-other-words` would fall prey
to the same problems.

Thus, a goal in the far future is to investigate if it's possible to take
inspiration from `in-other-words`' approach in order to develop an effect system
with similar power, but relying on [delimited continuation primops](https://github.com/lexi-lambda/ghc-proposals/blob/delimited-continuation-primops/proposals/0000-delimited-continuation-primops.md)
together with other IO operations for performance, with the eventual goal to
support more effects than `eff` (currently) does.

***
<b id="f1">[1](#a1)</b> Every effect is required to be *representational* in the carrier monad.
This means that if you can represent your effect using:
* a `mtl`-style effect class
* without any associated type families
* and it can be newtype derived

then you can represent your effect with `in-other-words`.
