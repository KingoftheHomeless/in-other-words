# Changelog for `in-other-words`

## 0.2.0.0 (2021-01-30)
### Breaking Changes
* `-Fast` interpretations of every continuation-related effect have been removed due
  to lack of usefulness.
* The `ThreadsEff (ContT r) (ReaderPrim i)` instance has been removed due to unlawfulness.
* `Control.Monad.Trans.List.Church` and `Control.Monad.Trans.Free.Church.Alternate` have
    received a minor revamp. Notably, the representations of `ListT` and `FreeT` have been
    changed to become lawful monad transformers.
* `bracketToIO` now executes the cleanup action of any `generalBracket`
    _uninterruptibly masked_.

### Non-breaking Changes
* Fixed a bug where `listen` when using `listenToIO` would be lifted incorrectly by carriers
    based on `FreeT` and `ListT`, which arose due to these not having been lawful
    monad transformers.
* All uses of `CompositionC` in the library has been changed to proper newtypes.
    This should improve the quality of error messages as well as compilation times.
* Added `bracketToIOUnsafe`, which has the previous semantics of `bracketToIO`
    -- that is, the cleanup action of each `generalBracket` is only executed
    _interruptibly masked_.
* `Control.Efffect.Newtype` now exports the constructors of `WrapperOf`, thus
     addressing an issue where users wouldn't be allowed to derive via `WrapperOf`.
* `Control.Efffect.Carrier` now exports the constructors of `IdentityT`, thus
     addressing an issue where users wouldn't be allowed to derive via `IdentityT`.
* Fixed an issue where `FailC` lacked a `MonadFail` instance.
* Added `errorToIOAsExc` and `errorToErrorIOAsExc` (thanks @poscat0x04!)


## 0.1.1.0 (2020-10-30)
### Non-breaking Changes
* Added `runTellAction` and `ignoreTell` interpreters.
* Added `runEmbed` interpreter
* Fixed an issue with `runShift` where HO-actions applied on a `shift` could affect the continuation provided to the argument of `shift`.


## 0.1.0.0 (2020-10-10)
Initial release.
