{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Union where
import Data.Kind (Type)

data Union (r :: [Type -> Type]) v where
  UNow :: t v -> Union (t ': r) v

class Member t r where
  inj :: t v -> Union r v
  prj :: Union r v -> Maybe (t v)

data FEFree r a where
  Pure :: a -> FEFree r a
  Impure :: Union r x -> (x -> FEFree r a) -> FEFree r a

data Reader i x where
  Get :: Reader i i

data Writer o x where
  Put :: o -> Writer o ()

