{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, KindSignatures,
            FunctionalDependencies, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  helium@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Monad.Select 
   ( module Top.Monad.Select
   , module Control.Monad.State
   ) where

import Top.Util.Embedding
import Control.Monad.State
import Data.Kind (Type)

--------------------------------------------------------
-- Select Monad

newtype Select t m a = Select (m a)

instance (Functor m) => Functor (Select t m) where
   fmap f (Select ma) = Select (fmap f ma)

instance (Applicative m) => Applicative (Select t m) where
   pure a = Select (pure a)

   (Select mf) <*> (Select ma) = Select (mf <*> ma)

instance (Monad m) => Monad (Select t m) where
   return = pure

   (Select ma) >>= f = Select (ma >>= (\a -> let (Select mb) = f a in mb))

instance (MonadState s m, Embedded label s t) => MonadState t (Select t m) where
   get   = Select (gets   (getE embedding  ))
   put i = Select (modify (setE embedding i))

instance MonadTrans (Select t) where
   lift = select
   
select :: m a -> Select t m a
select = Select

--------------------------------------------------------
-- SelectFix Monad

newtype SelectFix (t :: (Type -> Type) -> Type) (m :: Type -> Type) a = SelectFix (m a)

instance Functor m => Functor (SelectFix t m) where
   fmap f (SelectFix ma) = SelectFix (fmap f ma)

instance Applicative m => Applicative (SelectFix t m) where
   pure a = SelectFix (pure a)
  
   (SelectFix mf) <*> (SelectFix ma) = SelectFix (mf <*> ma)

instance Monad m => Monad (SelectFix t m) where
   return = pure

   (SelectFix ma) >>= f = SelectFix (ma >>= \a -> let (SelectFix mb) = f a in mb)
                            
instance (MonadState s m, Embedded label s (t m)) => MonadState (t m) (SelectFix t m) where
   get   = SelectFix (gets   (getE embedding  ))
   put i = SelectFix (modify (setE embedding i))

instance MonadTrans (SelectFix t) where
   lift = selectFix

selectFix :: m a -> SelectFix t m a
selectFix = SelectFix

--------------------------------------------------------
-- Class Embedded

class Embedded label s t | label s -> t, t -> label where
   embedding :: Embedding s t

instance Embedded c s2 t => Embedded c (s1, s2) t where
   embedding = composeE sndE embedding
   
--------------------------------------------------------
-- deselect functions for Select Monad

deselect :: Select t m a -> m a  
deselect (Select m) = m

deselectFor :: (Embedded label s t, MonadState s m) => label -> Select t m a -> m a
deselectFor  _ = deselect

--------------------------------------------------------
-- deselect functions for SelectFix Monad

deselectFix :: SelectFix t m a -> m a  
deselectFix (SelectFix m) = m

deselectFixFor :: (Embedded label s (t m), MonadState s m) => label -> SelectFix t m a -> m a
deselectFixFor _ = deselectFix 