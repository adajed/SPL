{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParSPL where
import AbsSPL
import LexSPL
import ErrM
import Operator
import Token
import Type
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: ((Pos, Integer)) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> ((Pos, Integer))
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: ((Pos, String)) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> ((Pos, String))
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: ((Pos, Char)) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> ((Pos, Char))
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: ((Pos, CIdent)) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> ((Pos, CIdent))
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ((Pos, VIdent)) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ((Pos, VIdent))
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: ((Pos, Program Pos)) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> ((Pos, Program Pos))
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ((Pos, TopDef Pos)) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ((Pos, TopDef Pos))
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ((Pos, [TopDef Pos])) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ((Pos, [TopDef Pos]))
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ((Pos, Argument Pos)) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ((Pos, Argument Pos))
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ((Pos, [Argument Pos])) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ((Pos, [Argument Pos]))
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ((Pos, ClassElem Pos)) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ((Pos, ClassElem Pos))
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ((Pos, [VIdent])) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ((Pos, [VIdent]))
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: ((Pos, [ClassElem Pos])) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> ((Pos, [ClassElem Pos]))
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: ((Pos, Block Pos)) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> ((Pos, Block Pos))
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: ((Pos, [Stmt Pos])) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> ((Pos, [Stmt Pos]))
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ((Pos, Stmt Pos)) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ((Pos, Stmt Pos))
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: ((Pos, Item Pos)) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> ((Pos, Item Pos))
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ((Pos, [Item Pos])) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ((Pos, [Item Pos]))
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: ((Pos, Type Pos)) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> ((Pos, Type Pos))
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ((Pos, [Type Pos])) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ((Pos, [Type Pos]))
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: ((Pos, Expr Pos)) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> ((Pos, Expr Pos))
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ((Pos, [Expr Pos])) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ((Pos, [Expr Pos]))
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ((Pos, Stmt Pos)) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ((Pos, Stmt Pos))
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x80\x11\x02\x00\x87\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x21\x00\x70\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x80\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x02\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x21\x00\x60\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x21\x00\x60\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x80\x10\x02\x00\x86\x00\x00\x00\x00\x00\x04\x20\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x21\x04\x64\x08\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x08\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x02\x00\x86\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x02\x00\x86\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x08\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x21\x04\x64\x08\x00\x00\x00\x00\x00\x00\x00\x80\x10\x02\x00\x86\x00\x00\x00\x00\x00\x84\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x02\x94\xfc\xcf\x01\xc6\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x10\x02\x00\x86\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x08\x00\x00\x00\x00\x01\x00\x00\x00\x00\x04\x08\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x11\x90\x20\x04\x00\x01\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x02\x40\x1d\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x00\x00\x00\x80\x10\x02\x00\x86\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x21\x00\x60\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x21\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x00\x40\x14\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x00\x44\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x01\x09\x42\x00\x10\x00\x00\x00\x00\x00\x00\x80\x11\x90\x20\x04\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x14\x6c\x89\x00\x40\x07\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x08\x00\x00\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x20\x04\x21\x40\xc9\xff\x18\x60\x7c\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x02\x94\xfc\x8f\x01\xc6\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x42\x10\x02\x94\xfc\x8f\x01\xc6\x07\x00\x00\x00\x20\x04\x21\x40\xc9\xff\x18\x60\x7c\x00\x00\x00\x00\x42\x10\x00\x14\x6c\x89\x00\x40\x07\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x42\x10\x02\x94\xfc\x8f\x01\xc6\x07\x00\x00\x00\x20\x04\x01\x40\x41\x16\x08\x00\x74\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x20\x04\x01\x40\xc1\x96\x08\x00\x74\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x21\x40\xc9\xff\x18\x60\x7c\x00\x00\x00\x00\x42\x10\x00\x14\x64\x81\x00\x40\x07\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x04\x21\x40\xc9\xff\x18\x60\x7c\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x10\x02\x94\xfc\x8f\x01\xc6\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Integer","String","Char","CIdent","VIdent","Program","TopDef","ListTopDef","Argument","ListArgument","ClassElem","ListVIdent","ListClassElem","Block","ListStmt","Stmt","Item","ListItem","Type","ListType","Expr7","Expr6","Expr5","Expr4","Expr3","Expr2","Expr1","Expr","ListExpr","LambdaExpr","'!'","'!='","'%'","'&'","'&&'","'('","')'","'*'","'+'","'++'","','","'-'","'--'","'->'","'.'","'/'","';'","'<'","'<<'","'<='","'='","'=='","'>'","'>='","'>>'","'['","'[]'","'\\\\'","']'","'^'","'bool'","'class'","'else'","'false'","'if'","'int'","'new'","'null'","'return'","'true'","'void'","'while'","'{'","'|'","'||'","'}'","'~'","'for'","'to'","'down'","'in'","'by'","'extends'","'constr'","'then'","'typedef'","'char'","'string'","'real'","'or'","'and'","L_integ","L_CIdent","L_VIdent","L_String","L_Char","%eof"]
        bit_start = st * 100
        bit_end = (st + 1) * 100
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..99]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\xa6\x00\xca\xff\x00\x00\x00\x00\xd5\xff\xa6\x00\x00\x00\x04\x00\x00\x00\xdd\xff\x00\x00\x00\x00\xdd\xff\x00\x00\x00\x00\x00\x00\x0d\x00\xdb\xff\x2d\x00\x93\x00\x00\x00\x00\x00\x00\x00\x13\x00\x53\x00\x93\x00\x00\x00\x20\x00\x93\x00\x9b\x00\x45\x00\xbd\x00\x79\x00\x85\x00\x04\x00\x00\x00\x93\x00\x00\x00\x00\x00\x5e\x00\x93\x00\x00\x00\x04\x00\x00\x00\x89\x00\x00\x00\x00\x00\xc4\x00\x93\x00\x41\x00\x80\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x93\x00\x52\x00\x8d\x00\x00\x00\x72\x00\x95\x00\x00\x00\xa4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x58\x00\x00\x00\x7d\x00\x2b\x00\xff\xff\xd8\xff\x00\x00\x8d\x01\xae\x00\x99\x00\xae\x00\x00\x00\x99\x00\x93\x00\x00\x00\xa8\x00\x93\x00\x00\x00\x82\x00\x00\x00\xaa\x00\x00\x00\xae\x00\xb2\x00\x00\x00\x00\x00\x8b\x00\x00\x00\x99\x00\xac\x00\x00\x00\xb3\x00\x2f\x00\x99\x00\xbb\x00\xc6\x00\xb6\x00\x00\x00\xce\x00\x00\x00\xc7\x00\xd1\x00\x00\x00\x99\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\xae\x00\x99\x00\xa7\x00\x99\x00\xcf\x00\xde\x00\xd9\x00\xc9\x00\x00\x00\x00\x00\x00\x00\xb1\x00\x99\x00\xd8\x00\x00\x00\xf1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7d\x00\x7d\x00\x00\x00\x2b\x00\x2b\x00\x2b\x00\x2b\x00\x2b\x00\x00\x00\x2b\x00\x00\x00\x00\x00\xee\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\x00\x57\x00\x02\x01\x99\x00\x99\x00\x00\x00\x03\x01\xee\xff\x99\x00\x99\x00\x2c\x00\x04\x01\xef\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x99\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\x00\x19\x01\x00\x00\x00\x00\x00\x00\x0b\x01\x71\x00\x99\x00\xf0\x00\x2c\x00\x2c\x00\x57\x00\x2c\x01\x00\x00\x00\x00\x99\x00\x02\x00\x2c\x00\x99\x00\x05\x00\x57\x00\x00\x00\x2c\x00\x99\x00\x29\x01\x00\x00\x2c\x00\x38\x01\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x3a\x04\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x4b\x01\x00\x00\x69\x01\x00\x00\x00\x00\x81\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x29\x04\x50\x01\x83\x01\x6c\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x66\x01\x00\x00\x78\x00\x00\x00\x00\x00\x7a\x01\x3b\x04\x00\x00\xa2\x00\x00\x00\x00\x00\x7d\x01\x00\x00\x3b\x00\x3f\x04\x00\x00\x00\x00\x00\x00\x00\x00\x8a\x01\x76\x01\x00\x00\x46\x04\xc3\x00\x00\x00\x00\x00\x8c\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd1\x03\x67\x01\xd6\x03\x00\x00\x2d\x01\x4d\x04\x00\x00\x00\x00\x75\x00\x00\x00\x5e\x02\x00\x00\x00\x00\x00\x00\xec\x03\x00\x00\x00\x00\x00\x00\x97\x01\x00\x00\x7a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x82\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x02\x5a\x03\x61\x03\x60\x00\x7c\x03\x88\x03\xa0\x03\xa5\x03\xaa\x03\xaf\x03\x82\x03\xc7\x03\xcc\x03\xf1\x03\xf6\x03\xfb\x03\x11\x04\x16\x04\x1b\x04\x20\x04\x36\x04\x3c\x01\x98\x01\xa6\x02\x00\x00\x00\x00\x00\x00\x90\x01\x00\x00\x00\x00\x00\x00\xf7\x00\xc2\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x01\x00\x01\x00\x00\xca\x02\x59\x01\x00\x00\x00\x00\x00\x00\xe6\x02\xee\x02\x92\x01\x00\x00\x00\x00\xae\x01\x00\x00\x00\x00\x00\x00\x0a\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x12\x03\x00\x00\xca\x01\xe6\x01\x0f\x01\x00\x00\x00\x00\x00\x00\x2e\x03\x00\x00\x02\x02\x36\x03\x00\x00\x1e\x01\x00\x00\x1e\x02\x52\x03\x00\x00\x00\x00\x3a\x02\x00\x00\x00\x00\x56\x02\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\xfe\xff\xca\xff\x00\x00\xf4\xff\xf9\xff\x00\x00\xcc\xff\x00\x00\xcf\xff\xcb\xff\x00\x00\xce\xff\xcd\xff\xfb\xff\x00\x00\x00\x00\x00\x00\xc7\xff\xc9\xff\xfa\xff\xf3\xff\xc6\xff\x00\x00\xf1\xff\xe9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\x00\x00\x00\x00\xc8\xff\xc7\xff\xc5\xff\xf2\xff\x00\x00\xf1\xff\xe8\xff\x00\x00\xf6\xff\x00\x00\xe9\xff\xf7\xff\x00\x00\xf1\xff\xeb\xff\x00\x00\xef\xff\xf8\xff\xe6\xff\x00\x00\xee\xff\xf1\xff\x00\x00\x00\x00\xf5\xff\x00\x00\xeb\xff\xea\xff\x00\x00\xbf\xff\xc0\xff\xbe\xff\xc1\xff\xe3\xff\xe5\xff\x00\x00\xb6\xff\xad\xff\xaa\xff\xa3\xff\xa0\xff\x9d\xff\x98\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\x97\xff\xf1\xff\xc2\xff\x00\x00\x00\x00\xc4\xff\x00\x00\xc3\xff\x00\x00\xe7\xff\x00\x00\x00\x00\xfd\xff\xfc\xff\x00\x00\xb7\xff\x00\x00\x00\x00\xdd\xff\xca\xff\x00\x00\x00\x00\x00\x00\x96\xff\x00\x00\xb9\xff\x00\x00\xb8\xff\x00\x00\x00\x00\xd4\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x97\xff\x00\x00\x00\x00\xd3\xff\xd1\xff\x00\x00\x00\x00\xed\xff\xec\xff\xe2\xff\x00\x00\x00\x00\x00\x00\xbd\xff\x00\x00\xaf\xff\xae\xff\xb1\xff\xb2\xff\xb4\xff\xb5\xff\xb0\xff\xb3\xff\xab\xff\xac\xff\xa1\xff\xa6\xff\xa7\xff\xa5\xff\xa8\xff\xa9\xff\xa2\xff\xa4\xff\x9e\xff\x9f\xff\x00\x00\xdf\xff\xe0\xff\xba\xff\x99\xff\x97\xff\x00\x00\x00\x00\x00\x00\x97\xff\xde\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\xff\x94\xff\x9a\xff\x00\x00\x95\xff\xe1\xff\xbb\xff\xbc\xff\xd2\xff\xd0\xff\x00\x00\xdc\xff\x9b\xff\x9c\xff\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xff\xd5\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\xff\x00\x00\x00\x00\x00\x00\xd9\xff\x00\x00\x00\x00\xd7\xff\x00\x00\xd8\xff\xd6\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x02\x00\x01\x00\x15\x00\x05\x00\x2d\x00\x2b\x00\x06\x00\x3e\x00\x07\x00\x06\x00\x03\x00\x07\x00\x0c\x00\x06\x00\x07\x00\x35\x00\x12\x00\x11\x00\x14\x00\x3c\x00\x16\x00\x17\x00\x18\x00\x43\x00\x06\x00\x12\x00\x1a\x00\x3f\x00\x1c\x00\x0b\x00\x1b\x00\x1f\x00\x33\x00\x15\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x1b\x00\x2e\x00\x2f\x00\x30\x00\x06\x00\x06\x00\x09\x00\x06\x00\x34\x00\x0c\x00\x0c\x00\x34\x00\x39\x00\x3a\x00\x3d\x00\x11\x00\x03\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x40\x00\x0a\x00\x1a\x00\x06\x00\x1c\x00\x1a\x00\x1b\x00\x1f\x00\x0b\x00\x12\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x01\x00\x03\x00\x07\x00\x2f\x00\x30\x00\x06\x00\x06\x00\x3f\x00\x00\x00\x01\x00\x02\x00\x0c\x00\x04\x00\x39\x00\x3a\x00\x0f\x00\x12\x00\x13\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x03\x00\x2b\x00\x1a\x00\x1a\x00\x1c\x00\x14\x00\x15\x00\x16\x00\x17\x00\x03\x00\x22\x00\x23\x00\x03\x00\x25\x00\x26\x00\x12\x00\x28\x00\x03\x00\x04\x00\x2b\x00\x01\x00\x0b\x00\x08\x00\x2f\x00\x12\x00\x06\x00\x2b\x00\x12\x00\x13\x00\x07\x00\x10\x00\x0c\x00\x06\x00\x13\x00\x11\x00\x40\x00\x11\x00\x07\x00\x3e\x00\x19\x00\x40\x00\x41\x00\x42\x00\x01\x00\x1e\x00\x1a\x00\x2b\x00\x1c\x00\x06\x00\x0b\x00\x06\x00\x31\x00\x32\x00\x22\x00\x0c\x00\x04\x00\x25\x00\x26\x00\x2c\x00\x28\x00\x07\x00\x11\x00\x0b\x00\x06\x00\x01\x00\x06\x00\x2f\x00\x1f\x00\x1a\x00\x06\x00\x1c\x00\x1b\x00\x24\x00\x06\x00\x06\x00\x0c\x00\x22\x00\x29\x00\x11\x00\x25\x00\x26\x00\x3e\x00\x28\x00\x40\x00\x41\x00\x42\x00\x1f\x00\x20\x00\x04\x00\x2f\x00\x0e\x00\x24\x00\x40\x00\x39\x00\x3a\x00\x0b\x00\x29\x00\x22\x00\x0b\x00\x3f\x00\x1d\x00\x26\x00\x07\x00\x28\x00\x3e\x00\x11\x00\x40\x00\x41\x00\x42\x00\x1f\x00\x2f\x00\x38\x00\x39\x00\x3a\x00\x24\x00\x11\x00\x1f\x00\x15\x00\x3f\x00\x29\x00\x40\x00\x24\x00\x0b\x00\x11\x00\x2e\x00\x3e\x00\x29\x00\x40\x00\x41\x00\x42\x00\x40\x00\x2e\x00\x36\x00\x2b\x00\x1d\x00\x39\x00\x3a\x00\x07\x00\x04\x00\x36\x00\x04\x00\x3f\x00\x39\x00\x3a\x00\x11\x00\x00\x00\x01\x00\x02\x00\x3f\x00\x04\x00\x10\x00\x11\x00\x10\x00\x11\x00\x07\x00\x07\x00\x07\x00\x1d\x00\x0d\x00\x37\x00\x00\x00\x01\x00\x02\x00\x07\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x0d\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x31\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x0d\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x07\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x21\x00\x1d\x00\x00\x00\x01\x00\x02\x00\x07\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x21\x00\x04\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x0c\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x04\x00\x04\x00\x03\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x0d\x00\x03\x00\x0f\x00\x03\x00\x0d\x00\x12\x00\x0c\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0a\x00\x0e\x00\x0d\x00\x0d\x00\x04\x00\x04\x00\x0d\x00\x11\x00\x0d\x00\xff\xff\x0f\x00\x15\x00\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x0d\x00\xff\xff\x0f\x00\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x14\x00\x15\x00\x16\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\xff\xff\xff\xff\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x03\x00\xff\xff\xff\xff\x14\x00\x15\x00\x08\x00\x09\x00\xff\xff\x14\x00\x15\x00\x00\x00\x01\x00\x02\x00\xff\xff\x04\x00\x12\x00\xff\xff\x03\x00\x03\x00\x05\x00\x06\x00\x07\x00\x03\x00\x08\x00\x09\x00\xff\xff\xff\xff\x08\x00\x09\x00\x03\x00\x14\x00\x15\x00\x12\x00\x12\x00\x08\x00\x09\x00\x03\x00\x12\x00\xff\xff\xff\xff\xff\xff\x08\x00\x09\x00\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x76\x00\x50\x00\xb8\x00\x77\x00\x74\x00\x1b\x00\x51\x00\x03\x00\xd9\x00\x14\x00\x03\x00\xde\x00\x52\x00\x05\x00\x16\x00\x1c\x00\x78\x00\x53\x00\x79\x00\x75\x00\x7a\x00\x7b\x00\x7c\x00\xff\xff\x14\x00\x07\x00\x54\x00\x10\x00\x55\x00\x25\x00\x15\x00\x09\x00\xb9\x00\x1d\x00\x56\x00\x57\x00\x0b\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x0c\x00\x5c\x00\x36\x00\x50\x00\x15\x00\x5d\x00\x5e\x00\x5f\x00\x51\x00\x1a\x00\x7e\x00\x14\x00\xda\x00\x7f\x00\x52\x00\xdf\x00\x0e\x00\x0f\x00\x7d\x00\x53\x00\x03\x00\x03\x00\x10\x00\x16\x00\x60\x00\x61\x00\x16\x00\x29\x00\x54\x00\x39\x00\x55\x00\xb3\x00\x15\x00\x09\x00\x3a\x00\x2a\x00\x56\x00\x57\x00\x0b\x00\x58\x00\x59\x00\x5a\x00\x5b\x00\x0c\x00\x5c\x00\x36\x00\x50\x00\x03\x00\x24\x00\x5e\x00\x5f\x00\x51\x00\x88\x00\x10\x00\x40\x00\x41\x00\x42\x00\x52\x00\x43\x00\x0e\x00\x0f\x00\x89\x00\x17\x00\x18\x00\x03\x00\x10\x00\x16\x00\x60\x00\x61\x00\x03\x00\x2e\x00\x54\x00\x8a\x00\x55\x00\x47\x00\x48\x00\x49\x00\xa7\x00\x66\x00\x56\x00\xc1\x00\x03\x00\x58\x00\x59\x00\x1d\x00\x5b\x00\x80\x00\x81\x00\x36\x00\x50\x00\x29\x00\x82\x00\x5e\x00\x67\x00\x51\x00\x36\x00\x17\x00\x25\x00\x28\x00\x83\x00\x52\x00\x31\x00\x84\x00\x38\x00\x16\x00\x66\x00\x3d\x00\x03\x00\x85\x00\x16\x00\x60\x00\x61\x00\x50\x00\x86\x00\x54\x00\x36\x00\x55\x00\x51\x00\x3a\x00\x14\x00\xcf\x00\xd0\x00\x56\x00\x52\x00\x31\x00\x58\x00\x59\x00\x87\x00\x5b\x00\x8e\x00\x2f\x00\x32\x00\x69\x00\x50\x00\x64\x00\x5e\x00\x09\x00\x54\x00\x51\x00\x55\x00\x15\x00\x0b\x00\x62\x00\xb4\x00\x52\x00\x56\x00\x0c\x00\xb5\x00\x58\x00\x59\x00\x03\x00\x5b\x00\x16\x00\x60\x00\x61\x00\x09\x00\x0a\x00\x3d\x00\x5e\x00\xb1\x00\x0b\x00\x16\x00\x0e\x00\x0f\x00\x3e\x00\x0c\x00\x56\x00\xb0\x00\x10\x00\xaf\x00\x59\x00\xae\x00\x5b\x00\x03\x00\xad\x00\x16\x00\x60\x00\x61\x00\x09\x00\x5e\x00\x0d\x00\x0e\x00\x0f\x00\x0b\x00\xac\x00\x09\x00\x93\x00\x10\x00\x0c\x00\x16\x00\x0b\x00\x92\x00\x91\x00\x2c\x00\x03\x00\x0c\x00\x16\x00\x60\x00\x61\x00\x16\x00\x3c\x00\x2d\x00\x36\x00\xc5\x00\x0e\x00\x0f\x00\xc4\x00\x8a\x00\x2d\x00\x8a\x00\x10\x00\x0e\x00\x0f\x00\xc3\x00\x40\x00\x41\x00\x42\x00\x10\x00\x43\x00\x8b\x00\x8c\x00\x8b\x00\xc6\x00\xbd\x00\xba\x00\xcb\x00\xca\x00\xbd\x00\xd3\x00\x40\x00\x41\x00\x42\x00\xd1\x00\x43\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xbe\x00\xbd\x00\xbf\x00\x40\x00\x41\x00\x42\x00\xd7\x00\x43\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xbe\x00\xbd\x00\xd3\x00\x40\x00\x41\x00\x42\x00\xe2\x00\x43\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xbe\x00\xd2\x00\xdc\x00\x40\x00\x41\x00\x42\x00\xe5\x00\x43\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x6a\x00\x6b\x00\x40\x00\x41\x00\x42\x00\xdc\x00\x43\x00\x12\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x6a\x00\x95\x00\x40\x00\x41\x00\x42\x00\x1f\x00\x43\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x6a\x00\xc1\x00\x40\x00\x41\x00\x42\x00\x26\x00\x43\x00\x11\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x6a\x00\xba\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x6d\x00\x44\x00\x10\x00\x45\x00\x1e\x00\x34\x00\x46\x00\x2f\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x70\x00\x36\x00\x8e\x00\x71\x00\xb6\x00\x94\x00\x8f\x00\x72\x00\x44\x00\x00\x00\xcb\x00\x73\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\xc8\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\xd5\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\xd4\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\xe0\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\xe3\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\xe5\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x40\x00\x41\x00\x42\x00\x03\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x44\x00\x00\x00\xe6\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x64\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xb5\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xb1\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xaa\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x93\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xc5\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xbb\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xcd\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xcc\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xc7\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xd7\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xda\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xdf\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\xe2\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\xa9\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\xa8\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\xa6\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\xa0\x00\x47\x00\x48\x00\x49\x00\xa5\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x47\x00\x48\x00\x49\x00\xa4\x00\x00\x00\x47\x00\x48\x00\x49\x00\xa3\x00\x00\x00\x47\x00\x48\x00\x49\x00\xa2\x00\x00\x00\x47\x00\x48\x00\x49\x00\xa1\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x47\x00\x48\x00\x9f\x00\x00\x00\x00\x00\x47\x00\x48\x00\x9e\x00\x00\x00\x00\x00\x47\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x47\x00\x6c\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x47\x00\x62\x00\x00\x00\x00\x00\x00\x00\x47\x00\x9d\x00\x00\x00\x00\x00\x00\x00\x47\x00\x9c\x00\x00\x00\x00\x00\x00\x00\x47\x00\x9b\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x47\x00\x9a\x00\x00\x00\x00\x00\x00\x00\x47\x00\x99\x00\x03\x00\x00\x00\x00\x00\x47\x00\x98\x00\x20\x00\x21\x00\x00\x00\x47\x00\x97\x00\x40\x00\x41\x00\x42\x00\x00\x00\x43\x00\x22\x00\x00\x00\x03\x00\x03\x00\x04\x00\x05\x00\x06\x00\x03\x00\x20\x00\x33\x00\x00\x00\x00\x00\x20\x00\x3a\x00\x03\x00\x47\x00\x96\x00\x07\x00\x22\x00\x20\x00\x3f\x00\x03\x00\x22\x00\x00\x00\x00\x00\x00\x00\x20\x00\x69\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 109) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109)
	]

happy_n_terms = 68 :: Int
happy_n_nonterms = 30 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ((Just (tokenLineCol happy_var_1), prToken happy_var_1)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 ((Just (tokenLineCol happy_var_1), read (prToken happy_var_1))
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 ((Just (tokenLineCol happy_var_1), CIdent (prToken happy_var_1))
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ((Just (tokenLineCol happy_var_1), VIdent (prToken happy_var_1))
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 ((fst happy_var_1, AbsSPL.Prog (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_7 = happyReduce 6# 6# happyReduction_7
happyReduction_7 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut17 happy_x_6 of { happy_var_6 -> 
	happyIn10
		 ((fst happy_var_1, AbsSPL.FnDef (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_8 = happyReduce 5# 6# happyReduction_8
happyReduction_8 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut22 happy_x_4 of { happy_var_4 -> 
	happyIn10
		 ((Just (tokenLineCol happy_var_1), AbsSPL.TypeDef (Just (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_9 = happyReduce 5# 6# happyReduction_9
happyReduction_9 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn10
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ClassDef (Just (tokenLineCol happy_var_1)) (snd happy_var_2) (AbsSPL.NoExtends Nothing) (reverse (snd happy_var_4)))
	) `HappyStk` happyRest}}}

happyReduce_10 = happyReduce 7# 6# happyReduction_10
happyReduction_10 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut7 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_6 of { happy_var_6 -> 
	happyIn10
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ClassDef (Just (tokenLineCol happy_var_1)) (snd happy_var_2) (AbsSPL.Extends (fst happy_var_4) (snd happy_var_4)) (reverse (snd happy_var_6)))
	) `HappyStk` happyRest}}}}

happyReduce_11 = happySpecReduce_1  7# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ((fst happy_var_1, [snd happy_var_1])
	)}

happyReduce_12 = happySpecReduce_2  7# happyReduction_12
happyReduction_12 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 ((fst happy_var_1, ((:) (snd happy_var_1) (snd happy_var_2)))
	)}}

happyReduce_13 = happySpecReduce_2  8# happyReduction_13
happyReduction_13 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 ((fst happy_var_1, AbsSPL.Arg (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_14 = happySpecReduce_0  9# happyReduction_14
happyReduction_14  =  happyIn13
		 ((Nothing, [])
	)

happyReduce_15 = happySpecReduce_1  9# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 ((fst happy_var_1, [snd happy_var_1])
	)}

happyReduce_16 = happySpecReduce_3  9# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 ((fst happy_var_1, (snd happy_var_1):(snd happy_var_3))
	)}}

happyReduce_17 = happySpecReduce_3  10# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn14
		 ((fst happy_var_1, AbsSPL.Field (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_18 = happyReduce 5# 10# happyReduction_18
happyReduction_18 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	case happyOut17 happy_x_5 of { happy_var_5 -> 
	happyIn14
		 ((Just (tokenLineCol happy_var_1), AbsSPL.Constr (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_19 = happyReduce 6# 10# happyReduction_19
happyReduction_19 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	case happyOut17 happy_x_6 of { happy_var_6 -> 
	happyIn14
		 ((fst happy_var_1, AbsSPL.Method (fst happy_var_1) (snd happy_var_1) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_20 = happySpecReduce_1  11# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 ((fst happy_var_1, [snd happy_var_1])
	)}

happyReduce_21 = happySpecReduce_3  11# happyReduction_21
happyReduction_21 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn15
		 ((fst happy_var_1, (snd happy_var_1):(snd happy_var_3))
	)}}

happyReduce_22 = happySpecReduce_0  12# happyReduction_22
happyReduction_22  =  happyIn16
		 ((Nothing, [])
	)

happyReduce_23 = happySpecReduce_2  12# happyReduction_23
happyReduction_23 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn16
		 ((fst happy_var_1, (snd happy_var_2):(snd happy_var_1))
	)}}

happyReduce_24 = happySpecReduce_3  13# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut18 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 ((Just (tokenLineCol happy_var_1), AbsSPL.Bl (Just (tokenLineCol happy_var_1)) (reverse (snd happy_var_2)))
	)}}

happyReduce_25 = happySpecReduce_0  14# happyReduction_25
happyReduction_25  =  happyIn18
		 ((Nothing, [])
	)

happyReduce_26 = happySpecReduce_2  14# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	case happyOut19 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 ((fst happy_var_1, (snd happy_var_2):(snd happy_var_1))
	)}}

happyReduce_27 = happySpecReduce_1  15# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.Empty (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_28 = happySpecReduce_1  15# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((fst happy_var_1, AbsSPL.BStmt (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_29 = happySpecReduce_3  15# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 ((fst happy_var_1, AbsSPL.Decl (fst happy_var_1) (snd happy_var_1) (snd happy_var_2))
	)}}

happyReduce_30 = happyReduce 4# 15# happyReduction_30
happyReduction_30 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 ((fst happy_var_1, AbsSPL.Ass (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_31 = happySpecReduce_3  15# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((fst happy_var_1, AbsSPL.Incr (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_32 = happySpecReduce_3  15# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((fst happy_var_1, AbsSPL.Decr (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_33 = happySpecReduce_3  15# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.Ret (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_34 = happySpecReduce_2  15# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.VRet (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_35 = happyReduce 5# 15# happyReduction_35
happyReduction_35 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut19 happy_x_5 of { happy_var_5 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.Cond (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_36 = happyReduce 7# 15# happyReduction_36
happyReduction_36 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut19 happy_x_5 of { happy_var_5 -> 
	case happyOut19 happy_x_7 of { happy_var_7 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.CondElse (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_37 = happyReduce 5# 15# happyReduction_37
happyReduction_37 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	case happyOut19 happy_x_5 of { happy_var_5 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.While (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5))
	) `HappyStk` happyRest}}}

happyReduce_38 = happyReduce 9# 15# happyReduction_38
happyReduction_38 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_7 of { happy_var_7 -> 
	case happyOut19 happy_x_9 of { happy_var_9 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ForUp (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7) (AbsSPL.EInt Nothing 1) (snd happy_var_9))
	) `HappyStk` happyRest}}}}}

happyReduce_39 = happyReduce 11# 15# happyReduction_39
happyReduction_39 (happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_7 of { happy_var_7 -> 
	case happyOut31 happy_x_9 of { happy_var_9 -> 
	case happyOut19 happy_x_11 of { happy_var_11 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ForUp (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7) (snd happy_var_9) (snd happy_var_11))
	) `HappyStk` happyRest}}}}}}

happyReduce_40 = happyReduce 10# 15# happyReduction_40
happyReduction_40 (happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_8 of { happy_var_8 -> 
	case happyOut19 happy_x_10 of { happy_var_10 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ForDown (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_8) (AbsSPL.EInt Nothing 1) (snd happy_var_10))
	) `HappyStk` happyRest}}}}}

happyReduce_41 = happyReduce 12# 15# happyReduction_41
happyReduction_41 (happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	case happyOut31 happy_x_8 of { happy_var_8 -> 
	case happyOut31 happy_x_10 of { happy_var_10 -> 
	case happyOut19 happy_x_12 of { happy_var_12 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ForUp (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_8) (snd happy_var_10) (snd happy_var_12))
	) `HappyStk` happyRest}}}}}}

happyReduce_42 = happyReduce 7# 15# happyReduction_42
happyReduction_42 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut31 happy_x_5 of { happy_var_5 -> 
	case happyOut19 happy_x_7 of { happy_var_7 -> 
	happyIn19
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ForEach (Just (tokenLineCol happy_var_1)) (snd happy_var_3) (snd happy_var_5) (snd happy_var_7))
	) `HappyStk` happyRest}}}}

happyReduce_43 = happySpecReduce_2  15# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((fst happy_var_1, AbsSPL.SExp (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_44 = happySpecReduce_1  16# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn20
		 ((fst happy_var_1, AbsSPL.NoInit (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_45 = happySpecReduce_3  16# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 ((fst happy_var_1, AbsSPL.Init (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_46 = happySpecReduce_1  17# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ((fst happy_var_1, [snd happy_var_1])
	)}

happyReduce_47 = happySpecReduce_3  17# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 ((fst happy_var_1, (snd happy_var_1):(snd happy_var_3))
	)}}

happyReduce_48 = happySpecReduce_1  18# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((Just (tokenLineCol happy_var_1), Type.Int (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_49 = happySpecReduce_1  18# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((Just (tokenLineCol happy_var_1), Type.Char (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_50 = happySpecReduce_1  18# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((Just (tokenLineCol happy_var_1), Type.Array (Just (tokenLineCol happy_var_1)) (Type.Char Nothing))
	)}

happyReduce_51 = happySpecReduce_1  18# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((Just (tokenLineCol happy_var_1), Type.Bool (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_52 = happySpecReduce_1  18# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((Just (tokenLineCol happy_var_1), Type.Void (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_53 = happySpecReduce_1  18# happyReduction_53
happyReduction_53 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((fst happy_var_1, Type.NamedType (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_54 = happySpecReduce_2  18# happyReduction_54
happyReduction_54 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 ((fst happy_var_1, Type.Array (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_55 = happyReduce 4# 18# happyReduction_55
happyReduction_55 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 ((fst happy_var_1, Type.Fun (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_56 = happySpecReduce_0  19# happyReduction_56
happyReduction_56  =  happyIn23
		 ((Nothing, [])
	)

happyReduce_57 = happySpecReduce_1  19# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 ((fst happy_var_1, [snd happy_var_1])
	)}

happyReduce_58 = happySpecReduce_3  19# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 ((fst happy_var_1, (snd happy_var_1):(snd happy_var_3))
	)}}

happyReduce_59 = happySpecReduce_1  20# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ENull (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_60 = happySpecReduce_1  20# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ETrue (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_61 = happySpecReduce_1  20# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), AbsSPL.EFalse (Just (tokenLineCol happy_var_1)))
	)}

happyReduce_62 = happySpecReduce_1  20# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, AbsSPL.EVar (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_63 = happySpecReduce_1  20# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, AbsSPL.EString (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_64 = happySpecReduce_1  20# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut4 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, AbsSPL.EInt (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_65 = happySpecReduce_1  20# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ((fst happy_var_1, AbsSPL.EChar (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_66 = happySpecReduce_3  20# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsSPL.EField (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	)}}

happyReduce_67 = happyReduce 4# 20# happyReduction_67
happyReduction_67 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsSPL.EArrAcc (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_68 = happyReduce 4# 20# happyReduction_68
happyReduction_68 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 ((fst happy_var_1, AbsSPL.EApp (fst happy_var_1) (snd happy_var_1) (snd happy_var_3))
	) `HappyStk` happyRest}}

happyReduce_69 = happySpecReduce_3  20# happyReduction_69
happyReduction_69 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 ((Just (tokenLineCol happy_var_1), (snd happy_var_2))
	)}}

happyReduce_70 = happySpecReduce_2  21# happyReduction_70
happyReduction_70 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 ((Just (tokenLineCol happy_var_1), AbsSPL.EUnaryOp (Just (tokenLineCol happy_var_1)) Neg (snd happy_var_2))
	)}}

happyReduce_71 = happySpecReduce_2  21# happyReduction_71
happyReduction_71 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 ((Just (tokenLineCol happy_var_1), AbsSPL.EUnaryOp (Just (tokenLineCol happy_var_1)) Not (snd happy_var_2))
	)}}

happyReduce_72 = happySpecReduce_2  21# happyReduction_72
happyReduction_72 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn25
		 ((Just (tokenLineCol happy_var_1), AbsSPL.EUnaryOp (Just (tokenLineCol happy_var_1)) BitNot (snd happy_var_2))
	)}}

happyReduce_73 = happySpecReduce_1  21# happyReduction_73
happyReduction_73 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (happy_var_1
	)}

happyReduce_74 = happySpecReduce_3  22# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Times (snd happy_var_3))
	)}}

happyReduce_75 = happySpecReduce_3  22# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Div (snd happy_var_3))
	)}}

happyReduce_76 = happySpecReduce_3  22# happyReduction_76
happyReduction_76 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Mod (snd happy_var_3))
	)}}

happyReduce_77 = happySpecReduce_3  22# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) LShift (snd happy_var_3))
	)}}

happyReduce_78 = happySpecReduce_3  22# happyReduction_78
happyReduction_78 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) RShift (snd happy_var_3))
	)}}

happyReduce_79 = happySpecReduce_3  22# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) BitAnd (snd happy_var_3))
	)}}

happyReduce_80 = happySpecReduce_3  22# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) BitOr (snd happy_var_3))
	)}}

happyReduce_81 = happySpecReduce_3  22# happyReduction_81
happyReduction_81 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn26
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) BitXor (snd happy_var_3))
	)}}

happyReduce_82 = happySpecReduce_1  22# happyReduction_82
happyReduction_82 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn26
		 (happy_var_1
	)}

happyReduce_83 = happySpecReduce_3  23# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Plus (snd happy_var_3))
	)}}

happyReduce_84 = happySpecReduce_3  23# happyReduction_84
happyReduction_84 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	happyIn27
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Minus (snd happy_var_3))
	)}}

happyReduce_85 = happySpecReduce_1  23# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  24# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Less (snd happy_var_3))
	)}}

happyReduce_87 = happySpecReduce_3  24# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) LessEq (snd happy_var_3))
	)}}

happyReduce_88 = happySpecReduce_3  24# happyReduction_88
happyReduction_88 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Greater (snd happy_var_3))
	)}}

happyReduce_89 = happySpecReduce_3  24# happyReduction_89
happyReduction_89 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) GreaterEq (snd happy_var_3))
	)}}

happyReduce_90 = happySpecReduce_3  24# happyReduction_90
happyReduction_90 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Equal (snd happy_var_3))
	)}}

happyReduce_91 = happySpecReduce_3  24# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) NotEqual (snd happy_var_3))
	)}}

happyReduce_92 = happySpecReduce_1  24# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

happyReduce_93 = happySpecReduce_3  25# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) And (snd happy_var_3))
	)}}

happyReduce_94 = happySpecReduce_3  25# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut29 happy_x_3 of { happy_var_3 -> 
	happyIn29
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) And (snd happy_var_3))
	)}}

happyReduce_95 = happySpecReduce_1  25# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_96 = happySpecReduce_3  26# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Or (snd happy_var_3))
	)}}

happyReduce_97 = happySpecReduce_3  26# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 ((fst happy_var_1, AbsSPL.EBinOp (fst happy_var_1) (snd happy_var_1) Or (snd happy_var_3))
	)}}

happyReduce_98 = happySpecReduce_1  26# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 (happy_var_1
	)}

happyReduce_99 = happyReduce 5# 27# happyReduction_99
happyReduction_99 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut7 happy_x_2 of { happy_var_2 -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), AbsSPL.EObjNew (Just (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_100 = happyReduce 5# 27# happyReduction_100
happyReduction_100 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_2 of { happy_var_2 -> 
	case happyOut31 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), AbsSPL.EArrNew (Just (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_101 = happyReduce 4# 27# happyReduction_101
happyReduction_101 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), AbsSPL.ELambda (Just (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4))
	) `HappyStk` happyRest}}}

happyReduce_102 = happySpecReduce_3  27# happyReduction_102
happyReduction_102 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 ((Just (tokenLineCol happy_var_1), AbsSPL.EArray (Just (tokenLineCol happy_var_1)) (snd happy_var_2))
	)}}

happyReduce_103 = happySpecReduce_1  27# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_104 = happySpecReduce_0  28# happyReduction_104
happyReduction_104  =  happyIn32
		 ((Nothing, [])
	)

happyReduce_105 = happySpecReduce_1  28# happyReduction_105
happyReduction_105 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 ((fst happy_var_1, [snd happy_var_1])
	)}

happyReduce_106 = happySpecReduce_3  28# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn32
		 ((fst happy_var_1, (snd happy_var_1):(snd happy_var_3))
	)}}

happyReduce_107 = happySpecReduce_1  29# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((fst happy_var_1, AbsSPL.Ret (fst happy_var_1) (snd happy_var_1))
	)}

happyReduce_108 = happyReduce 6# 29# happyReduction_108
happyReduction_108 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	case happyOut33 happy_x_4 of { happy_var_4 -> 
	case happyOut33 happy_x_6 of { happy_var_6 -> 
	happyIn33
		 ((Just (tokenLineCol happy_var_1), AbsSPL.CondElse (Just (tokenLineCol happy_var_1)) (snd happy_var_2) (snd happy_var_4) (snd happy_var_6))
	) `HappyStk` happyRest}}}}

happyReduce_109 = happySpecReduce_1  29# happyReduction_109
happyReduction_109 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn33
		 ((fst happy_var_1, AbsSPL.BStmt (fst happy_var_1) (snd happy_var_1))
	)}

happyNewToken action sts stk [] =
	happyDoAction 67# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (T_Keyword _ 1) -> cont 1#;
	PT _ (T_Keyword _ 2) -> cont 2#;
	PT _ (T_Keyword _ 3) -> cont 3#;
	PT _ (T_Keyword _ 4) -> cont 4#;
	PT _ (T_Keyword _ 5) -> cont 5#;
	PT _ (T_Keyword _ 6) -> cont 6#;
	PT _ (T_Keyword _ 7) -> cont 7#;
	PT _ (T_Keyword _ 8) -> cont 8#;
	PT _ (T_Keyword _ 9) -> cont 9#;
	PT _ (T_Keyword _ 10) -> cont 10#;
	PT _ (T_Keyword _ 11) -> cont 11#;
	PT _ (T_Keyword _ 12) -> cont 12#;
	PT _ (T_Keyword _ 13) -> cont 13#;
	PT _ (T_Keyword _ 14) -> cont 14#;
	PT _ (T_Keyword _ 15) -> cont 15#;
	PT _ (T_Keyword _ 16) -> cont 16#;
	PT _ (T_Keyword _ 17) -> cont 17#;
	PT _ (T_Keyword _ 18) -> cont 18#;
	PT _ (T_Keyword _ 19) -> cont 19#;
	PT _ (T_Keyword _ 20) -> cont 20#;
	PT _ (T_Keyword _ 21) -> cont 21#;
	PT _ (T_Keyword _ 22) -> cont 22#;
	PT _ (T_Keyword _ 23) -> cont 23#;
	PT _ (T_Keyword _ 24) -> cont 24#;
	PT _ (T_Keyword _ 25) -> cont 25#;
	PT _ (T_Keyword _ 26) -> cont 26#;
	PT _ (T_Keyword _ 27) -> cont 27#;
	PT _ (T_Keyword _ 28) -> cont 28#;
	PT _ (T_Keyword _ 29) -> cont 29#;
	PT _ (T_Keyword _ 30) -> cont 30#;
	PT _ (T_Keyword _ 31) -> cont 31#;
	PT _ (T_Keyword _ 32) -> cont 32#;
	PT _ (T_Keyword _ 33) -> cont 33#;
	PT _ (T_Keyword _ 34) -> cont 34#;
	PT _ (T_Keyword _ 35) -> cont 35#;
	PT _ (T_Keyword _ 36) -> cont 36#;
	PT _ (T_Keyword _ 37) -> cont 37#;
	PT _ (T_Keyword _ 38) -> cont 38#;
	PT _ (T_Keyword _ 39) -> cont 39#;
	PT _ (T_Keyword _ 40) -> cont 40#;
	PT _ (T_Keyword _ 41) -> cont 41#;
	PT _ (T_Keyword _ 42) -> cont 42#;
	PT _ (T_Keyword _ 43) -> cont 43#;
	PT _ (T_Keyword _ 44) -> cont 44#;
	PT _ (T_Keyword _ 45) -> cont 45#;
	PT _ (T_Keyword _ 46) -> cont 46#;
	PT _ (T_Keyword _ 47) -> cont 47#;
	PT _ (T_Keyword _ 48) -> cont 48#;
	PT _ (T_Keyword _ 49) -> cont 49#;
	PT _ (T_Keyword _ 50) -> cont 50#;
	PT _ (T_Keyword _ 51) -> cont 51#;
	PT _ (T_Keyword _ 52) -> cont 52#;
	PT _ (T_Keyword _ 53) -> cont 53#;
	PT _ (T_Keyword _ 54) -> cont 54#;
	PT _ (T_Keyword _ 55) -> cont 55#;
	PT _ (T_Keyword _ 56) -> cont 56#;
	PT _ (T_Keyword _ 57) -> cont 57#;
	PT _ (T_Keyword _ 58) -> cont 58#;
	PT _ (T_Keyword _ 59) -> cont 59#;
	PT _ (T_Keyword _ 60) -> cont 60#;
	PT _ (T_Keyword _ 61) -> cont 61#;
	PT _ (T_Int _) -> cont 62#;
	PT _ (T_CIdent _) -> cont 63#;
	PT _ (T_VIdent _) -> cont 64#;
	PT _ (T_String _) -> cont 65#;
	PT _ (T_Char _) -> cont 66#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 67# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut9 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before '" ++ id(prToken t) ++ "'"

myLexer = tokens

parseProgram :: [Token] -> Err (Program Pos)
parseProgram = (>>= return . snd) . pProgram
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 11 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "/tmp/ghcb5f8_0/ghc_2.h" #-}




























































































































































{-# LINE 11 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+#  i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 180 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+#  nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+#  nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
