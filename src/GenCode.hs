module GenCode where

import CodeM
import IR

import Control.Monad.Trans.State
import Control.Monad.Identity

data SState = SState { code :: [Code]
                     }

type GenCode = StateT SState Identity

execGenCode :: GenCode a -> [Code]
execGenCode m = reverse (code (runIdentity (execStateT m state)))
    where state = SState { code = [] }

emit :: Code -> GenCode ()
emit c = modify (\s -> s { code = c:(code s) } )

genCode :: [IR] -> [Code]
genCode ir = execGenCode (mapM_ genIR ir)

toVal :: ValIR -> Val
toVal (VarIR _) = VReg ax DWord
toVal (IntIR n) = VInt n DWord
toVal (BoolIR True) = VInt 1 Byte
toVal (BoolIR False) = VInt 0 Byte
toVal (LabelIR label) = VLabel label
toVal (ArgIR _) = VReg ax DWord

varToVal :: Var -> Val
varToVal _ = reg

getMem :: Val -> Val
getMem (VReg r size) = VMem r 0 size
getMem _ = reg

reg :: Val
reg = VReg ax DWord

genIR :: IR -> GenCode ()
genIR (IR_Label label)  = emit (CLabel (VLabel label))
genIR (IR_Ass x v)      = emit (CMov reg (toVal v))
genIR (IR_BinOp (BOpInt IAdd) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CAdd reg (toVal v2))
genIR (IR_BinOp (BOpInt ISub) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CSub reg (toVal v2))
genIR (IR_BinOp (BOpInt IMul) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CIMul (toVal v2))
genIR (IR_BinOp (BOpInt IDiv) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CIDiv (toVal v2))
genIR (IR_BinOp (BOpInt IMod) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CIDiv (toVal v2))
genIR (IR_BinOp (BOpInt ILshift) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CShiftL reg (toVal v2))
genIR (IR_BinOp (BOpInt IRshift) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CShiftR reg (toVal v2))
genIR (IR_BinOp (BOpInt IBitAnd) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CBitAnd reg (toVal v2))
genIR (IR_BinOp (BOpInt IBitOr) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CBitOr reg (toVal v2))
genIR (IR_BinOp (BOpInt IBitXor) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CBitXor reg (toVal v2))
genIR (IR_BinOp (BOpBool BAnd) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CBitAnd reg (toVal v2))
genIR (IR_BinOp (BOpBool BOr) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CBitOr reg (toVal v2))
genIR (IR_BinOp (BOpBool BXor) x v1 v2) =
    emit (CMov reg (toVal v1)) >> emit (CBitXor reg (toVal v2))
genIR (IR_BinOp (BOpRel LTH) x v1 v2) = emit (CCmp (toVal v1) (toVal v2))
genIR (IR_BinOp (BOpRel LEQ) x v1 v2) = emit (CCmp (toVal v1) (toVal v2))
genIR (IR_BinOp (BOpRel GTH) x v1 v2) = emit (CCmp (toVal v1) (toVal v2))
genIR (IR_BinOp (BOpRel GEQ) x v1 v2) = emit (CCmp (toVal v1) (toVal v2))
genIR (IR_BinOp (BOpRel EQU) x v1 v2) = emit (CCmp (toVal v1) (toVal v2))
genIR (IR_BinOp (BOpRel NEQ) x v1 v2) = emit (CCmp (toVal v1) (toVal v2))

genIR (IR_UnOp (UOpInt INeg) x v) = emit (CNeg (toVal v))
genIR (IR_UnOp (UOpInt INot) x v) = emit (CBitNot (toVal v))
genIR (IR_UnOp (UOpBool BNot) x v) = emit (CBitNot (toVal v))

genIR (IR_MemRead x v) = emit (CMov (varToVal x) (getMem (toVal v)))
genIR (IR_MemSave v1 v2) = emit (CMov (getMem (toVal v1)) (toVal v2))

genIR (IR_Param v) = emit (CMov (VReg di DWord) (toVal v))

genIR (IR_Call _ v _) = emit (CCall (toVal v))

genIR (IR_Return v) = do
    emit (CMov (VReg ax DWord) (toVal v))
    emit CRet

genIR (IR_Jump label) =
    emit (CJump (VLabel label))
genIR (IR_CondJump v1 op v2 label) = do
    emit (CCmp (toVal v1) (toVal v2))
    emit (CCondJump op (VLabel label))

genIR _ = emit CNop

