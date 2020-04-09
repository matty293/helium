{-# LANGUAGE TupleSections #-}

module Helium.CodeGeneration.LLVM.CompileBind
  ( compileBinds,
    toStruct,
  )
where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Either
import qualified Data.Graph as Graph
import Data.Word (Word32)
import qualified Helium.CodeGeneration.Iridium.Data as Iridium
import qualified Helium.CodeGeneration.Iridium.Type as Iridium
import qualified Helium.CodeGeneration.LLVM.Builtins as Builtins
import qualified Helium.CodeGeneration.Core.TypeEnvironment as Core
import Helium.CodeGeneration.LLVM.CompileStruct
import Helium.CodeGeneration.LLVM.CompileType
import Helium.CodeGeneration.LLVM.ConstructorLayout
import Helium.CodeGeneration.LLVM.Env (Env (..))
import Helium.CodeGeneration.LLVM.Struct
import Helium.CodeGeneration.LLVM.Target
import Helium.CodeGeneration.LLVM.Utils
import LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.IntegerPredicate as IntegerPredicate
import LLVM.AST.Operand
import LLVM.AST.Type as Type
import Lvm.Common.Id (Id, NameSupply, idFromString, mapWithSupply, mapWithSupply', splitNameSupply)
import Lvm.Common.IdMap (findMap)
import qualified Lvm.Core.Type as Core

idThunk :: Id
idThunk = idFromString "$alloc_thunk"

idACon :: Id
idACon = idFromString "$alloc_con"

idNACon :: Id
idNACon = idFromString "$nonalloc_con"

compileBinds :: Env -> NameSupply -> [Iridium.Bind] -> [Named Instruction]
compileBinds env supply binds = concat inits ++ concat assigns
  where
    (inits, assigns) = unzip $ mapWithSupply (compileBind env) supply $ sortBinds binds

compileBind :: Env -> NameSupply -> Iridium.Bind -> ([Named Instruction], [Named Instruction])
compileBind env supply b@(Iridium.Bind varId target args) =
  compileBind' env supply b $ toStruct env target $ length $ filter isRight args

compileBind' :: Env -> NameSupply -> Iridium.Bind -> Either Int Struct -> ([Named Instruction], [Named Instruction])
compileBind' env supply (Iridium.Bind varId target _) (Left tag) =
  ( [toName varId := AST.IntToPtr (ConstantOperand $ Constant.Int (fromIntegral $ targetWordSize $ envTarget env) value) voidPointer []],
    []
  )
  where
    -- Put a '1' in the least significant bit to distinguish it from a pointer.
    value :: Integer
    value = fromIntegral tag * 2 + 1
compileBind' env supply bind@(Iridium.Bind varId target args) (Right struct) =
  ( concat splitInstructions
      ++ getAllocate target nameBind (sizeOf env struct)
      ++ [nameStruct := BitCast operandVoid (pointer t) []]
      ++ castBind,
    additionalArgInstructions
      ++ initialize supplyInit env (LocalReference (pointer t) nameStruct) struct (additionalArgs ++ if shouldReverse then reverse argOperands else argOperands)
  )
  where
    t = structType env struct
    (shouldReverse, additionalArgInstructions, additionalArgs) = bindArguments env supplyAdditionalArgs target (length args') operandVoid
    args' = [arg | Right arg <- args]
    (splitInstructions, argOperands) = unzip $ fst $ mapWithSupply' splitValueFlag' supplyArgs (zip args' $ drop (length additionalArgs) $ fields struct)
    splitValueFlag' :: NameSupply -> (Iridium.Variable, StructField) -> (([Named Instruction], (Operand, Operand)), NameSupply)
    splitValueFlag' s (var, StructField tp Nothing)
      | not (Core.typeEqual (envTypeEnv env) (Iridium.variableType var) tp) =
        let (nameThunk, s1) = freshName s
            (s2, s3) = splitNameSupply s1
         in ( ( cast s2 env (toOperand env var) nameThunk (Iridium.variableType var) tp,
                ( LocalReference (compileType env tp) nameThunk,
                  operandTrue
                )
              ),
              s3
            )
      | otherwise =
        ( ( [],
            (toOperand env var, operandTrue)
          ),
          s
        )
    -- Flag is stored in header
    splitValueFlag' s (var, StructField tp _) = let (s1, s2) = splitNameSupply s in (splitValueFlag env s1 (var, tp), s2)
    (supplyArgs, supply1) = splitNameSupply supply
    (supplyInit, supply2) = splitNameSupply supply1
    (supplyAdditionalArgs, supply3) = splitNameSupply supply2
    (nameBind, supply4) = getNameReuse supply3 target
    (nameStruct, _) = freshNameFromId (nameSuggestion target) supply4
    whnf = Core.typeIsStrict $ Iridium.bindType (envTypeEnv env) bind
    operandVoid = LocalReference voidPointer nameBind
    castBind
      | whnf = [toName varId := BitCast operandVoid voidPointer []]
      | otherwise =
        [ toName varId
            := InsertValue
              (ConstantOperand $ Constant.Struct Nothing True [Constant.Undef voidPointer, Constant.Int 1 0])
              operandVoid
              [0]
              []
        ]

-- When debugging memory usage ds_alloc will keep track of memory usage of constructor allocation
allocate :: Operand -> Name -> Int -> [Named Instruction]
allocate op name size =
  [name := Call Nothing C [] (Right op) [(ConstantOperand $ Constant.Int 32 $ fromIntegral size, [])] [] []]

getAllocate :: Iridium.BindTarget -> Name -> Int -> [Named Instruction]
getAllocate (Iridium.BindTargetConstructor _ mv) name size = getDataAllocate mv name size
getAllocate (Iridium.BindTargetTuple _ mv) name size = getDataAllocate mv name size
getAllocate _ name size = allocate Builtins.fn_alloc name size

getDataAllocate :: Maybe Id -> Name -> Int -> [Named Instruction]
getDataAllocate reuse name size = maybe (allocate Builtins.ds_alloc name size) (const []) reuse

getNameReuse :: NameSupply -> Iridium.BindTarget -> (Name, NameSupply)
getNameReuse supply (Iridium.BindTargetConstructor _ mv) = getNameReuse' supply mv
getNameReuse supply (Iridium.BindTargetTuple _ mv) = getNameReuse' supply mv
getNameReuse supply _ = freshName supply

getNameReuse' :: NameSupply -> Maybe Id -> (Name, NameSupply)
getNameReuse' supply mv = maybe (freshName supply) ((,supply) . toName) mv

nameSuggestion :: Iridium.BindTarget -> Id
nameSuggestion (Iridium.BindTargetConstructor _ mv) = nameSuggestion' mv
nameSuggestion (Iridium.BindTargetTuple _ mv) = nameSuggestion' mv
nameSuggestion _ = idThunk

nameSuggestion' :: Maybe Id -> Id
nameSuggestion' mv = maybe idACon (const idNACon) mv

toStruct :: Env -> Iridium.BindTarget -> Int -> Either Int Struct
toStruct env (Iridium.BindTargetConstructor (Iridium.DataTypeConstructor conId _) _) arity = case findMap conId (envConstructors env) of
  LayoutInline value -> Left value
  LayoutPointer struct -> Right struct
toStruct env (Iridium.BindTargetTuple arity _) _ = Right $ tupleStruct arity
toStruct env target arity = Right $ Struct Nothing 0 0 fields
  where
    fields =
      StructField Iridium.typeUnsafePtr Nothing -- Thunk* next
        : StructField Iridium.typeTrampoline Nothing -- functionpointer to trampoline
        : StructField Iridium.typeInt16 Nothing -- i16 remaining
        : StructField Iridium.typeInt16 Nothing -- i16 given
        : replicate arity (StructField (Core.typeNotStrict $ Iridium.typeUnsafePtr) Nothing)

operandTrue :: Operand
operandTrue = ConstantOperand $ Constant.Int 1 1

-- A thunk has an additional argument, namely the function. We add that argument here
bindArguments :: Env -> NameSupply -> Iridium.BindTarget -> Int -> Operand -> (Bool, [Named Instruction], [(Operand, Operand)])
bindArguments env _ target@(Iridium.BindTargetFunction (Iridium.GlobalFunction fn arity _)) givenArgs self
  | arity == 0 && givenArgs /= 0 = error ("Cannot bind arguments to a global function with 0 arguments: " ++ show fn)
  | otherwise =
    ( True,
      [],
      [ (self, operandTrue),
        (ConstantOperand $ Constant.GlobalReference trampolineType $ toNamePrefixed "trampoline$" fn, operandTrue),
        (ConstantOperand $ Constant.Int 16 $ fromIntegral $ arity - givenArgs, operandTrue),
        (ConstantOperand $ Constant.Int 16 $ fromIntegral $ givenArgs, operandTrue)
      ]
    )
bindArguments env supply (Iridium.BindTargetThunk var) givenArgs _ =
  ( True,
    instrNext
      ++ [ nameNextThunk := BitCast operandNext thunkType [], -- Extract function pointer and 'remaining' from next thunk
           nameFnPtrPtr := getElementPtr operandNextThunk [0, 2],
           nameNextRemainingPtr := getElementPtr operandNextThunk [0, 3], -- What if 'remaining' is a magic value?
           nameFnPtr := Load False (LocalReference (pointer trampolineType) nameFnPtrPtr) Nothing 0 [],
           nameNextRemaining' := Load False (LocalReference (pointer $ IntegerType 16) nameNextRemainingPtr) Nothing 0 [],
           nameIsMagicNumber := ICmp IntegerPredicate.SGE (LocalReference (IntegerType 16) nameNextRemaining') (ConstantOperand $ Constant.Int 16 $ 32766) [],
           nameNextRemaining := Select (LocalReference boolType nameIsMagicNumber) (ConstantOperand $ Constant.Int 16 $ -1 - fromIntegral givenArgs) (LocalReference (IntegerType 16) nameNextRemaining') [],
           nameRemaining := Sub False False (LocalReference (IntegerType 16) nameNextRemaining) (ConstantOperand $ Constant.Int 16 $ fromIntegral givenArgs) []
         ],
    [ (operandNext, operandTrue),
      (LocalReference trampolineType nameFnPtr, operandTrue),
      (LocalReference (IntegerType 16) nameRemaining, operandTrue),
      (ConstantOperand $ Constant.Int 16 $ fromIntegral $ givenArgs, operandTrue)
    ]
  )
  where
    operandNextThunk = LocalReference thunkType nameNextThunk
    (nameNext, supply0) = freshName supply
    (nameNextThunk, supply1) = freshName supply0
    (nameFnPtrPtr, supply2) = freshName supply1
    (nameFnPtr, supply3) = freshName supply2
    (nameNextRemainingPtr, supply4) = freshName supply3
    (nameNextRemaining', supply5) = freshName supply4
    (nameNextRemaining, supply6) = freshName supply5
    (nameRemaining, supply7) = freshName supply6
    (nameIsMagicNumber, _) = freshName supply7
    (instrNext, operandNext)
      | Core.typeIsStrict (Iridium.variableType var) = ([], toOperand env var)
      | otherwise =
        ( [nameNext := ExtractValue (toOperand env var) [0] []],
          LocalReference voidPointer nameNext
        )
bindArguments env _ _ _ _ = (False, [], [])

-- Sorts binds such that if a bind targets a thunk defined in the same block,
-- then that thunk is declared before this bind
-- Before: letalloc %a = thunk %b .., %b = ..
-- After: letalloc %b = .., %a = thunk %b ..
sortBinds :: [Iridium.Bind] -> [Iridium.Bind]
sortBinds = map getBind . Graph.stronglyConnComp . map node
  where
    node :: Iridium.Bind -> (Iridium.Bind, Id, [Id])
    node bind@(Iridium.Bind name (Iridium.BindTargetThunk target) _) =
      (bind, name, [Iridium.variableName target])
    node bind@(Iridium.Bind name _ _) =
      (bind, name, [])
    getBind :: Graph.SCC Iridium.Bind -> Iridium.Bind
    getBind (Graph.AcyclicSCC bind) = bind
    getBind _ = error "sortBinds: Found cyclic dependency in bind targets of thunks"
