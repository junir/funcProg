module Main where

import Test.Hspec

type Context = [(String, Binding)]

data Binding = NameBind
			 | VarBind Type


setBinding :: Context -> String -> Binding -> Context
setBinding ctx x bind = (x, bind):ctx --добавляю в контекст связь

getBinding :: Context -> Int -> Binding
getBinding ctx index | index > length ctx = error "requested index higher than context"
                      | otherwise = snd $ ctx !! (length ctx - index - 1) --достаем из контекста по индексу, но только если индекс не превышает размер контекста

data Type = BooleanType
		  | ArrType Type Type --стрелочка
		  deriving (Show,Eq)


data Term = Var Int Int
		  | Lambda String Type Term
		  | App Term Term
		  | TermTrue
		  | TermFalse
		  | TermIf Term Term Term


getType :: Term -> Type
getType term = typeof term []

typeof :: Term -> Context -> Type
typeof term ctx = case term of
	TermTrue -> BooleanType
	TermFalse -> BooleanType
	TermIf term1 term2 term3 ->
		if (typeof term1 ctx) == BooleanType then --если тип терма1 булин
			let term2Type = typeof term2 ctx in  --то сравниваем типы терма 2 и терма 3
			if term2Type == typeof term3 ctx  then term2Type --если равны, возвращаем его
				else error "arms of conditional have different types" --если нет, бросаем ошибку
		else error "guard of conditional not a boolean"
	Var index _ -> --если переменая
	   let bind = getBinding ctx index in --достаем ее значение из контекста
	   case bind of
	       NameBind -> error "no name bindings"
	       VarBind typ -> typ
	Lambda arg argType t -> --если абстракция
	    let ctx' = setBinding ctx arg (VarBind(argType)) in --довабляем новую связь
		let bodyType = typeof t ctx' in --находим тип терма по новому контексту
		ArrType argType bodyType --возвращаем : Тип -> Тип
	App t1 t2 -> --если аппликация
		let t1Type = (typeof t1 ctx) in --достаем все типы терма1
		let t2Type = (typeof t2 ctx) in -- терма 2
		case t1Type of
			ArrType t11Type t12Type ->
				if t2Type == t11Type --сравниваем типы из терма 1 с типом терма 2
					then t12Type --если тру - выводим этот тип
					else error "type mismatch in App" --выводим ошибку если нет
			_ -> error "App must be of an array type" --если нет нужного конструктора - выводим ошибку


term0 = TermFalse

-- BooleanType
term1 = TermIf TermFalse (App (Lambda "s" BooleanType TermFalse) (TermTrue)) TermTrue

-- ArrType
term2 = Lambda "s" BooleanType TermFalse

term3 = TermIf TermFalse TermTrue TermFalse

main = hspec $ do
  describe "Tests" $ do
    it "BooleanType" $
       getType term0 `shouldBe` BooleanType
    it "BooleanType1" $
       getType term1 `shouldBe` BooleanType
    it "Arr" $
       getType term2 `shouldBe` ArrType BooleanType BooleanType
