module Parser.Syntax where

import Prelude

import Math (pow, log, sqrt, exp, sin, cos, tan, asin, acos, atan, pi) as Math

mathsinh :: Number -> Number
mathsinh x = (Math.exp x - Math.exp (-x)) / 2.0

mathcosh :: Number -> Number
mathcosh x = (Math.exp x + Math.exp (-x)) / 2.0

mathtanh :: Number -> Number
mathtanh x = (Math.exp x - Math.exp (-x)) / (Math.exp x + Math.exp (-x))

mathasinh :: Number -> Number
mathasinh x = Math.log (x + Math.sqrt(x * x + 1.0))

mathacosh :: Number -> Number
mathacosh x = Math.log (x + Math.sqrt(x * x - 1.0))

mathatanh :: Number -> Number
mathatanh x = 0.5 * Math.log ((1.0 + x) / (1.0 - x))

data Expr a = Lit a
          | Var Name
          | Binop Binop (Expr a) (Expr a)
          | Unop Unop (Expr a)

type Name = String

data Binop = Add | Sub | Mul | Div | Pow

data Unop = Negate | Sqrt | Log | Exp | Sin | Cos | Tan | Sinh | Cosh | Tanh
                   | Asin | Acos | Atan | Asinh | Acosh | Atanh

data Dual = Dual {height :: Number, slope :: Number}

derive instance eqDual :: Eq Dual

instance showDual :: Show Dual where
  show (Dual {height, slope}) = show height

instance semiringDual :: Semiring Dual where
  add (Dual {height: f, slope: f'}) (Dual {height: g, slope: g'}) =
    Dual {height: f + g, slope: f' + g'}
  mul (Dual {height: f, slope: f'}) (Dual {height: g, slope: g'}) =
    Dual {height: f * g, slope: f' * g + g' * f}
  zero = Dual {height: 0.0, slope: 0.0}
  one = Dual {height: 1.0, slope: 0.0}

instance ringDual :: Ring Dual where
  sub (Dual {height: f, slope: f'}) (Dual {height: g, slope: g'}) =
      Dual {height: f - g, slope: f' - g'}

instance divisionRingDual :: DivisionRing Dual where
  recip (Dual {height: g, slope: g'}) =
    Dual {height: one / g, slope: - g' / (g * g)}

instance commutativeRingDual :: CommutativeRing Dual

instance euclideanRingDual :: EuclideanRing Dual where
  degree _ = 0
  div d1 d2 = d1 * recip d2
  mod _ _ = zero

class Real t where
  fromNumber :: Number -> t
  pi :: t
  log :: t -> t
  sqrt :: t -> t
  exp :: t -> t
  sin :: t -> t
  cos :: t -> t
  tan :: t -> t
  sinh :: t -> t
  cosh :: t -> t
  tanh :: t -> t
  asin :: t -> t
  acos :: t -> t
  atan :: t -> t
  asinh :: t -> t
  acosh :: t -> t
  atanh :: t -> t

instance realNumber :: Real Number where
  pi = Math.pi
  fromNumber = identity
  log = Math.log
  sqrt = Math.sqrt
  exp = Math.exp
  sin = Math.sin
  cos = Math.cos
  tan = Math.tan
  sinh = mathsinh
  cosh = mathcosh
  tanh = mathtanh
  asin = Math.asin
  acos = Math.acos
  atan = Math.atan
  asinh = mathasinh
  acosh = mathacosh
  atanh = mathatanh


instance realDual :: Real Dual where
  pi = Dual {height: Math.pi, slope: 0.0}
  fromNumber x = Dual {height: x, slope: 0.0}
  log (Dual {height: f, slope: f'}) =
    Dual {height: Math.log f, slope: f'/f}
  sqrt (Dual {height: f, slope: f'}) =
    Dual {height: Math.sqrt f, slope: f'/(2.0 * Math.sqrt f)}
  exp (Dual {height: f, slope: f'}) =
    Dual {height: Math.exp f, slope: f' * Math.exp f}
  sin (Dual {height: f, slope: f'}) =
    Dual {height: Math.sin f, slope: f' * Math.cos f}
  cos (Dual {height: f, slope: f'}) =
    Dual {height: Math.cos f, slope: - f' * Math.sin f}
  tan (Dual {height: f, slope: f'}) =
    Dual {height: Math.tan f, slope: f' / Math.pow (Math.cos f) 2.0}
  sinh (Dual {height: f, slope: f'}) =
    Dual {height: mathsinh f, slope: f' * mathcosh f}
  cosh (Dual {height: f, slope: f'}) =
    Dual {height: mathcosh f, slope: f' * mathsinh f}
  tanh (Dual {height: f, slope: f'}) =
    Dual {height: mathtanh f, slope: f' / Math.pow (mathcosh f) 2.0}
  asin (Dual {height: f, slope: f'}) =
    Dual {height: Math.asin f, slope: f' / (Math.sqrt $ 1.0 - f * f)}
  acos (Dual {height: f, slope: f'}) =
    Dual {height: Math.acos f, slope: - f'/ (Math.sqrt $ 1.0 - f * f)}
  atan (Dual {height: f, slope: f'}) =
    Dual {height: Math.atan f, slope: f' / (1.0 + f * f)}
  asinh (Dual {height: f, slope: f'}) =
    Dual {height: mathasinh f, slope: f' / (Math.sqrt $ 1.0 + f * f)}
  acosh (Dual {height: f, slope: f'}) =
    Dual {height: mathacosh f, slope: f' / (Math.sqrt $ f * f - 1.0)}
  atanh (Dual {height: f, slope: f'}) =
    Dual {height: mathatanh f, slope:  f' / (1.0 - f * f)}

class Powerable t where
  pow :: t -> t -> t

instance powerNumber :: Powerable Number where
  pow = Math.pow

instance powerDual :: Powerable Dual where
  pow (Dual {height: f, slope: f'}) (Dual {height: g, slope: g'}) =
    Dual {height: Math.pow f g, slope: ((if g' == 0.0 then 0.0 else g' * Math.log f) + g * f' / f) * Math.pow f g}

instance showExpr :: Show a => Show (Expr a) where
  show (Lit x) = show x
  show (Var n) = n
  show (Binop Pow e1 e2) = show e1 <> "^{" <> show e2 <> "}"
  show (Binop Div e1 e2) = "\\dfrac{" <> show e1 <> "}{" <> show e2 <> "}"
  show (Binop op e1 e2) = "(" <> show e1 <> show op <> show e2 <> ")"
  show (Unop Log e1) = "\\ln(" <> show e1 <> ")"
  show (Unop Sqrt e1) = "\\sqrt{" <> show e1 <> "}"
  show (Unop Exp e1) = "\\mathrm{e}^{" <> show e1 <> "}"
  show (Unop Sin e1) = "\\sin(" <> show e1 <> ")"
  show (Unop Cos e1) = "\\cos(" <> show e1 <> ")"
  show (Unop Tan e1) = "\\tan(" <> show e1 <> ")"
  show (Unop Sinh e1) = "\\sinh(" <> show e1 <> ")"
  show (Unop Cosh e1) = "\\cosh(" <> show e1 <> ")"
  show (Unop Tanh e1) = "\\tanh(" <> show e1 <> ")"
  show (Unop Asin e1) = "\\arcsin(" <> show e1 <> ")"
  show (Unop Acos e1) = "\\arccos(" <> show e1 <> ")"
  show (Unop Atan e1) = "\\arctan(" <> show e1 <> ")"
  show (Unop Asinh e1) = "\\mathrm{arcsinh}(" <> show e1 <> ")"
  show (Unop Acosh e1) = "\\mathrm{arccosh}(" <> show e1 <> ")"
  show (Unop Atanh e1) = "\\mathrm{arctanh}(" <> show e1 <> ")"
  show (Unop op e1) = show op <> show e1

instance showBinop :: Show Binop where
  show Add = " + "
  show Sub = " - "
  show Mul = ""
  show Div = "{- cumbersome symbol for quotient -}"
  show Pow = "{- cumbersome symbol for exponent -}"

instance showUnop :: Show Unop where
  show Negate = "-"
  show Sqrt = "{- cumbersome symbol for square root -}"
  show Log = "{- cumbersome symbol for logarithm -}"
  show Exp = "{- cumbersome symbol for exponential -}"
  show Sin = "{- cumbersome symbol for sine -}"
  show Cos = "{- cumbersome symbol for cosine -}"
  show Tan = "{- cumbersome symbol for tangent -}"
  show Sinh = "{- cumbersome symbol for hyperbolic sine -}"
  show Cosh = "{- cumbersome symbol for hyperbolic cosine -}"
  show Tanh = "{- cumbersome symbol for hyperbolic tangent -}"
  show Asin = "{- cumbersome symbol for inverse sine -}"
  show Acos = "{- cumbersome symbol for inverse cosine -}"
  show Atan = "{- cumbersome symbol for inverse tangent -}"
  show Asinh = "{- cumbersome symbol for inverse hyperbolic sine -}"
  show Acosh = "{- cumbersome symbol for inverse hyperbolic cosine -}"
  show Atanh = "{- cumbersome symbol for inverse hyperbolic tangent -}"

derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance eqBinop :: Eq Binop
derive instance eqUnop :: Eq Unop
