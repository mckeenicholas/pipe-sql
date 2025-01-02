module AST where

-- Base expressions
data Expr = 
    BinaryExpr BinaryExpr
  | UnaryExpr UnaryExpr
  | FuncCall FuncCall
  | Literal Literal
  | ColumnExpr Column
  deriving (Show, Eq)

-- Binary operations like AND, OR, +, -, etc.
data BinaryExpr = BinExpr {
    left :: Expr,
    binOp :: BinOp,
    right :: Expr
} deriving (Show, Eq)

-- Unary operations like NOT, -, etc.
data UnaryExpr = UnExpr {
    unOp :: UnaryOp,
    expr :: Expr
} deriving (Show, Eq)

-- Literal values (strings, numbers, etc.)
data Literal = 
    StringLit String
  | NumLit Double
  | BoolLit Bool
  | NullLit
  deriving (Show, Eq)

-- Function calls with name, arguments and optional alias
data FuncCall = Function {
    funcName :: String,
    args :: [Expr],
    funcAlias :: Maybe String
} deriving (Show, Eq)

-- Column reference with name and optional table
data Column = Column {
    columnName :: String,
    tableName :: Maybe String
} deriving (Show, Eq)

-- Table reference with name and optional alias
data Table = Table {
    name :: String,
    tableAlais :: Maybe String
} deriving (Show, Eq)

-- Order direction for ORDER BY
data OrderDir = ASC | DESC
  deriving (Show, Eq)

-- Join types
data JoinType = 
    LeftJoin
  | RightJoin 
  | FullJoin
  | InnerJoin
  | OuterJoin
  deriving (Show, Eq)

-- SQL clauses
data Clause =
    From FromClause
  | Select SelectClause
  | Order OrderClause
  | Group GroupClause
  | Join JoinClause
  | Where WhereClause
  deriving (Show, Eq)

-- Individual clause types
newtype FromClause = FromClause {
    tables :: [Table]
} deriving (Show, Eq)   

newtype SelectClause = SelectClause {
    expressions :: [Expr]
} deriving (Show, Eq)

newtype OrderClause = OrderClause {
    orderItems :: [(Expr, OrderDir)]
} deriving (Show, Eq)

newtype GroupClause = GroupClause {
    groupByExprs :: [Expr]
} deriving (Show, Eq)

data JoinClause = JoinClause {
    joinTable :: Table,
    joinCondition :: Expr,
    joinType :: JoinType
} deriving (Show, Eq)

newtype WhereClause = WhereClause {
    condition :: Expr
} deriving (Show, Eq)

-- Binary and Unary operators (you'll need to define these)
data BinOp = And | Or | Plus | Minus | Multiply | Divide -- Add more as needed
  deriving (Show, Eq)

data UnaryOp = Not | Negate
  deriving (Show, Eq)