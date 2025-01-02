import Lexer

main :: IO ()
main = do
    let statement = "SELECT id FROM Users WHERE x > 100 AND b < 2"
    print $ tokenize statement