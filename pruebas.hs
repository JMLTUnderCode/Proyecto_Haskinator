import System.IO

main:: IO ()
main = do
    content <- readFile "testFile.txt"
    print content