import SimpleJSON

data Doc = ToBeDefined
         deriving (Show)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

-- ++ for Doc
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

-- hcat - concat for Doc
hcat :: [Doc] -> Doc
hcat xs = undefined

