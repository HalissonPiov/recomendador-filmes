-- PARTE 1: Tipo Algébrico
data Genero = Acao 
    | Comedia
    | Drama
    | FiccaoCientifica
    | Terror
    | Animacao
    deriving (Show, Eq, Ord) 

-- PARTE 2: CLASSIFICACAO INDICATIVA
data Classificacao = Livre
    | Maior10
    | Maior12
    | Maior14
    | Maior16
    | Maior18
    deriving (Show, Eq, Ord)

-- Parte 3: REPRESENTANDO UM FILME
data Filme = Filme String Genero Int Float Classificacao
    deriving (Show, Eq)

filme1 :: Filme
filme1 = Filme "Interestelar" FiccaoCientifica 2014 8.7 Maior10

filme2 :: Filme
filme2 = Filme "Toy Story" Animacao 1995 8.3 Livre

filme3 :: Filme
filme3 = Filme "O Poderoso Chefao" Drama 1972 9.2 Maior14

filme4 :: Filme
filme4 = Filme "Corra!" Terror 2017 7.8 Maior14

filme5 :: Filme
filme5 = Filme "As Branquelas" Comedia 2004 5.8 Maior12

filme6 :: Filme
filme6 = Filme "Mad Max: Estrada da Furia" Acao 2015 8.1 Maior16

filme7 :: Filme
filme7 = Filme "Os Sem-Floresta" Comedia 2006 6.7 Livre

filme8 :: Filme
filme8 = Filme "Carros" Acao 2006 7.3 Livre

filme9 :: Filme
filme9 = Filme "O Jogo da Imitacao" Drama 2014 8.0 Maior12

catalogo :: [Filme]
catalogo = [filme1, filme2, filme3, filme4, filme5, filme6]

