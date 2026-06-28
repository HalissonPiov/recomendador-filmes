data Genero = Acao 
    | Comedia
    | Drama
    | FiccaoCientifica
    | Terror
    | Animacao
    deriving (Show, Eq, Ord) 

data Classificacao = Livre
    | Maior10
    | Maior12
    | Maior14
    | Maior16
    | Maior18
    deriving (Show, Eq, Ord)

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
filme7 = Filme "Os Sem-Floresta" Animacao 2006 6.7 Livre

filme8 :: Filme
filme8 = Filme "Carros" Animacao 2006 7.3 Livre

filme9 :: Filme
filme9 = Filme "O Jogo da Imitacao" Drama 2014 8.0 Maior12

filme10 :: Filme
filme10 = Filme "Matrix" FiccaoCientifica 1999 8.7 Maior14

filme11 :: Filme
filme11 = Filme "Vingadores: Ultimato" Acao 2019 8.4 Maior12

filme12 :: Filme
filme12 = Filme "A Origem" FiccaoCientifica 2010 8.8 Maior14

filme13 :: Filme
filme13 = Filme "Procurando Nemo" Animacao 2003 8.2 Livre

filme14 :: Filme
filme14 = Filme "Forrest Gump" Drama 1994 8.8 Maior12

filme15 :: Filme
filme15 = Filme "Todo Mundo em Panico" Comedia 2000 6.3 Maior16

filme16 :: Filme
filme16 = Filme "Se Beber, Nao Case" Comedia 2009 7.7 Maior16

filme17 :: Filme
filme17 = Filme "Invocacao do Mal" Terror 2013 7.5 Maior14

filme18 :: Filme
filme18 = Filme "It: A Coisa" Terror 2017 7.3 Maior16

filme19 :: Filme
filme19 = Filme "Homem-Aranha: No Aranhaverso" Animacao 2018 8.4 Livre

filme20 :: Filme
filme20 = Filme "Gladiador" Acao 2000 8.5 Maior16

catalogo :: [Filme]
catalogo = [filme1, filme2, filme3, filme4, filme5, filme6, filme7, filme8, filme9, filme10, filme11, filme12, filme13, filme14, filme15, filme16, filme17, filme18, filme19, filme20]

-- Testes no GHCi:
-- > tituloFilme filme1
-- "Interestelar"
--
-- > tituloFilme filme2
-- "Toy Story"
tituloFilme :: Filme -> String
tituloFilme (Filme titulo _ _ _ _) = titulo

-- Testes no GHCi:
-- > notaFilme filme3
-- 9.2
--
-- > notaFilme filme5
-- 5.8
notaFilme :: Filme -> Float
notaFilme (Filme _ _ _ nota _) = nota

-- Testes no GHCi:
-- > pertenceAoGenero FiccaoCientifica filme1
-- True
--
-- > pertenceAoGenero Comedia filme1
-- False
--
-- > pertenceAoGenero Animacao filme2
-- True
pertenceAoGenero :: Genero -> Filme -> Bool
pertenceAoGenero generoBuscado (Filme _ generoDoFilme _ _ _) = generoBuscado == generoDoFilme

-- Testes no GHCi:
-- > possuiNotaMinima 8.0 filme1
-- True
--
-- > possuiNotaMinima 8.0 filme5
-- False
--
-- > possuiNotaMinima 9.0 filme3
-- True
possuiNotaMinima :: Float -> Filme -> Bool
possuiNotaMinima notaMinima (Filme _ _ _ notaDoFilme _) = notaDoFilme >= notaMinima

-- Testes no GHCi:
-- > recomendar FiccaoCientifica 8.0 catalogo
-- [Filme "Interestelar" FiccaoCientifica 2014 8.7 Maior10,Filme "Matrix" FiccaoCientifica 1999 8.7 Maior14,Filme "A Origem" FiccaoCientifica 2010 8.8 Maior14]
--
-- > recomendar Comedia 7.0 catalogo
-- [Filme "Se Beber, Nao Case" Comedia 2009 7.7 Maior16]
--
-- > recomendar Drama 8.0 catalogo
-- [Filme "O Poderoso Chefao" Drama 1972 9.2 Maior14,Filme "O Jogo da Imitacao" Drama 2014 8.0 Maior12,Filme "Forrest Gump" Drama 1994 8.8 Maior12]
recomendar :: Genero -> Float -> [Filme] -> [Filme]
recomendar _ _ [] = []
recomendar generoBuscado notaMinima (filme@(Filme _ generoFilme _ notaDoFilme _) : resto)
    | generoBuscado == generoFilme && notaDoFilme >= notaMinima = filme : recomendar generoBuscado notaMinima resto
    | otherwise = recomendar generoBuscado notaMinima resto

-- Testes no GHCi:
-- > permitidoPara Maior12 filme1
-- True
--
-- > permitidoPara Maior12 filme3
-- False
--
-- > permitidoPara Livre filme2
-- True
--
-- > permitidoPara Maior14 filme4
-- True
permitidoPara :: Classificacao -> Filme -> Bool
permitidoPara classificacaoMaxima (Filme _ _ _ _ classificacaoFilme) = classificacaoFilme <= classificacaoMaxima

-- Testes no GHCi:
-- > recomendarPara Animacao 8.0 Livre catalogo
-- ["Toy Story","Procurando Nemo","Homem-Aranha: No Aranhaverso"]
--
-- > recomendarPara Drama 8.0 Maior12 catalogo
-- ["O Jogo da Imitacao"]
--
-- > recomendarPara Acao 8.0 Maior12 catalogo
-- ["Vingadores: Ultimato"]
--
-- > recomendarPara Terror 8.0 Maior14 catalogo
-- []
recomendarPara :: Genero -> Float -> Classificacao -> [Filme] -> [String]
recomendarPara _ _ _ [] = []
recomendarPara generoBuscado notaMinima classificacaoMaxima (Filme titulo generoFilme _ notaDoFilme classificacaoFilme : resto)
    | generoBuscado == generoFilme && notaDoFilme >= notaMinima && classificacaoFilme <= classificacaoMaxima = titulo : recomendarPara generoBuscado notaMinima classificacaoMaxima resto
    | otherwise = recomendarPara generoBuscado notaMinima classificacaoMaxima resto