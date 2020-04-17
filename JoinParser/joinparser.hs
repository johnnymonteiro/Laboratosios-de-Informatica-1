module Main where
import System.IO
import System.Environment
import System.Process

type Nome = String
type Email = String
type Curso = String 
type Numero = String
type Univ = String
type Curso1 = String
type Filiacao = String
type Jantar = String
type Almoco = String
type Join = String
type Outro = String
type Data = String

type Inscricao = (Nome, Email, Curso, Numero, Univ, Curso1, Filiacao, Jantar, Almoco, Join, Outro, Data)



-- SECCAO IO
main = do  
--tarefa1
	putStrLn "+++++++++++++++++++++++++++++++++++++++++++"
	putStrLn "++**        JOIN PARSER 3.4beta        **++"
	putStrLn "++++** Insira o nome do ficheiro csv **++++"
	putStrLn "++++___________________________________++++"
	csvname <- getLine
	x <- readFile (csvname ++".csv")
	y <- return ((drop 1 (lines x)) :: [String])
	z <- return ((virgula y) :: [[String]])
	w <- return ((map tuplar z))
	c <- return ((validacao w)++(repetidos w) ++ (validnames w))
--tarefa2
	a <- return (diferentes w)
	as <- return (diferentesx a)
	n <- return (nomes as)
	parsinglatex <- return( parsingtex (c))
	alunos <- return (isaluno n)
	externos <- return (isexterno n)
	empresas <- return (isempresa n)
	crashaaluno <- return (tex (texalunos alunos))
	crashaexterno <- return (tex (texexterno externos))
	crashaempresa <- return(tex (texempresa empresas))
	multipagescrashaaluno <- return (mpc ("cards_alunos.pdf"))
	multipagescrashaexterno <- return (mpc ("cards_externos.pdf"))
	multipagescrashaempresa <- return (mpc ("cards_empresas.pdf"))
--tarefa3&4
	cscount <- return (zipall (+) (map statsc (as)))
	csdatcount <- return (csdat (cscount))
	aljncount <- return (zipall (+) (map statsaj (as)))
	aljndatcount <- return (aljndat (aljncount))
	incount <- return (zipall (+) (map statsi (as)))
        indatcount <- return (indat (incount))
      	analiseintmontar <- return(analiseint (n))
	analiseajmontar <- return(analiseaj (n))
	analisedatint <- return(analiseintdat (analiseintmontar))
	analisedataj <- return(analiseajdat (analiseajmontar))
	tabela1 <- return(unitable1 (cscount))
	tabela2 <- return(unitable2 (incount))
	tabela3 <- return(unitable3 (aljncount))
 	tabela4 <- return(multable1 (analiseintmontar))
	tabela5 <- return(multable2 (analiseajmontar))
	statslatex <- return(juntatex (tabela1) (tabela2) (tabela3) (tabela4) (tabela5))
	graficos <- readFile "gnustats/graficos.tex"
--criacaodeficheiros
	writeFile "CRASHAS_ALUNOS.tex" multipagescrashaaluno
	writeFile "CRASHAS_EXTERNOS.tex" multipagescrashaexterno
	writeFile "CRASHAS_EMPRESAS.tex" multipagescrashaempresa
	writeFile "cards_alunos.tex" crashaaluno
	writeFile "cards_externos.tex" crashaexterno
	writeFile "cards_empresas.tex" crashaempresa
	writeFile "int.dat" indatcount
   	writeFile "cs.dat" csdatcount
	writeFile "aj.dat" aljndatcount 
	writeFile "Estatisticas.tex" statslatex
	writeFile "ESTATS.tex" statslatex
	writeFile "analise1.dat" analisedatint
	writeFile "analise2.dat" analisedataj 
	writeFile "Parsing.tex" parsinglatex
	writeFile "Graficos.tex" graficos
--shellscript
	runCommand "sh scrip.sh"
	



-- SECCAO FUNCOES 
-- TAREFA 1

virgula :: [String] -> [[String]]
virgula [ ] = [ ]
virgula (x:xs) = (splitstringx x 0) : (virgula xs)


					
splitstringx :: String -> Int -> [String]
splitstringx [] _ = [[]]
splitstringx (c:cs) x = let (s:ss) = splitstringx cs x
					in if (c==',') && (even x) then []:(s:ss) 
					   else if (c=='\"') then (splitstringx cs (x+1))
							    else (c:s):ss

tuplar :: [String] -> (Inscricao)
tuplar (n:n2:n3:n4:n5:n6:n7:n8:n9:n10:n11:n12:n13) = (n,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12)


compara :: [(Inscricao)] -> Bool
compara [x] = True
compara ((n,e,c,nm,u,c1,f,j,a,jo,o,d):(ns,es,cs,nms,us,c1s,fs,js,as,jos,os,ds):r) = ((e /= es) && (compara ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r)))

repetidos :: [(Inscricao)] -> String
repetidos [x] = ""
repetidos ((n,e,c,nm,u,c1,f,j,a,jo,o,d):(ns,es,cs,nms,us,c1s,fs,js,as,jos,os,ds):r) = if (compara ((n,e,c,nm,u,c1,f,j,a,jo,o,d):(ns,es,cs,nms,us,c1s,fs,js,as,jos,os,ds):r) == True) then repetidos ((ns,es,cs,nms,us,c1s,fs,js,as,jos,os,ds):r)
                                                                                   else "\\item{O registo com o email " ++ show e ++ " é repetido.}\n" ++ (repetidos ((ns,es,cs,nms,us,c1s,fs,js,as,jos,os,ds):r))

validacao :: [(Inscricao)] -> String
validacao [] = ""
validacao ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r)   | (n /= "") && (e /= "") && (c /= "") && (nm /= "") && (u == "") && (c1 == "") && (f == "") = validacao r
											| (n /= "") && (e /= "") && (c == "") && (nm == "") && (u /= "") && (c1 /= "") && (f == "") = validacao r
											| (n /= "") && (e /= "") && (c == "") && (nm == "") && (u == "") && (c1 == "") && (f /= "") = validacao r
											| otherwise = "\\item {O registo com o email " ++ show e ++ " tem a inscric\227o inv\225lida.}\n" ++ (validacao r)

validnames :: [(Inscricao)] -> String
validnames [] = ""
validnames ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r) | ((length(words(n))) < 2) = "\\item {O registo com o email " ++ show e ++ " tem o nome mal preenchido.}\n" ++ (validnames r)
					    | otherwise = (validnames r) 


parsingtex :: String -> String
parsingtex s = "\\documentclass{article}\n\\usepackage{a4wide}\n\\usepackage[portuges]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{verbatim}\n\\usepackage{graphicx}\n\\usepackage{eso-pic}\n\n\\title{Parsing das Inscri\231\245es}\n\n\\begin{document}\n\\maketitle\n\\begin{itemize}\n"++ s ++"\n\\end{itemize}\n\\end{document}\n"
 





diferentes :: [(Inscricao)] -> [(Inscricao)]
diferentes [x] = [x]
diferentes (t1:t2:r) = if (compara (t1:t2:r) == True) then (t1 : diferentes(t2:r)) else (diferentes (t2:r))


						
diferentesx :: [(Inscricao)] -> [(Inscricao)]
diferentesx [] = []
diferentesx ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r)| (n /= "") && (e /= "") && (c /= "") && (nm /= "") && (u == "") && (c1 == "") && (f == "") = (n,e,c,nm,u,c1,f,j,a,jo,o,d) : diferentesx r
											| (n /= "") && (e /= "") && (c == "") && (nm == "") && (u /= "") && (c1 /= "") && (f == "") = (n,e,c,nm,u,c1,f,j,a,jo,o,d) : diferentesx r
											| (n /= "") && (e /= "") && (c == "") && (nm == "") && (u == "") && (c1 == "") && (f /= "") = (n,e,c,nm,u,c1,f,j,a,jo,o,d) : diferentesx r
											| otherwise = diferentesx r
											

-- TAREFA 2

fsls :: String -> String
fsls x = head (words x) ++ " " ++ last (words x)

nomes :: [(Inscricao)] -> [(Inscricao)]
nomes [] = []
nomes ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r) = ((fsls n),e,c,nm,u,c1,f,j,a,jo,o,d) : (nomes r)

											
isaluno :: [(Inscricao)] -> [(Inscricao)]
isaluno [] = []
isaluno ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r)| (n /= "") && (e /= "") && (c /= "") && (nm /= "") && (u == "") && (c1 == "") && (f == "") = (n,e,c,nm,u,c1,f,j,a,jo,o,d) : (isaluno r)
										|otherwise = isaluno r

isexterno :: [(Inscricao)] -> [(Inscricao)]
isexterno [] = []
isexterno ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r)| (n /= "") && (e /= "") && (c == "") && (nm == "") && (u /= "") && (c1 /= "") && (f == "") = (n,e,c,nm,u,c1,f,j,a,jo,o,d) : 					(isexterno r)
										   |otherwise = isexterno r

isempresa :: [(Inscricao)] -> [(Inscricao)]	
isempresa [] = []										
isempresa ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r)| (n /= "") && (e /= "") && (c == "") && (nm == "") && (u == "") && (c1 == "") && (f /= "") = (n,e,c,nm,u,c1,f,j,a,jo,o,d) : (isempresa r)						
										  |otherwise = isempresa r	
texalunos :: [(Inscricao)] -> String
texalunos [] = ""
texalunos ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r) ="\\AddToShipoutPicture{\\BackgroundPic{design/background-participante}}\n\n\\includegraphics{design/logo}\\\\\n\n\\addvspace{5mm}\n\n\\begin{center}\n\t\\huge{NAME}\n\t\\scriptsize{\n\t\\begin{tabular*}{0.75\\textwidth}{c}\n\t"++ n ++" \\\\\n\t\t\\hline\n\t\\end{tabular*}}\\\\\n\tCOMPANY\\\\ Universidade do Minho - " ++ c ++ "\n\t\t\n\\end{center}\n\n\\begin{flushright}\n\t\\begin{tabular}{r l l}\n%\t\t\\normalsize{Participante}& &\n\t\t\\normalsize{Participante} & &\n\t\\end{tabular}\n\\end{flushright}\n\n\\pagebreak" ++ (texalunos r)

texexterno :: [(Inscricao)] -> String
texexterno [] = ""
texexterno ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r) ="\\AddToShipoutPicture{\\BackgroundPic{design/background-participante}}\n\n\\includegraphics{design/logo}\\\\\n\n\\addvspace{5mm}\n\n\\begin{center}\n\t\\huge{NAME}\n\t\\scriptsize{\n\t\\begin{tabular*}{0.75\\textwidth}{c}\n\t"++ n ++" \\\\\n\t\t\\hline\n\t\\end{tabular*}}\\\\\n\tCOMPANY\\\\ "++ u ++ " - " ++ c1 ++ "\n\t\t\n\\end{center}\n\n\\begin{flushright}\n\t\\begin{tabular}{r l l}\n%\t\t\\normalsize{Participante}& &\n\t\t\\normalsize{Participante} & &\n\t\\end{tabular}\n\\end{flushright}\n\n\\pagebreak" ++ (texexterno r)

texempresa :: [(Inscricao)] -> String
texempresa [] = ""
texempresa ((n,e,c,nm,u,c1,f,j,a,jo,o,d):r) ="\\AddToShipoutPicture{\\BackgroundPic{design/background-participante}}\n\n\\includegraphics{design/logo}\\\\\n\n\\addvspace{5mm}\n\n\\begin{center}\n\t\\huge{NAME}\n\t\\scriptsize{\n\t\\begin{tabular*}{0.75\\textwidth}{c}\n\t"++ n ++" \\\\\n\t\t\\hline\n\t\\end{tabular*}}\\\\\n\tCOMPANY\\\\" ++ f ++ "\n\t\t\n\\end{center}\n\n\\begin{flushright}\n\t\\begin{tabular}{r l l}\n%\t\t\\normalsize{Participante}& &\n\t\t\\normalsize{Participante} & &\n\t\\end{tabular}\n\\end{flushright}\n\n\\pagebreak" ++ (texempresa r)

tex :: String -> String
tex x = "\\documentclass{article}\n\\usepackage[portuges]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{graphicx}\n\\usepackage{eso-pic}\n\n\n\\paperwidth 89mm\n\\paperheight 55mm\n\\pdfpagewidth=\\paperwidth\n\\pdfpageheight=\\paperheight\n\\textwidth       89mm\n\\textheight      55in\n\\oddsidemargin  -26mm\n\\evensidemargin -26mm\n\\hoffset\t 0mm\n\\topmargin      -33mm\n\\itemindent      0mm\n\\parindent       0mm\n\\renewcommand\\familydefault{\\sfdefault}\n\n\n%%%%%%%%%%%%%%%%%%%%%%%%%%%% BEGIN DOCUMENT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\\setlength{\\unitlength}{1mm}\n\\newcommand\\BackgroundPic[1]{\n\\put(-4,0){\\parbox[b][\\paperheight]{\\paperwidth}{%\n\\vfill\n\\centering\n\\includegraphics{#1}%\n\\vfill\n}}}\n\n\n\\pagestyle{empty}\n\\begin{document}\n" ++ x ++ "\\end{document}\n"

mpc :: String -> String
mpc s ="\\documentclass{article}\n\\usepackage[portuges]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{graphicx}\n\\usepackage{eso-pic}\n\\usepackage{pdfpages}\n\\begin{document}\n\\includepdf[nup=2x5,pages=-]{"++ s ++"}\n\\end{document}\n"


--TAREFA3


zipall::(a->a->a)->[[a]]->[a]
zipall _ [] = []
zipall _ (a:[]) = a
zipall f (l:ls) = zipWith f l (zipall f ls)



statsc :: (Inscricao) -> [Int]
statsc (n,e,c,nm,u,c1,f,j,a,jo,o,d) | (c == "LEI") = [1,0,0,0,0,0]
  				    | (c == "LCC") = [0,1,0,0,0,0]
				    | (c == "MEI") = [0,0,1,0,0,0]
				    | (c == "MI") = [0,0,0,1,0,0]
				    | (c == "MERSCOM") = [0,0,0,0,1,0]
				    | (c == "MBIO") = [0,0,0,0,0,1]
				    | otherwise = [0,0,0,0,0,0]


statsaj :: (Inscricao) -> [Int]
statsaj (n,e,c,nm,u,c1,f,j,a,jo,o,d) | (a == "1") && (j == "1") = [1,1]
				     | (a == "1") && (j == "0") = [1,0]
				     | (a == "0") && (j == "1") = [0,1]
				     | (a == "0") && (j == "0") = [0,0]
																	
										
csdat :: [Int] -> String
csdat (a:b:c:d:e:f:r) = ("1\t" ++ show a ++ "\n2\t " ++ show b ++ "\n3\t " ++  show c ++ "\n4\t " ++ show d ++ "\n5\t " ++ show e ++ "\n6\t" ++ show f) 


statsi :: (Inscricao) -> [Int]
statsi (n,e,c,nm,u,c1,f,j,a,jo,o,d)   	| (jo == "['Procura 1\\xc2\\xba emprego', 'Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado', 'Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas']") = [1,1,1]
					| (jo == "['Procura 1\\xc2\\xba emprego', 'Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado']") = [1,1,0]
					| (jo == "['Procura 1\\xc2\\xba emprego', 'Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas']") = [1,0,1]
					| (jo == "['Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado', 'Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas']") = [0,1,1]
					| (jo == "['Procura 1\\xc2\\xba emprego']") = [1,0,0]
					| (jo == "['Procura projecto/disserta\\xc3\\xa7\\xc3\\xa3o de mestrado']") = [0,1,0] 
					| (jo == "['Interesse pelas \\xc3\\xa1reas tem\\xc3\\xa1ticas']") = [0,0,1]
					| otherwise = [0,0,0]


indat :: [Int] -> String
indat (a:b:c:r) = ("1\t" ++ show a ++ "\n2\t " ++ show b ++ "\n3\t " ++  show c) 

aljndat :: [Int] -> String
aljndat (a:b:r) = ("1\t " ++ show a ++ "\n2\t " ++ show b)


unitable1 :: [Int] -> String
unitable1 (n:n2:n3:n4:n5:n6:r) = "\\begin{center}\n\\begin{tabular}[b]{|c||c|}\n\\hline\n\\textbf{\\emph{Curso}} & Numero de Inscri\231\245es\\\\\n\\hline\nLEI & " ++ show n ++ "\\\\\n\\hline\nLCC & " ++ show n2 ++ "\\\\\n\\hline\nMI & " ++ show n3 ++ "\\\\\n\\hline\nMEI & " ++ show n4 ++ "\\\\\n\\hline\nMERSCOM & " ++ show n5 ++ "\\\\\n\\hline\nMBIO & " ++ show n6 ++ "\\\\\n\\hline\n\\end{tabular}\n\\end{center}"

unitable2 :: [Int] -> String
unitable2 (n:n2:n3:r) = "\\begin{center}\n\\begin{tabular}[b]{|c||c|}\n\\hline\n\\textbf{\\emph{Interesses}} & Numero de Inscri\231\245es\\\\\n\\hline\nProcura 1\186 Emprego & " ++ show n ++ "\\\\\n\\hline\nProcura projecto/disserta\231\227o de mestrado & " ++ show n2 ++ "\\\\\n\\hline\nInteresse pelas \225reas tem\225ticas & " ++ show n3 ++ "\\\\\n\\hline\nTotal & " ++ show (n+n2+n3) ++ "\\\\\n\\hline\n\\end{tabular}\n\\end{center}"

unitable3 :: [Int] -> String
unitable3 (n:n2:r) = "\\begin{center}\n\\begin{tabular}[b]{|c||c|}\n\\hline\n\\textbf{\\emph{Almo\231o / Jantar}} & Numero de Inscri\231\245es\\\\\n\\hline\nAlmo\231o & " ++ show n ++ "\\\\\n\\hline\nJantar & " ++ show n2 ++"\\\\\n\\hline\n\\end{tabular}\n\\end{center}"


-- TAREFA 4

filterx :: (String -> Bool) -> [(Inscricao)] -> [(Inscricao)]
filterx _ [] = []
filterx p (((n,e,c,nm,u,c1,f,j,a,jo,o,d):r)) | p c       = ((n,e,c,nm,u,c1,f,j,a,jo,o,d) : (filterx p r))
               			             | otherwise = filterx p r

analise :: [(Inscricao)] -> String -> (String,[Int])
analise l s |((filterx (s==) l) == []) = (s,[0,0,0])
	    |otherwise = (s,(zipall (+) (map statsi (filterx (s==) l))))

analise2 :: [(Inscricao)] -> String -> (String,[Int])
analise2 l s |((filterx (s==) l) == []) = (s,[0,0])
             |otherwise = (s,(zipall (+) (map statsaj (filterx (s==) l))))


analiseint :: [(Inscricao)] -> [(String,[Int])]
analiseint l = [(analise l "LEI"),(analise l "LCC"),(analise l "MEI"),(analise l "MI"),(analise l "MERSCOM"),(analise l "MBIO")]

analiseaj :: [(Inscricao)] -> [(String,[Int])]
analiseaj l = [(analise2 l "LEI"),(analise2 l "LCC"),(analise2 l "MEI"),(analise2 l "MI"),(analise2 l "MERSCOM"),(analise2 l "MBIO")]



analiseintdat :: [(String,[Int])] -> String
analiseintdat [] = " "
analiseintdat ((c,[]):r) = ("\"" ++ c ++ " 0 0 0\n") ++ analiseintdat r
analiseintdat ((c,(h:t:z:x)):r) =  ("\"" ++ c ++ "\" " ++ show h ++ " " ++ show t ++  " " ++ show z ++ "\n") ++ analiseintdat r 


analiseajdat :: [(String,[Int])] -> String
analiseajdat [] = " "
analiseajdat ((c,(h:t:z)):r) =  ("\"" ++ c ++ "\" " ++ show h ++ " " ++ show t ++ "\n") ++ analiseajdat r 

multable1 :: [(String,[Int])] -> String
multable1 ((_,(n:n2:n3:r)):(_,(s:s2:s3:r2)):(_,(m:m2:m3:r3)):(_,(l:l2:l3:r4)):(_,(x:x2:x3:r5)):(_,(z:z2:z3:r6)):rn) = "\\begin{center}\n\\begin{tabular}[b]{|l||c|c|c|c|c|c|r|}\n\\cline{1-2}\n\\verb+Contagem+ & \\verb+Curso+ \\\\\n\\hline\n\\textbf{Porque se inscreveu nas JOIN} & LEI & LCC & MI & MEI & MERSCOM & MBIO & Totais\\\\\n\\hline\n\\emph{Procura 1\186 Emprego} & "++ show n ++ " & " ++ show s ++ " & "++ show m ++ " & "++ show l ++ " & "++ show x ++ " & "++ show z ++ " & "++ show (n+s+m+l+x+z) ++ " \\\\\n\\hline\n\\emph{Procura projecto/disserta\231\227o de mestrado} & "++ show n2 ++" & "++ show s2 ++" & "++ show m2 ++" & "++ show l2 ++ " & "++ show x2 ++" & "++ show z2 ++ " & " ++ show (n2+s2+m2+l2+x2+z2) ++" \\\\\n\\hline\n\\emph{Interesse pelas \225reas tem\225ticas} & " ++ show n3 ++ " & " ++ show s3 ++ " & " ++ show m3 ++ " & " ++ show l3 ++ " & "++ show x3 ++" & "++ show z3 ++ " & " ++ show (n3+s3+m3+l3+x3+z3) ++" \\\\\n\\hline\n\\emph{Total} & " ++ show (n+n2+n3) ++ " & " ++ show (s+s2+s3) ++ " & "++ show (m+m2+m3) ++ " & " ++ show (l+l2+l3) ++ " & " ++ show (x+x2+x3) ++ " & " ++ show(z+z2+z3) ++ " & " ++ show (n+n2+n3+s+s2+s3+m+m2+m3+l+l2+l3+x+x2+x3+z+z2+z3) ++ " \\\\\n\\hline\n\\end{tabular}\n\\end{center}"

multable2 :: [(String,[Int])] -> String
multable2 ((_,(n:n2:nr)):(_,(s:s2:r2)):(_,(m:m2:r3)):(_,(l:l2:r4)):(_,(x:x2:r5)):(_,(z:z2:r6)):rn)  = "\\begin{center}\n\\begin{tabular}[b]{|l||c|c|c|c|c|c|r|}\n\\cline{1-2}\n\\verb+Contagem+ & \\verb+Curso+ \\\\\n\\hline\n\\textbf{Almo\231o / Jantar} & LEI & LCC & MI & MEI & MERSCOM & MBIO & Totais\\\\\n\\hline\n\\emph{Almo\231o} & "++ show n ++ " & "++ show s ++" & "++ show m ++" & "++ show l ++" & "++ show x ++" & "++ show z ++" & "++ show (n+s+m+l+x+z) ++" \\\\\n\\hline\n\\emph{Jantar} & "++ show n2 ++" & "++ show s2 ++" & "++ show m2 ++" & "++ show l2 ++" & "++ show x2 ++" & "++ show z2 ++" & "++ show (n2+s2+m2+l2+x2+z2) ++ " \\\\\n\\hline\n\\emph{Total} & "++ show (n+n2) ++" & "++ show (s+s2) ++" & "++ show (m+m2) ++" & "++ show (l+l2) ++" & "++ show (x+x2) ++" & "++ show (z+z2) ++" & "++ show (n+n2+s+s2+m+m2+l+l2+x+x2+z+z2) ++" \\\\\n\\hline\n\\end{tabular}\n\\end{center}"


-- JUNTAR OS GRAFICOS E TABELAS

juntatex :: String -> String -> String -> String -> String -> String
juntatex n s m l x = "\\documentclass{article}\n\\usepackage{a4wide}\n\\usepackage[portuges]{babel}\n\\usepackage[utf8]{inputenc}\n\\usepackage{verbatim}\n\\usepackage{graphicx}\n\\usepackage{eso-pic}\n\n\\title{Estat\237sticas dos Inscritos}\n\n\\begin{document}\n\\maketitle\n\\abstract{Neste documento encontram-se as estat\237sticas dos alunos inscritos da Univerdade do Minho, de acordo com os Interesses, Curso e Almo\231os ou Jantares}\n\\section{An\225lise Unidimensional}\n\\subsection{N\250mero de Inscritos por Curso (UM)}\n " ++ n ++ " \n\\subsection{Raz\227o pela Inscri\231\227o}\n" ++ s ++ " \n\\subsection{Inscri\231\245es em almo\231os / jantares}\n"++ m ++ "\n\\pagebreak\n\\section{An\225lise Multidimensional}\n\\subsection{Raz\245es da Inscri\231\227o nas JOIN}\n\n " ++ l ++ " \n\n\n\\subsection{Inscri\231\245es}\n" ++ x ++ " \n\n\n\\end{document}\n"


