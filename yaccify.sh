
mono fsyacc.exe Parser.fsy
mono fslex.exe Lexer.fsl

sed -i '2 i\module Parser' Parser.fs