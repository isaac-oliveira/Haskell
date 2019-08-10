type Person = String
type Book = String

type Database = [(Person, Book)]

exempleBase :: Database
exempleBase = [("Alice", "Livro 1"), ("Alice", "Livro 2"), ("Anna", "Livro 3")]

numBorrowed :: Database  -> Book -> [Person]
numBorrowed [] book = []
numBorrowed ((p, b):database) book = (if b == book then [p] else [] ) ++ numBorrowed database book

borrowers :: Database  -> Book -> [Person]
borrowers [] book = []
borrowers ((p, b):database) book = (if b == book then [p] else [] ) ++ numBorrowed database book

borrowed :: Database  -> Book -> Bool
borrowed [] book = False
borrowed ((p, b):database) book = b == book  || borrowed database book