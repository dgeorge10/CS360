sentence([Article1,Noun1,Verb,Article2,Noun2]) :- article(Article1), noun(Noun1), verb(Verb), article(Article2), noun(Noun2).

article(a).
article(the).

noun(boy).
noun(girl).
noun(dog).
noun(cat).

verb(sees).
verb(pets).
