# First-order-logic-resolution
This repository contains codes that can make first order logic resolution.

Basically knowledge base can be entered in a .txt, for example, formal expressions of 'If someone is both a good man and a Texas A&M student, the person should be honest', ' If someone is honest, professors like him.' and 'John is both a good man and a TAMU student'. 

Once 'folresolution.py' is executed, command line will ask you to ask a question about the knowledge base, for example, 'do professors like John?'. Then the program will answer yes through first order resolution.

Content:
  
  1. 'folresolution.py' is the main program that implements first order resolution.
  2. 'SentenceParser.py' and 'AtomicSentenceParser.py' compile the knowledge base and questions to parse tree using the yacc         python package.
