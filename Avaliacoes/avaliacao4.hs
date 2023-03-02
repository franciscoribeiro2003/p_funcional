import Log

--Exercicio 1
parseMessage :: String -> LogEntry
parseMessage n | words n !! 0 == "E" = LogMessage (Error (read (words n !! 1)::Int)) (read (words n !! 2)::Int) (unwords (drop 3 (words n)))
               | words n !! 0 == "W" = LogMessage (Warning) (read (words n !! 1)::Int) (unwords (drop 2 (words n)))
               | words n !! 0 == "I" = LogMessage (Info) (read (words n !! 1)::Int) (unwords (drop 2 (words n)))
               | otherwise           = Unknown n

--Exercicio 2-----------------------------------------------------------------------
insert :: LogEntry -> MessageTree -> MessageTree

insert mensagem@LogMessage{} Empty = Node mensagem Empty Empty
insert mensagem1@(LogMessage _ ts1 _) (Node mensagem2@(LogMessage _ ts2 _) left right)
  | ts1 >= ts2 = Node mensagem2 left (insert mensagem1 right)
  | otherwise = Node mensagem2 (insert mensagem1 left) right
insert _ tree = tree

-- Exercicio 3----------------------------------------------------------------------

build :: [LogEntry] -> MessageTree
build = foldr insert Empty

inOrder :: MessageTree -> [LogEntry]
inOrder Empty = []
inOrder (Node mensagem left right) = inOrder left ++ [mensagem] ++ inOrder right

--No codex incluir o exercicio 2 para funcionar

--Extra----------------------------------------------------------------------------

sortMessages :: [LogEntry] -> [LogEntry]
sortMessages msgs = inOrder (build msgs)



--Francisco Ribeiro 2022 up202104797
