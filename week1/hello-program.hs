main = do
	-- Prints string with new line
	putStrLn "Enter your name: "

	-- Gets user input and stores it in name
	-- <- gets name entered from an IO action
	name <- getLine

	putStrLn("Hello " ++ name)