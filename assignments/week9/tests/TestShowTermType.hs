{-#LANGUAGE GADTs, ScopedTypeVariables #-}
-- I am sorry, there is just no way to write haskell code without these two

module TestShowTermType where

  import Test.Tasty.QuickCheck

  import LambdaCalcImplementation (mkFreeVar, mkApp, bindToLam, Term)
  import HelpShow (parenthesize)

  -- | Help for @TestShowTerm@
  -- Indicate in each expression
  -- where can we add more spaces
  data Spaces where
    -- indicate how much space to put there
    NumberOfSpace :: Int -> Spaces 
  
  instance Show Spaces where 
    show (NumberOfSpace n) = replicate n ' '

  data VarName where
    MkVar :: String -> VarName

  instance Show VarName where 
    show (MkVar var) = var


  -- | same lambda term definition as hints/CapteureAvoiding.hs   
  -- this data structure encode all the possible spaces and parenthesis
  -- each show term will exactly correspond to 
  -- a valid lambda expression
  -- used to make a random show lambda term to test parser                               
  data TestShowTerm where

    -- the variable
    Var :: VarName  -- ^ the variable name
        -> TestShowTerm

    -- the application
    App :: TestShowTerm  -- ^ the frist term
          -> Spaces  -- ^ extra spaces between two terms
          -> TestShowTerm -- ^ the second term (term being applied)
          -> TestShowTerm

    -- the lambda
    Lam :: Spaces -- ^ the space after the \
          -> VarName -- ^ the input parameter name
          -> Spaces -- ^ extra space in front of the arrow
          -> Spaces -- ^ the extra space after the arrow
          -> TestShowTerm -- ^ the body of the term
          -> TestShowTerm
    
    -- a parenthesized expression
    -- parenthesize a term is also a term
    Parenthesized :: TestShowTerm
                  -> TestShowTerm
    
    -- put space around a expression
    -- will create a a valid expression
    SpaceAround :: Spaces  -- ^ Space in front of a term
                -> TestShowTerm  -- ^ the Actual Expression
                -> Spaces  -- ^ space after the expresion
                -> TestShowTerm
  

  -- | this is a helper for @genSizedVarName@
  -- This is a list of valid char
  -- that can be the non-start charactor of a variable name
  validNonStartVarChar :: [Char]
  validNonStartVarChar = '\'' :  -- prime is a valid charactor
                        ['a' .. 'z'] ++  -- all the letters are valid
                        ['0' .. '9']  -- all the digits are valid

  -- | generate a random sized variable name
  -- varaible name will always be smaller than the size
  genSizedVarName :: Int  -- ^ the size, can be smaller than 1
                  -> Gen VarName 
  genSizedVarName size | size < 1 =
    do 
      -- get a one single charactor name
      start <- elements ['a' .. 'z']
      return $ MkVar [start]  
  genSizedVarName size | otherwise = 
    do 
      start <- elements ['a' .. 'z']
      lengthRest <- choose (0, size - 1)
      rest <- vectorOf lengthRest $ elements validNonStartVarChar
      return $ MkVar $ start : rest

  -- | generate sized spaces
  -- helper to generate a showTerm
  genSizedSpaces :: Int -> Gen Spaces 
  genSizedSpaces size = 
    do 
      length <- choose (0, size)
      return $ NumberOfSpace length
  
  -- | gen with random size 
  -- helper function
  genWithRandSize :: Gen Int -> (Int -> Gen a) -> Gen a 
  genWithRandSize genSize genSizedObj =
    do 
      size <- genSize 
      genSizedObj size

  -- | generate a sized var
  genSizedVar :: Int -> Gen TestShowTerm
  genSizedVar size = 
    do 
      varName <- genSizedVarName size
      return $ Var varName
  
  -- | generate a sized application
  genSizedApp :: Int -> Gen TestShowTerm
  genSizedApp size =
    do 
      let sizeGen = choose (0, size `div` 3)
      -- generate a sized right term
      rTerm <- genWithRandSize sizeGen genSizedShowTerm  
      -- generate some spaces
      spaces <- genWithRandSize sizeGen genSizedSpaces
      -- generate a sized left term
      lTerm <- genWithRandSize sizeGen genSizedShowTerm 
      -- return
      return $ App rTerm spaces lTerm
  
  -- | generate a sized Lambda
  genSizedLam :: Int -> Gen TestShowTerm
  genSizedLam size =
    do 
      let sizeGen = choose (0, size `div` 5)
      -- generate space after \
      spaceAfterSlash <- genWithRandSize sizeGen genSizedSpaces
      -- generate a sized variable name
      paraName <- genWithRandSize sizeGen genSizedVarName
      -- generate space before ->
      spaceBeforeArrow <- genWithRandSize sizeGen genSizedSpaces 
      -- generate space after -> 
      spaceAfterArrow <- genWithRandSize sizeGen genSizedSpaces
      -- generate the the body 
      body <- genWithRandSize sizeGen genSizedShowTerm
      return $ Lam spaceAfterSlash paraName spaceBeforeArrow spaceAfterArrow body

  -- | generate a sized parenthesized expression
  genSizedParened :: Int -> Gen TestShowTerm
  genSizedParened size = 
    do 
      let sizeGen = choose (0, size - 2)
      -- generate the term inside
      innerTerm <- genWithRandSize sizeGen genSizedShowTerm
      return $ Parenthesized innerTerm
  
  -- | generate a sized SpaceAround expression
  genSpaceAround :: Int -> Gen TestShowTerm
  genSpaceAround size =
    do 
      let sizeGen = choose (0, size `div` 3)
      -- generate the space before 
      spaceBefore <- genWithRandSize sizeGen genSizedSpaces
      -- generate the inner expression
      innerTerm <- genWithRandSize sizeGen genSizedShowTerm
      -- generate the space after
      spaceAfter <- genWithRandSize sizeGen genSizedSpaces
      return $ SpaceAround spaceBefore innerTerm spaceAfter
      
  -- | generate a random test show Lambda term
  genSizedShowTerm ::  Int -> Gen TestShowTerm
  genSizedShowTerm size 
    | size < 1 = genSizedVar size
    | otherwise = 
      oneof [
        genSizedVar size,
        genSizedApp size,
        genSizedLam size,
        genSizedParened size,
        genSpaceAround size
        ]

  -- | shrink the VarName
  shrinkVarName :: VarName -> [VarName]
  shrinkVarName (MkVar []) = []
  shrinkVarName (MkVar [start]) = []
  shrinkVarName (MkVar (start:res)) = 
    [MkVar (start: res') | fstN <- [0 .. length res - 1], let res' = take fstN res]

  -- | shrink the space
  -- helper for @shrinkShowTerm@
  shrinkSpace :: Spaces -> [Spaces]
  shrinkSpace (NumberOfSpace n) = 
    [NumberOfSpace n' | n' <- [0 .. n - 1]] 

  -- | shrink a show term
  -- generate a list of show term that is smaller 
  -- than the input
  shrinkShowTerm :: TestShowTerm -> [TestShowTerm]
  shrinkShowTerm (Var varName) = 
    [Var name' | name' <- shrinkVarName varName] 
  shrinkShowTerm (App rTerm spaces lTerm) =
    (rTerm : shrinkShowTerm rTerm) ++
    (lTerm : shrinkShowTerm lTerm) ++
    [App rTerm' spaces lTerm | rTerm' <- shrinkShowTerm rTerm] ++
    [App rTerm spaces' lTerm | spaces' <- shrinkSpace spaces] ++
    [App rTerm spaces lTerm' | lTerm' <- shrinkShowTerm lTerm]
  shrinkShowTerm (Lam fstS varName sndS thrS body) =
    (body : shrinkShowTerm body) ++
    [Lam fstS' varName sndS thrS body | fstS' <- shrinkSpace fstS] ++
    [Lam fstS varName' sndS thrS body | varName' <- shrinkVarName varName] ++
    [Lam fstS varName sndS' thrS body | sndS' <- shrinkSpace sndS] ++
    [Lam fstS varName sndS thrS' body | thrS' <- shrinkSpace thrS] ++
    [Lam fstS varName sndS thrS body' | body' <- shrinkShowTerm body]
  shrinkShowTerm (Parenthesized term) =
    (term: shrinkShowTerm term) ++ 
    [Parenthesized term' | term' <- shrinkShowTerm term]
  shrinkShowTerm (SpaceAround fstS term sndS) = 
    (term: shrinkShowTerm term) ++ 
    [SpaceAround fstS' term sndS | fstS' <- shrinkSpace fstS] ++
    [SpaceAround fstS term' sndS | term' <- shrinkShowTerm term] ++
    [SpaceAround fstS term sndS' | sndS' <- shrinkSpace sndS]

  instance Arbitrary TestShowTerm where
    arbitrary = sized genSizedShowTerm
    shrink = shrinkShowTerm

  -- | shows the term with minimal parenthesis
  showPretty :: TestShowTerm -- ^ the term to show
                -> Integer   -- ^ the outter precedence level between 0 and 100. 
                             -- the lower the number, the tighter the bind
                -> String
  showPretty (Var name) _ = show name  -- never parenthesize a varaible

  showPretty (App lTerm spaceBetween rTerm) outter =
    parenthesize outter 1 (
      (showPretty lTerm 1) ++ 
      (show spaceBetween) ++ " " ++ 
      (showPretty rTerm 0)
      ) 

  showPretty (Lam sAftSlash varName sBefArrow sAftArrow body) outter =
    parenthesize outter 2 (
      "\\" ++ (show sAftSlash) ++
      (show varName) ++
      (show sBefArrow) ++ "->" ++ (show sAftArrow) ++
      (showPretty body 2)
      ) 

  showPretty (Parenthesized term) _ =
    "(" ++ (showPretty term 100) ++ ")"

  showPretty (SpaceAround sBefore term sAfter) outter = 
    (show sBefore) ++ (showPretty term outter) ++ (show sAfter)
     
  instance Show TestShowTerm where
    show t = "\"" ++ (showPretty t 100) ++ "\""

  -- | converting the show term to homework term
  showTermToHw :: TestShowTerm -> Term
  showTermToHw (Var name) = mkFreeVar (show name)
  showTermToHw (App lTerm s rTerm) = 
    mkApp (showTermToHw lTerm) (showTermToHw rTerm)
  showTermToHw (Lam s1 paraName s2 s3 body) =
    bindToLam (show paraName) (showTermToHw body)
  showTermToHw (Parenthesized term) = showTermToHw term 
  showTermToHw (SpaceAround s1 term s2) = showTermToHw term 