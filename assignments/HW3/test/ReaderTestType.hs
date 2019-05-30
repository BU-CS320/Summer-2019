module ReaderTestType(Reader(..), ShowableReader, showableToReader) where

  import Test.Tasty.QuickCheck
  import Reader (Reader(..))

  data ShowableReader e a = MkShowReader (Fun e a) deriving Show

  showableToReader :: ShowableReader e a -> Reader e a
  showableToReader (MkShowReader (Fun _ f)) = Reader f

  instance (CoArbitrary s, Function s, Arbitrary a, Arbitrary s) => Arbitrary (ShowableReader s a) where
    arbitrary = do fun <- arbitrary; return $ MkShowReader fun 
    shrink (MkShowReader fun) = [MkShowReader fun' | fun' <- shrink fun]
