module LangTest where


    import TestBase (L0Ast, L1Ast, L2Ast, L3Ast, L4Ast)

    import Test.Tasty (testGroup, TestTree)
    import Test.Tasty.HUnit (assertEqual, testCase, assertBool, (@=?))
    import Test.Tasty.QuickCheck as QC
    import GHC.Generics (Generic)
    import Data.Typeable (Typeable)
    import Test.QuickCheck.Arbitrary.Generic

    import qualified Lang0Parser as L0Parser (parser)
    import qualified Lang1Parser as L1Parser (parser)
    import qualified Lang2Parser as L2Parser (parser)
    import qualified Lang3Parser as L3Parser (parser)
    import qualified Lang4Parser as L4Parser (parser)


    ---- All Lang Test ----
    langTest = testGroup "Lang Parser test" [
          QC.testProperty "show should match parse" $ ((\ x -> Just (x , "") == (L0Parser.parser $ show x)) :: L0Ast -> Bool),
          QC.testProperty "show should match parse" $ ((\ x -> Just (x , "") == (L1Parser.parser $ show x)) :: L1Ast -> Bool),
          QC.testProperty "show should match parse" $ ((\ x -> Just (x , "") == (L2Parser.parser $ show x)) :: L2Ast -> Bool),
          QC.testProperty "show should match parse" $ ((\ x -> Just (x , "") == (L3Parser.parser $ show x)) :: L3Ast -> Bool),
          QC.testProperty "show should match parse" $ ((\ x -> Just (x , "") == (L4Parser.parser $ show x)) :: L4Ast -> Bool)
          ]
