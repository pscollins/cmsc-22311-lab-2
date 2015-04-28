import Suck

import Data.Map

import Test.HUnit



main = runTestTT $ TestList [
         testModel1 ~=? htmlToPrimModel testString1
       , testModel2 ~=? htmlToPrimModel testString2]

testString1 = unlines [
               "<div id=\"body\">"
             , "<p>Hello World Today</p>"
             , "</div>"]

testModel1 = fromList [(("Hello", "World"), ["Today"])]

testString2 = unlines [
               "<div id=\"body\">"
             , "<p>Hello World Today I</p>"
             , "</div>"]

testModel2 = fromList [
              (("Hello", "World"), ["Today"])
             , (("World", "Today"), ["I"])]


testString = unlines [
              "<div class=\"contrib\">"
             , "<a href=\"/\">Author</a>"
             , "<div class=\"abstract\">"
             , "<strong>Abstract</strong><p> if not the unconscious. </p>"
             , "</div> "
             , "<div id=\"body\">"
             , "<blockquote class=\"disp-quote\">"
             , "<p>Human soul, let us see whether present time can be long.</p>"
             , "</div>"]
