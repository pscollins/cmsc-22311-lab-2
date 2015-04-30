import Suck

import Data.Map
import Test.HUnit


bodyWords = words . extractBody
htmlToPrimModel = toPrimModel . extractBody
htmlToFreqModel = toFreqModel . htmlToPrimModel
htmlToProcessedModel = toProcessedModel . htmlToFreqModel

main = runTestTT $ TestList [
        testBody1 ~=? bodyWords testString1
       , testBody2 ~=? bodyWords testString2
       , testBody3 ~=? bodyWords testString3
       , testModel1 ~=? htmlToPrimModel testString1
       , testModel2 ~=? htmlToPrimModel testString2
       , testFreqModel1 ~=? htmlToFreqModel testString1
       , testFreqModel2 ~=? htmlToFreqModel testString2
       , testProcessedModel2 ~=? htmlToProcessedModel testString2
       , testModel1Double ~=? mergePrimModels [testModel1, testModel1]
       , testBody3 ~=? bodyWords testString4]


testString1 = unlines [
               "<div id=\"body\">"
             , "<p>Hello World Today</p>"
             , "</div>"]

testBody1 = ["Hello", "World", "Today"]
testModel1 = fromList [(("Hello", "World"), ["Today"])]
testModel1Double = fromList [(("Hello", "World"), ["Today", "Today"])]
testFreqModel1 = fromList [(("Hello", "World"), [(1, "Today")])]


testString2 = unlines [
               "<div id=\"body\">"
             , "<p>Hello World Today I</p>"
             , "</div>"]
testBody2 = ["Hello", "World", "Today", "I"]
testModel2 = fromList [
              (("Hello", "World"), ["Today"])
             , (("World", "Today"), ["I"])]
testFreqModel2 = fromList [
                  (("Hello", "World"), [(1, "Today")])
                 , (("World", "Today"), [(1, "I")])]
testProcessedModel2 = [("World", [(1, 1)]), ("Today", [])]


testString3 = unlines [
              "<div class=\"contrib\">"
             , "<a href=\"/\">Author</a>"
             , "<div class=\"abstract\">"
             , "<strong>Abstract</strong><p> if not the unconscious. </p>"
             , "</div> "
             , "<div id=\"body\">"
             , "<blockquote class=\"disp-quote\">"
             , "<p>Human soul, let us see.</p>"
             , "</div>"]

testBody3 = words "Human soul, let us see."

testString4 = unlines [
              "<div class=\"contrib\">"
             , "<a href=\"/\">Author</a>"
             , "<div class=\"abstract\">"
             , "<strong>Abstract</strong><p> if not the unconscious. </p>"
             , "</div> "
             , "<div>"
             , "<div id=\"body\">"
             , "<blockquote class=\"disp-quote\">"
             , "<p>Human soul, let us see.</p>"
             , "</div>"
             , "</div>"]
