{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Text.Blaze.Svg11 ((!), mkPath, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A


-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidget $ svgDoc


svgDoc :: S.Svg
svgDoc = -- S.docTypeSvg 
       S.svg ! A.version "1.1" 
       ! A.width "150" ! A.height "100" 
       ! A.stroke "black" ! A.strokeWidth "5"
       $ do
   S.circle ! A.cx "50" ! A.cy "50" ! A.r "20"
   S.path ! A.d makeSimplePath

makeSimplePath :: S.AttributeValue
makeSimplePath =  mkPath $ do
   l 0 0
   m 20 20 
   l 30 30 
   m 10 90


postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
