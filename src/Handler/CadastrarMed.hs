{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.CadastrarMed where

import Import
import Text.Cassius
import Text.Julius

getCadMedicosR :: Handler Html
getCadMedicosR = do
    defaultLayout $ do 
        -- estatico
        addStylesheet (StaticR css_bootstrap_css)
        setTitle "Cadastrar Doutores" 
        toWidgetHead $(cassiusFile "templates/cadmedico.cassius")
        $(whamletFile "templates/cadmedico.hamlet")