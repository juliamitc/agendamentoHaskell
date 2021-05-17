{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Agendamento where

import Import
import Text.Cassius
import Text.Julius

getAgendamentoR :: Handler Html
getAgendamentoR = do
    defaultLayout $ do 
        -- estatico
        addStylesheet (StaticR css_bootstrap_css)
        setTitle "Agendamento de Consulta" 
        toWidgetHead $(cassiusFile "templates/agendamento.cassius")
        $(whamletFile "templates/agendamento.hamlet")