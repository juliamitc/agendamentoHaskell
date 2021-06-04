{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Agendamento where

import Import
import Text.Lucius
import Text.Julius
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql

medicoCB = do
  rows <- runDB $ selectList [] [Asc MedicoNome]
  optionsPairs $ 
      map (\r -> (medicoNome $ entityVal r, entityKey r)) rows

pacienteCB = do
  rows <- runDB $ selectList [] [Asc PacienteNome]
  optionsPairs $ 
      map (\r -> (pacienteNome $ entityVal r, entityKey r)) rows

-- renderDivs
formAgendamento :: Form Agendamento 
formAgendamento = renderDivs $ Agendamento
    <$> areq textField "Tipo: " Nothing
    <*> areq (selectField medicoCB) "Medico: " Nothing
    <*> areq (selectField pacienteCB) "Paciente: " Nothing
    <*> areq dayField "Dia: " Nothing
    <*> areq textField "Hora: " Nothing

getAgendamentoR :: Handler Html
getAgendamentoR = do 
    (widget,_) <- generateFormPost formAgendamento
    msg <- getMessage
    defaultLayout $ 
        [whamlet|
            $maybe mensa <- msg 
                <div>
                    ^{mensa}
            
            <h1>
                CADASTRO DE AgendamentoCOES
            
            <form method=post action=@{AgendamentoR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postAgendamentoR :: Handler Html
postAgendamentoR = do 
    ((result,_),_) <- runFormPost formAgendamento
    case result of 
        FormSuccess agendamento -> do 
            runDB $ insert agendamento 
            setMessage [shamlet|
                <div>
                    agendamentoCAO INCLUIDA
            |]
            redirect AgendamentoR
        _ -> redirect HomeR