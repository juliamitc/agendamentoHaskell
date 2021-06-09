{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Agendamento where

import Import
import Text.Cassius
import Text.Julius
import Handler.Auxiliar
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
    defaultLayout $ do
        setTitle "Cadastro de Agendamentos" 
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/form.cassius")
        (formWidget widget msg AgendamentoR "Cadastrar")

postAgendamentoR :: Handler Html
postAgendamentoR = do 
    ((result,_),_) <- runFormPost formAgendamento
    case result of 
        FormSuccess agendamento -> do 
            runDB $ insert agendamento 
            setMessage [shamlet|
                <div>
                    Agendamento Incluído!
            |]
            redirect AgendamentoR
        _ -> redirect HomeR


getConsultaR :: MedicoId -> Handler Html
getConsultaR medicoid = do 
    let sql = "SELECT ??,??,?? FROM agendamento \
          \ INNER JOIN medico ON medico.id = agendamento.medicoid \
          \ INNER JOIN paciente ON paciente.id = agendamento.pacienteid \
          \ WHERE medico.id = ?"
    medico <- runDB $ get404 medicoid
    agendamentos <- runDB $ rawSql sql [toPersistValue medicoid] :: Handler [(Entity Agendamento,Entity Paciente,Entity Agendamento)]
    defaultLayout $ do 
        [whamlet|
            <h1>
                Agenda de #{medicoNome medico}
            <ul>
                $forall (Entity _ agendamento, Entity _ paciente, Entity _ _) <- agendamentos
                    <li>
                        #{agendamentoTipo agendamento}
                        #{agendamentoHora agendamento}
                        #{show $ agendamentoDia agendamento}
                        #{pacienteNome paciente}
        |]
