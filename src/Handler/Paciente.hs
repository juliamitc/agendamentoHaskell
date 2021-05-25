{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Paciente where

import Import
import Text.Cassius

getMenuPacR :: Handler Html
getMenuPacR = do
    defaultLayout $ do 
        -- estatico
        addStylesheet (StaticR css_bootstrap_css)
        setTitle "Agendamento de Consulta" 
        toWidgetHead $(cassiusFile "templates/cadmedico.cassius")
        $(whamletFile "templates/menupacientes.hamlet")

formPaciente :: Form Paciente
formPaciente = renderDivs $ Paciente
    <$> areq textField "Nome: " Nothing
    <*> areq textField "CPF: " Nothing
    <*> areq intField "Idade: " Nothing
    <*> areq textField "Plano: " Nothing

getPacienteR :: Handler Html
getPacienteR = do
    (widget,_) <- generateFormPost formPaciente
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            
            <form method=post action=@{PacienteR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
                <input type="button" value="Voltar" onclick="history.back()">
        |]

postPacienteR :: Handler Html
postPacienteR = do
    ((result,_),_) <- runFormPost formPaciente
    case result of
        FormSuccess paciente -> do
            runDB $ insert paciente
            setMessage [shamlet| 
                <div>
                    PACIENTE INSERIDO COM SUCESSO!
            |]
            redirect PacienteR
        _ -> redirect HomeR
    
getPacientePerfilR :: PacienteId -> Handler Html
getPacientePerfilR pid = do
    paciente <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Perfil de #{pacienteNome paciente}
        <h2>
            Cpf: #{pacienteCpf paciente}
        <h2>
            Idade: #{pacienteIdade paciente}
        <h2>
            Plano: #{pacientePlano paciente}
    |]

getListaPacientesR :: Handler Html
getListaPacientesR = do
    pacientes <- runDB $ selectList [] [Asc PacienteNome]
    defaultLayout $(whamletFile "templates/pacientes.hamlet")

-- delete from paciente where id = pid
postApagarPacR :: PacienteId -> Handler Html
postApagarPacR pid = do
    runDB $ delete pid
    redirect ListaPacientesR   