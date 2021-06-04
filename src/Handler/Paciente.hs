{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Paciente where

import Import
import Text.Cassius
import Handler.Auxiliar

getMenuPacR :: Handler Html
getMenuPacR = do
    defaultLayout $ do 
        -- estatico
        addStylesheet (StaticR css_bootstrap_css)
        setTitle "Agendamento de Consulta" 
        toWidgetHead $(cassiusFile "templates/cadmedico.cassius")
        $(whamletFile "templates/menupacientes.hamlet")

formPaciente :: Maybe Paciente -> Form Paciente
formPaciente mc = renderDivs $ Paciente
    <$> areq textField "Nome: " (fmap pacienteNome mc)
    <*> areq textField "CPF: " (fmap pacienteCpf mc)
    <*> areq intField "Idade: " (fmap pacienteIdade mc)
    <*> areq textField "Plano: " (fmap pacientePlano mc)

getPacienteR :: Handler Html
getPacienteR = do
    (widget,_) <- generateFormPost (formPaciente Nothing)
    msg <- getMessage
    defaultLayout $ do
        setTitle "Login"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/home.cassius")
        (formWidget widget msg PacienteR "Cadastrar")

postPacienteR :: Handler Html
postPacienteR = do
    ((result,_),_) <- runFormPost (formPaciente Nothing)
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

getEditarPacR :: PacienteId -> Handler Html
getEditarPacR pid = do
    paciente <- runDB $ get404 pid
    (widget, _) <- generateFormPost (formPaciente (Just paciente))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarPacR pid) "Editar")

postEditarPacR :: PacienteId -> Handler Html
postEditarPacR pid = do
    pacienteAntigo <- runDB $ get404 pid
    ((result,_),_) <- runFormPost (formPaciente Nothing)
    case result of
        FormSuccess novoPaciente -> do
            runDB $ replace pid novoPaciente
            redirect ListaPacientesR
        _ -> redirect HomeR
  