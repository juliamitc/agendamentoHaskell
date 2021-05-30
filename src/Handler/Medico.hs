{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Medico where

import Import
import Text.Cassius
import Handler.Auxiliar

getMenuMedR :: Handler Html
getMenuMedR = do
    defaultLayout $ do 
        -- estatico
        addStylesheet (StaticR css_bootstrap_css)
        setTitle "Agendamento de Consulta" 
        toWidgetHead $(cassiusFile "templates/cadmedico.cassius")
        $(whamletFile "templates/menumed.hamlet")

formMedico :: Maybe Medico -> Form Medico
formMedico mc = renderDivs $ Medico
    <$> areq textField "Nome: " (fmap medicoNome mc)
    <*> areq textField "Especialidade: " (fmap medicoEspecialidade mc)
    <*> areq textField "CRM: " (fmap medicoCrm mc)

getMedicoR :: Handler Html
getMedicoR = do
    (widget,_) <- generateFormPost (formMedico Nothing)
    msg <- getMessage
    defaultLayout (formWidget widget msg MedicoR "Cadastrar")

postMedicoR :: Handler Html
postMedicoR = do
    ((result,_),_) <- runFormPost (formMedico Nothing)
    case result of
        FormSuccess medico -> do
            runDB $ insert medico
            setMessage [shamlet| 
                <div>
                    MEDICO INSERIDO COM SUCESSO!
            |]
            redirect MedicoR
        _ -> redirect HomeR
    
-- /medico/perfil/#MedicoId MedicoPerfilR GET
-- /medicos ListaMedicosR GET
-- /medico/#MedicoId/apagar ApagarMedR POST

-- faz um select * from medico where id = mid 
-- Se falhar mostra uma pagina de erro.
-- /medico/perfil/1 => mid = 1
getMedicoPerfilR :: MedicoId -> Handler Html
getMedicoPerfilR mid = do
    medico <- runDB $ get404 mid
    defaultLayout [whamlet|
        <h1>
            Perfil de #{medicoNome medico}
        <h2>
            Especialidade: #{medicoEspecialidade medico}
        <h2>
            CRM: #{medicoCrm medico}
    |]

getListaMedicosR :: Handler Html
getListaMedicosR = do
    medicos <- runDB $ selectList [] [Asc MedicoNome]
    defaultLayout $(whamletFile "templates/medicos.hamlet")

-- delete from medico where id = mid
postApagarMedR :: MedicoId -> Handler Html
postApagarMedR mid = do
    runDB $ delete mid
    redirect ListaMedicosR

getEditarMedR :: MedicoId -> Handler Html
getEditarMedR mid = do
    medico <- runDB $ get404 mid
    (widget,_) <- generateFormPost (formMedico (Just medico))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarMedR mid) "Editar")

postEditarMedR :: MedicoId -> Handler Html
postEditarMedR mid = do
    medicoAntigo <- runDB $ get404 mid
    ((result,_),_) <- runFormPost (formMedico Nothing)
    case result of
        FormSuccess novoMedico -> do
            runDB $ replace mid novoMedico
            redirect ListaMedicosR
        _ -> redirect HomeR