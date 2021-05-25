{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Medico where

import Import
import Text.Cassius

getMenuMedR :: Handler Html
getMenuMedR = do
    defaultLayout $ do 
        -- estatico
        addStylesheet (StaticR css_bootstrap_css)
        setTitle "Agendamento de Consulta" 
        toWidgetHead $(cassiusFile "templates/cadmedico.cassius")
        $(whamletFile "templates/cadmedico.hamlet")

formMedico :: Form Medico
formMedico = renderDivs $ Medico
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Especialidade: " Nothing
    <*> areq textField "CRM: " Nothing

getMedicoR :: Handler Html
getMedicoR = do
    (widget,_) <- generateFormPost formMedico
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            
            <form method=post action=@{MedicoR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
                <input type="button" value="Voltar" onclick="history.back()">
        |]

postMedicoR :: Handler Html
postMedicoR = do
    ((result,_),_) <- runFormPost formMedico
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