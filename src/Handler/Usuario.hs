{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Cassius
import Handler.Auxiliar

formLogin :: Form (Usuario, Text)
formLogin = renderDivs $ (,) 
    <$> (Usuario
        <$> areq textField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Confirmação: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        setTitle "Criar Usuário"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/form.cassius")
        (formWidget widget msg UsuarioR "Cadastrar")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (usuario@(Usuario email senha), conf) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of 
                Just _ -> do
                    setMessage [shamlet|
                        <div>
                            E-MAIL JÁ CADASTRADO!
                    |]
                    redirect UsuarioR
                Nothing -> do
                    if senha == conf then do
                        runDB $ insert usuario
                        setMessage [shamlet| 
                            <div>
                                USUARIO CRIADO COM SUCESSO!
                        |]
                        redirect UsuarioR
                    else do
                        setMessage [shamlet| 
                            <div>
                                SENHAS NÃO CONFEREM!
                        |]
                        redirect UsuarioR
        _ -> redirect HomeR

getListaUsuariosR :: Handler Html
getListaUsuariosR = do
    usuarios <- runDB $ selectList [] [Asc UsuarioEmail]
    defaultLayout $ do
        setTitle "Lista de Usuários"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/listar.cassius")
        $(whamletFile "templates/usuarios.hamlet")

postApagarUsuR :: UsuarioId -> Handler Html
postApagarUsuR uid = do
    runDB $ delete uid
    redirect ListaUsuariosR