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
    defaultLayout (formWidget widget msg UsuarioR "Criar")

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
