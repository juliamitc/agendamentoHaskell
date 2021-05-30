{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Text.Cassius

formLogin :: Form Usuario
formLogin = renderDivs $ Usuario
        <$> areq textField "E-mail: " Nothing
        <*> areq passwordField "Senha: " Nothing

getAutR :: Handler Html
getAutR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        setTitle "Login"
        addStylesheet (StaticR css_bootstrap_css)
        toWidgetHead $(cassiusFile "templates/formlogin.cassius")
        (formWidget widget msg AutR "Entrar")

postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (Usuario email senha) -> do 
           usuarioExiste <- runDB $ getBy (UniqueEmail email)
           case usuarioExiste of 
                Nothing -> do 
                    setMessage [shamlet|
                        <div>
                            Usuario não encontrado!
                    |]
                    redirect AutR
                Just (Entity _ usuario) -> do 
                    if senha == usuarioSenha usuario then do
                        setSession "_ID" (usuarioEmail usuario)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            <div>
                                Senha INCORRETA!
                        |]
                        redirect AutR 
        _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR

formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg rota m = $(whamletFile "templates/formlogin.hamlet")   